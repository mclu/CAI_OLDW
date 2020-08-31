# CAI - OLDW
# The script cleans the data, implements prophet model, and uses random search to
# perform parameter tuning.
#
# Author: Amy (Ming-Chen) Lu
# Updated: July 25, 2020
#80: ---------------------------------------------------------------------------
# Libraries: -------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(data.table)
library(prophet)

# Read in files: ---------------------------------------------------------------
setwd("/Users/Amy/Desktop/OLDW")
class_ = fread("./raw_data/oldw_class_7-2-20.csv", encoding='UTF-8')
class_mbrshp = fread("./raw_data/oldw_class_mbrshp_7-1-20.csv", encoding='UTF-8')
crse = fread("./raw_data/oldw_crse_7-2-20.csv", encoding='UTF-8')

# Remove non-English courses, those with archive/delete, and teach-out courses
crse = crse[!(grepl("[^ -~]|z_|delete|DELETE|Teach-Out|[Ss]andbox|Test Course", 
                    CRSE_NM) | SRC_SYS_CD == "FTRLRN"),
            .(CRSE_KEY, CRSE_NM = str_replace(CRSE_NM, "_", ":"), SRC_SYS_CD,
              SITE = ifelse(SRC_SYS_CD == "EDX", "Edx", "Coursera"))]

# Cumulative enrollments for each course: --------------------------------------
# Join the files by course_key and class_key
dt = class_mbrshp[MBRSHP_ROLE_CD == "LEARNER", 
                  .(USR_KEY, CLASS_KEY, 
                    MBRSHP_BEGIN_DTTM = as.Date(MBRSHP_BEGIN_DTTM, "%Y-%m-%d"))] %>%
  merge(., class_[, .(CLASS_KEY, CRSE_KEY)], 
        by = "CLASS_KEY", all.x=TRUE, all.y=FALSE) %>%
  merge(., crse, by = "CRSE_KEY", all.x=TRUE, all.y=FALSE) %>%
  .[, CLASS_KEY := NULL]

# Keep only the earliest enrollment for each user
dt_new = unique(dt[order(MBRSHP_BEGIN_DTTM), .(MBRSHP_BEGIN_DTTM, CRSE_NM, SITE), 
          by = c("USR_KEY", "CRSE_KEY")]) %>% 
  .[,`:=` (USR_KEY = NULL)] %>%
  .[!is.na(CRSE_NM)] %>%
  .[, .(CNT = .N), keyby=c("CRSE_NM", "SITE", "MBRSHP_BEGIN_DTTM")] %>%
  .[, y := cumsum(as.numeric(CNT)), by="CRSE_NM"] %>%
  .[, .SD[-1], by=CRSE_NM]

# Divided to Coursera and Edx: -------------------------------------------------
# Remove course with enrollments less than 20 in the past two months
# Also courses without 30 days enrollments
# Coursera
sys_yr = year(Sys.Date())
sys_mo = dt_coursera[order(yr, mo), tail(.SD, 1)]$mo
dt_coursera = dt_new[SITE == "Coursera"][,`:=` (SITE=NULL, ds=MBRSHP_BEGIN_DTTM)]

crse_vec = dt_coursera[, `:=` (yr = year(MBRSHP_BEGIN_DTTM), 
                               mo = month(MBRSHP_BEGIN_DTTM),
                               tot = .N), by="CRSE_NM"] %>%
  .[(yr == sys_yr) & ((mo == sys_mo) | (mo == (sys_mo - 1)))] %>%
  .[, .(N = sum(CNT), tot = mean(tot)), by="CRSE_NM"] %>%
  .[(N > 20) & (tot > 30)]
  
# Manually remove "Master of Applied Data Science Student Orientation"
crse_vec = crse_vec[CRSE_NM != "Master of Applied Data Science Student Orientation"]

dt_coursera = dt_coursera[CRSE_NM %in% crse_vec$CRSE_NM]

# Manually remove first 5 points due to discontinuity in dates
idx = dt_coursera[(CRSE_NM == "Hearing Loss in Children"), which = TRUE][1:5]
dt_coursera = dt_coursera[-idx]

#write.csv(dt_coursera, "dt_coursera.csv", row.names = FALSE)

# Edx
dt_edx = dt_new[SITE == "Edx"][,`:=` (SITE=NULL, ds=MBRSHP_BEGIN_DTTM)]
edx_vec = dt_edx[, `:=` (yr = year(MBRSHP_BEGIN_DTTM), 
                         mo = month(MBRSHP_BEGIN_DTTM),
                         tot = .N), by="CRSE_NM"] %>%
  .[(yr == sys_yr) & ((mo == sys_mo) | (mo == (sys_mo - 1)))] %>%
  .[, .(N = sum(CNT),tot = mean(tot)), by="CRSE_NM"] %>%
  .[(N > 20) & (tot > 30)]
dt_edx = dt_edx[CRSE_NM %in% edx_vec$CRSE_NM]

# remove unneeded files
rm(list = c("class_", "class_mbrshp", "crse"))

# Not needed - seems like model will complete the date itself: -----------------
# Complete the dates
indx = dt_coursera[, .(ds=seq(min(ds), max(ds), "days")), by=CRSE_NM]

# key the tables and join them using a rolling join
setkey(dt_coursera, CRSE_NM, ds)
setkey(indx, CRSE_NM, ds)
df = dt_coursera[,.(CRSE_NM, ds, y)] %>% .[indx, roll=TRUE]

# Run plot for single course: --------------------------------------------------
plot_a_crse = function(dt, course) {
  dt[CRSE_NM == course] %>% 
    ggplot(aes(x = ds, y = y)) +
    geom_line() + ggtitle(course) +
    xlab("") + ylab("Cumulative Enrollments") +
    theme_bw()
}

# Test the func
plot_a_crse(dt_coursera, "Python Basics")

# Prophet Model: ---------------------------------------------------------------
dt_coursera = fread("dt_coursera.csv")
# Check total of data points for each course
dt_crsa_par = dt_coursera[, .(date_diff = max(ds) - min(ds),
                              N = mean(tot)), by=CRSE_NM] %>%
  .[, horizon := date_diff %/% 9]


# Function of fitting and predicting coursera course using prophet forecaster
prophet_predict_crsa = function(course_name, periods, cp, seas){
  mod = prophet(dt_coursera[CRSE_NM == course_name, .(ds, y)], 
                algorithm = "LBFGS",
                changepoint.prior.scale = cp,
                seasonality.prior.scale = seas)
  future = make_future_dataframe(mod, periods = periods, freq = "day")
  forecast = predict(mod, future)
  return(list(mod, forecast))
}

# Run all models for Coursera course: ------------------------------------------
mod_ls = list()
for( i in unique(dt_coursera$CRSE_NM)){
  tryCatch({
    mod_ls[[i]] = prophet_predict_crsa(i, periods = 30, cp=.05, seas=10)
    }, error = function(e){
      cat("ERROR:", i,".", conditionMessage(e), "\n")})
}
save(mod_ls, file = "mod_ls0814.RData")
#load("mod_ls0814.RData")

# CV: --------------------------------------------------------------------------
CV_ls = list()
for( i in names(mod_ls) ){
  tryCatch({
    horizon = dt_crsa_par[CRSE_NM == i]$horizon
    CV_ls[[i]] = cross_validation(mod_ls[[i]][[1]], 
                                  horizon = horizon, units = "days")
  }, error = function(e){
    cat("ERROR:", i, conditionMessage(e),"\n")})
}
save(CV_ls, file = "CV_ls0814.RData")
#load("CV_ls0814.RData")

# Performance metrics
mat_ls = lapply(CV_ls, function(x) performance_metrics(x))
save(mat_ls, file = "mat_ls0814.RData")
#load("mat_ls0814.RData")

# A table showing all results: -------------------------------------------------
# RMSE and MAPE are computed using data started from 2017/1/1
eval_mod = function(x) {
  x[, yr := year(ds)] %>%
    .[yr >= 2017, 
      .(rmse = round(sqrt(mean((y - yhat)^2)), 2),
        mape = round(mean(abs(y - yhat) / y) * 100, 2))] %>%
    return()
}

tbl = as.data.frame(matrix(unlist(lapply(CV_ls, eval_mod)), 
                           ncol = 2, byrow = TRUE))
tbl["course"] = names(CV_ls)
colnames(tbl)[1:2] = c("rmse", "mape")
tbl = tbl %>% select(course, mape, rmse) %>% arrange(mape)

# Refit those failed at the first time
# Dentistry 101
course = "Dentistry 101"
dentistry_eval = eval_mod(cv)

# Managing Talent
course = "Managing Talent"
talent = eval_mod(cv)

# Sampling People, Networks and Records
course = "Sampling People, Networks and Records"
sampling = eval_mod(cv)
# add to tbl
df = data.frame(course = c("Dentistry 101", "Managing Talent",
                           "Sampling People, Networks and Records"),
                mape = c(dentistry_eval[[2]], talent[[2]], sampling[[2]]),
                rmse = c(dentistry_eval[[1]], talent[[1]], sampling[[1]]))

tbl = rbind(tbl, df) %>% arrange(mape)
write.csv(tbl, "tbl.csv", row.names = F)

# ggplot - predicted value: ----------------------------------------------------
gg_ls = list()
for (i in tbl$course) {
  test = mod_ls[[i]][[2]]
  predicted_df = test[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
  y = dt_coursera[CRSE_NM == i]$y
  length(y) = nrow(predicted_df)
  df = cbind(predicted_df, y) %>% 
    pivot_longer(cols = c("yhat", "y"))
  
  cols = c("y" = "black", "yhat" = "#f15c80")
  gg = ggplot(df, aes(ds)) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill="pink") +
    geom_line(aes(y = value, group = name, color = name), size = .8, alpha=.9) +
    scale_colour_manual(values = cols, 
                        labels = c("Actual Value", "Predicted Value")) +
    xlab("") + ylab("Cumulative Enrollments") + theme_bw() + 
    theme(legend.title = element_blank(),
          legend.justification=c(0,1), 
          legend.position=c(0.05, 0.95),
          legend.background = element_blank(),
          legend.key = element_blank())
  
  # Plot all four
  p2 = plot_cross_validation_metric(CV_ls[[i]], metric = "rmse")
  p3 = plot_cross_validation_metric(CV_ls[[i]], metric = "mape")
  p4 = plot_cross_validation_metric(CV_ls[[i]], metric = "coverage")
  gg_all = ggarrange(gg, p2, p3, p4, ncol=2, nrow=2)
  gg_ls[[i]] = annotate_figure(gg_all, 
                  top = text_grob(i, face = "bold", size = 15),
                  bottom = text_grob(
                    paste0("RMSE: ", tbl[tbl$course == i,"rmse"], "; ",
                           "MAPE: ", tbl[tbl$course == i,"mape"], "%"), 
                    color = "blue", face = "italic", size = 12))
}

# Save to pdf
ggexport(gg_ls, "gg_ls0814.pdf")

# Parameters tuning for badly performed models: ---------------------------------
# Random Search: ----------------------------------------------------------------
# Parameters: changepoint_prior_scale, seasonality_prior_scale
# https://www.jmlr.org/papers/volume13/bergstra12a/bergstra12a.pdf
# https://github.com/facebook/prophet/issues/549

# Set values for random search
set.seed(2020)
len = 20
cp_prior_vals = runif(len, 0, 20)
seas_prior_vals = runif(len, 0, 20) # Courses seem not present much seasonality.
tune_ls = list()
crse_name = tbl[tbl$mape > 5,]$course

for(j in crse_name) {
  # create empty vectors
  rmse = vector(length = len)
  mape = vector(length = len)
  
  # Random search `len` times for each course
  for (i in 1:len) {
    tryCatch({
      mod = prophet(dt_coursera[CRSE_NM == j, .(ds, y)], 
                    algorithm = "LBFGS",
                    changepoint.prior.scale = cp_prior_vals[i],
                    seasonality.prior.scale = seas_prior_vals[i])
      future = make_future_dataframe(mod, periods = 30, freq = "day")
      forecast = predict(mod, future)
      horizon = dt_crsa_par[CRSE_NM == j]$horizon
      cv = cross_validation(mod, horizon = horizon, units = "days")
      
      # Record RMSE and MAPE
      mape[i] = eval_mod(cv)[['mape']]
      rmse[i] = eval_mod(cv)[['rmse']]
    }, error = function(e){
      cat("ERROR:", j, i, conditionMessage(e), "\n")})
  }
  
  # Combine the search into df and save it to list
  tryCatch({
    df = data.frame(cbind(rep(j, len), cp_prior_vals, seas_prior_vals, 
                          round(rmse, 2), round(mape, 2)))
    colnames(df) = c("course", "cp_prior_vals", "seas_prior_vals",
                     "rmse", "mape")
    tune_ls[[j]] = df
  }, error = function(e){ cat("ERROR:", j, ".", conditionMessage(e), "\n")})

}
save(tune_ls, file = "tune_ls_over5.RData")
#load("tune_ls_over5.RData")

# Unlist results and pick the best one for each course: -------------------------
idx = c("cp","seas","rmse_new", "mape_new","mape_old","rmse_old")
dt_tune = rbindlist(tune_ls) %>%
  merge(., tbl, by="course", suffixes=c("_new", "_old")) %>%
  .[, (idx) := lapply(.SD, function(x) round(as.numeric(x), 2)), 
    .SDcols=-"course"] %>% 
  .[order(mape_new), .SD, by=course] %>%
  .[(mape_new > 0) | (rmse_new > 0), .SD[1], by=course] %>%
  .[, .(cp, seas, 
        mape_old, mape_new, mape_imp = mape_old - mape_new,
        rmse_old, rmse_new, rmse_imp = rmse_old - rmse_new), 
    by=course]
  
# Function to plot a course:-----------------------------------------------------
runplot4one = function(crse_name, dt_tune) {
  cp = dt_tune[course == crse_name]$cp_prior_vals
  seas = dt_tune[course == crse_name]$seas_prior_vals
  mod = prophet(dt_coursera[CRSE_NM == crse_name, .(ds, y)], 
                algorithm = "LBFGS", 
                changepoint.prior.scale = cp,
                seasonality.prior.scale = seas)
  future = make_future_dataframe(mod, periods = 30, freq = "day")
  forecast = predict(mod, future)
  predicted_df = forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
  y = dt_coursera[CRSE_NM == crse_name]$y
  length(y) = nrow(predicted_df)
  df = cbind(predicted_df, y) %>% 
    pivot_longer(cols = c("yhat", "y"))
  
  cols = c("y" = "black", "yhat" = "#f15c80")
  gg = ggplot(df, aes(ds)) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill="pink") +
    geom_line(aes(y = value, group = name, color = name), size = .8, alpha=.9) +
    scale_colour_manual(values = cols, 
                        labels = c("Actual Value", "Predicted Value")) +
    xlab("") + ylab("Cumulative Enrollments") + theme_bw() + 
    theme(legend.title = element_blank(),
          legend.justification=c(0,1), 
          legend.position=c(0.05, 0.95),
          legend.background = element_blank(),
          legend.key = element_blank())
  return(gg)
}

# Function to tune hyper-parameters for a course: -------------------------------
tune_par = function(crse_name, len, cp_prior_vals_min, cp_prior_vals_max,
                    seas_prior_vals_min, seas_prior_vals_max) {
  set.seed(2020)
  cp_prior_vals = runif(len, cp_prior_vals_min, cp_prior_vals_max)
  seas_prior_vals = runif(len, seas_prior_vals_min, seas_prior_vals_max)
  rmse = vector(length = len)
  mape = vector(length = len)
  for (i in 1:len) {
    tryCatch({
      mod = prophet(dt_coursera[CRSE_NM == crse_name, .(ds, y)], 
                    algorithm = "LBFGS",
                    changepoint.prior.scale = cp_prior_vals[i],
                    seasonality.prior.scale = seas_prior_vals[i])
      future = make_future_dataframe(mod, periods = 30, freq = "day")
      forecast = predict(mod, future)
      horizon = dt_crsa_par[CRSE_NM == crse_name]$horizon
      cv = cross_validation(mod, horizon = horizon, units = "days")
      idx = year(cv$ds) >= 2017
      mape[i] = eval_mod(cv)[['mape']]
      rmse[i] = eval_mod(cv)[['rmse']]
    }, error = function(e){
      cat("ERROR:", j, i, conditionMessage(e), "\n")})
  }
  
  df = data.frame(cbind(rep(course, len), cp_prior_vals, seas_prior_vals, 
                        round(rmse, 2), round(mape, 2)))
  colnames(df) = c("course", "cp_prior_vals", "seas_prior_vals","rmse", "mape")
  return(df)
}

# Run one model: ----------------------------------------------------------------
course = ""
View(dt_coursera[CRSE_NM == course])
ls = prophet_predict_crsa(course, periods=30, cp=.05, seas=10)
mod = ls[[1]]
forecast = ls[[2]]

# Plot forecasts
plot(mod, forecast)
prophet_plot_components(mod, forecast)

# Cross Validation
horizon = dt_crsa_par[CRSE_NM == course]$horizon
cv = cross_validation(mod, horizon = horizon, units = "days")

# Performance metric
eval_mod(cv)
plot_cross_validation_metric

# Successful Negotiation: Essential Strategies and Skills: ----------------------
course = "Successful Negotiation: Essential Strategies and Skills"
# 2020/6/6 (141), 6/7 (136), 6/17 (203) had high enrollments.
# This is a teach-out course, ignore it.

# Community Organizing for Social Justice: --------------------------------------
course = "Community Organizing for Social Justice"
tune_par(course, 10, 0, 30, 0, 10)

# The Finite Element Method for Problems in Physics: ----------------------------
course = "The Finite Element Method for Problems in Physics"
tune_par(course, 10, 0, 30, 0, 10)

# Applied Social Network Analysis in Python (CV_ls didn't worked): -------------
#Error: Optimization terminated abnormally. Falling back to Newton optimizer
course = "Applied Social Network Analysis in Python"

# Several extremely large counts in data, had double checked
# Remove outlier (one with CNT = 9365)
dt = dt_coursera[CRSE_NM==course, .(ds, CNT, y)]
mod = prophet(dt[, .(ds, y)], algorithm="LBFGS")
future = make_future_dataframe(mod, periods = 30, freq = "day")
forecast = predict(mod, future)
plot(mod, forecast) + ggtitle(course)
cv = cross_validation(mod, horizon=horizon, units="days")

# Conclusion
new = dt_tune[(dt_tune$mape_imp > 0) & (mape_new < 10), 
              .(course, mape=mape_new)]
df_final = rbind(tbl[!(tbl$course %in% new$course), c("course", "mape")], 
                 new) %>% arrange(mape)
rownames(df_final) = 1:nrow(df_final)
df_final[df_final$course == "Community Organizing for Social Justice", 
         "mape"] = 10.88
df_final[df_final$course == "The Finite Element Method for Problems in Physics", 
         "mape"] = 6.39
df_final = df_final[-nrow(df_final),]


# Models fitting before 0814: ---------------------------------------------------
# Inspect "Financial Accounting for Construction Projects"
crse_name = "Financial Accounting for Construction Projects"
runplot4one(crse_name, dt_tune)

tune_par(course, len = 20, cp_prior_vals_min = 0, cp_prior_vals_max = 10,
         seas_prior_vals_min = 0, seas_prior_vals_max = 20)
write.csv(df, file="fa4cp.csv", row.names = F)

# Inspect "Using Python to Access Web Data"
course = "Using Python to Access Web Data"
tune_par(course, len = 20, cp_prior_vals_min = 0, cp_prior_vals_max = 10,
         seas_prior_vals_min = 0, seas_prior_vals_max = 20)

# Inspect "Introduction to Natural Language Processing"
course = "Introduction to Natural Language Processing"
df = tune_par(course, len = 20, cp_prior_vals_min = 0, cp_prior_vals_max = 20,
         seas_prior_vals_min = 0, seas_prior_vals_max = 10)

