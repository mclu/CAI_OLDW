# Center for Academic Innovation - Time Series Forecasting with Facebook's Prophet

## Overview
The repo took the data from [Michigan online](https://www.online.umich.edu) and modeled online enrollments to track course growth. The course growth prediction helped the marketing team target certain courses and ask for advertisement money from Coursera.

## Navigation
- [OLDW.R](https://github.com/mclu/CAI_OLDW_prophet/blob/master/OLDW.R): The comprehensive script for data cleaning and Prophet model fitting.
- [OLDW_prep0729.Rmd](https://github.com/mclu/CAI_OLDW_prophet/blob/master/OLDW_prep0729.Rmd): The script cleans the data and presents run plots for Coursera courses.
- [prophet0812.Rmd](https://github.com/mclu/CAI_OLDW_prophet/blob/master/prophet0812.Rmd), [prophet0824.Rmd](https://github.com/mclu/CAI_OLDW_prophet/blob/master/prophet0824.Rmd): The scripts take the cleaned data and fit prophet model, then use random search for hyperparameter tuning.
- [forecasting plots.pdf](https://github.com/mclu/CAI_OLDW_prophet/blob/master/forecasting%20plots.pdf): Forecasting result for 75 courses.

## Reference
- Forecasting at scale. https://facebook.github.io/prophet/
