### Keagan H Rankin
### 2/13/2023

### THIS FILE explores fitting baseline forecast models to the data.
rm(list = ls())
#Ctrl L to clear console.
dev.off(dev.list()["RStudioGD"]) #Use to clear graphics

# Load required packages
library(fpp3)
library(GGally)
library(gridExtra)

#-----------------------------------------------------------------------------
## CLEAN
# First load in starts for example
starts <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/houses/house_starts.csv")

starts <- starts |>
  mutate(date = yearquarter(date)) |>
  select(-c(year,
            quarter,
            `Apartment and other unit type`, 
            `Single-detached`, 
            Multiples, Row, `Semi-detached`)) |>
  as_tsibble(index = date)

starts <- starts |> filter_index("Q1 1955" ~ "Q4 2022")

starts

#------------------------------------------------------------------------------
## CROSS VALIDATION FOR MODEL SELECTION AND HYPERPARAMETERS
starts_cv <- starts |> stretch_tsibble(.init=3, .step=1)

starts_cv_models <-  starts_cv |>
  model(
    Mean = MEAN(`Total units`),
    `Naïve` = NAIVE(`Total units`),
    `Seasonal naïve` = SNAIVE(`Total units`),
    Drift = RW(`Total units` ~ drift())
  )

starts_cv_models |> forecast(h=32) |> accuracy(starts)

# Naive Seasonal and Mean forecasts produce the lowest error, so we use these.


fc <- starts_cv_models |>
  forecast(h = 32) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Total units", distribution = `Total units`)

fc |> accuracy(starts, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE, colour=.model)) +
  geom_point() + geom_line()



#------------------------------------------------------------------------------
## FITTING BASELINE MODELS

# For a 8 year forecast, seasonal naive has lowest RMSSE along with the mean.
# retrain these two models on the entire dataset.
starts_fit_mean <- starts |> model(`Mean` = MEAN(`Total units`))
starts_fit_ns <- starts |> model(`Seasonal naïve` = SNAIVE(`Total units`))


starts_fc_mean <- starts_fit_mean |> forecast(h=32)
starts_fc_ns <- starts_fit_ns |> forecast(h=32)

# Plot
starts_fc_mean |> autoplot(
  starts |> filter_index("Q1 1980" ~ "Q4 2022"), color='red') + theme_classic()

starts_fc_ns |> autoplot(
  starts |> filter_index("Q1 1980" ~ "Q4 2022")) + theme_classic()


#------------------------------------------------------------------------------
## EVALUATING BASELINE MODELS (without a test set)

# Check innovation residual properties: uncorrelated, zero mean
mean_aug <- starts_fit_mean |> augment()
ns_aug <- starts_fit_ns |> augment()

# Mean check as per 5.4
starts_fit_mean |> gg_tsresiduals()
starts_fit_ns |> gg_tsresiduals()


# ljung test, l = 2m = 2*4 = 8 or l = T/5 which is > 8
mean_aug |> features(.innov, ljung_box, lag=8)
ns_aug |> features(.innov, ljung_box, lag=8)

# Conclusions -> stats are very high, our data is autocorrolated and the mean
# for the naive seasonal looks to be > 0. 
# Our forecast's do not satisfy the basic properties, so they can be improved
# by other methods.


#-------------------------------------------------------------------------------
### EXTRA STUFF
# This is how you stack autoplots - autolayer!
#https://stackoverflow.com/questions/57693208/automatically-plots-with-autoplot-function-from-forecasting-object

autoplot(starts_fc_ns, size=0.5, level=95, color='blue') + 
  autolayer(starts |> filter_index("Q1 1980" ~ "Q4 2022"), 
            size=0.5, color='azure3') + 
  theme_classic()


# Decomposition model might work better, check cv accuracy
starts_dcmp <- starts_cv |>
  model(stlf = decomposition_model(
    STL(`Total units` ~ season(window = 13) + trend(window=20), robust = TRUE),
    NAIVE(season_adjust)
  ))

starts_dcmp |> forecast(h=32) |> accuracy(starts)
# It is a bit better than the naive forecast model!


starts_dcmp_train <-  starts |>
  model(stlf = decomposition_model(
    STL(`Total units` ~ season(window = 13) + trend(window=20), robust = TRUE),
    NAIVE(season_adjust)
  ))

starts_dcmp_fc <- starts_dcmp_train |> forecast(h=32)

naive_dcmp_plot <- autoplot(starts_dcmp_fc, size=0.6, level=80, color='orange') + 
  autolayer(starts |> filter_index("Q1 1970" ~ "Q4 2022"), 
            size=0.6, color='azure3') + 
  labs(title='STL decomp naive forecast') +
  theme_classic() + ylim(0,40000)



#-----------------------------------------------------------------------------
## FITTING AN ARIMA MODEL TO STARTS
# The baseline models above are insufficient. Try some more complex models
# S-ARIMA -> Should fit
# Arima takes awhile to CV train and we get a bunch of candidate models,
# so add in a train-test alternative.
starts_train <- starts |> filter_index(. ~ '2008 Q4')

starts |> gg_tsdisplay(difference(`Total units`, 4),
                        plot_type='partial')+
  labs(title='Seasonally Differenced')

# Candidate model from above: ARIMA(1,0,2)(1,1,1) maybe?
#CV Train
#starts_cv_arima <- starts_cv |> 
#  model(
#    arima112101 = ARIMA(`Total units` ~ pdq(1,1,2) + PDQ(1,0,1)),
#    auto_ar = ARIMA(`Total units`) #stepwise = FALSE, approx = FALSE)
#  )

# Train test
starts_train_arima <- starts_train |> 
  model(
    #arima112101 = ARIMA(`Total units` ~ pdq(1,1,2) + PDQ(1,0,1)),
    auto_ar = ARIMA(`Total units`, stepwise = FALSE, approx = FALSE)
  )

# Check out the chosen model, plot resids, qq-plot and ljung test
starts_train_arima
glance(starts_train_arima) |> arrange(AICc)

starts_train_arima |> gg_tsresiduals()
arima_res <- augment(starts_train_arima) |> select(.resid)
qqnorm(arima_res$.resid, frame=FALSE)
qqline(arima_res$.resid, col='steelblue', lwd=2)

augment(starts_train_arima) |> features(.innov, ljung_box, lag = 8, dof = 5)
# Pass! Although if we penalized tails we may not be normal.
# Lets retrain on all data and plot model
starts_fit_arima <- starts |> 
  model(ARIMA(`Total units`, stepwise=FALSE, approx=FALSE))
  
starts_fc_arima <- starts_fit_arima |> forecast(h=32)
  
arima_plot <- starts_fc_arima |> 
  autoplot(starts|>filter_index('1970 Q1'~.), size=0.6) + 
  labs(title='Housing starts: ARIMA') + theme_classic() + ylim(0,40000)

starts_fit_arima |> gg_tsresiduals()
augment(starts_fit_arima) |> features(.innov, ljung_box, lag = 8, dof = 4)


#-------------------------------------------------------------------------------
## FITTING AN ETS MODEL TO STARTS

starts_train_ets <- starts_train |> model(ETS(`Total units`))
starts_train_ets |> gg_tsresiduals()
augment(starts_train_ets) |> features(.innov, ljung_box, lag = 8)

starts_fit_ets <- starts |> model(ETS(`Total units`))

starts_fc_ets <- starts_fit_ets |> forecast(h=32, bootstrap=TRUE)

ets_plot <-starts_fc_ets |> 
  autoplot(starts|>filter_index('1970 Q1'~.), size=0.6, color='red') + 
  labs(title='ETS') + theme_classic() + ylim(0,40000)


#-------------------------------------------------------------------------------
## FUN WITH PLOTTING
grid.arrange(arima_plot, ets_plot, naive_dcmp_plot, nrow=3)






















  