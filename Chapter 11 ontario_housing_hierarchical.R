### Keagan H Rankin
### 3/13/2023

### THIS FILE explores fitting Hierarchical Models to the Different Housing Types
rm(list = ls())
#Ctrl L to clear console.
dev.off(dev.list()["RStudioGD"]) #Use to clear graphics

# Load required packages
library(fpp3)
library(GGally)
library(gridExtra)
library(reshape2)

# First load in starts for example
#starts_csv <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/house_starts.csv")

# group by year
#starts_year <- starts_csv |> 
#  group_by(`year`) |> 
#  summarise(`Total units` = sum(`Total units`),
#            `Apartment` = sum(`Apartment and other unit type`),
#            `Multiples` = sum(`Multiples`),
#            `Row` = sum(`Row`),
#            `Semi-detached` = sum(`Semi-detached`),
#            `Single-detached` = sum(`Single-detached`),
#  )

# Filter years and melt
#starts_year <- melt(starts_year, id.vars=c('year'), var='house_type') 
#starts_year <- starts_year[!duplicated(starts_year),]
#duplicates(starts_year, index=`year`)

path = 'C:\\Users\\Keagan Rankin\\OneDrive - University of Toronto\\Saxe - Rankin\\Project 2. Housing Projections\\Data\\houses\\starts_by_type.csv'
#write.csv(starts_year, path)

#-------------------------------------------------------------------------------
## CLEAN
# Import, load to tsibble, filter
starts_csv <- readr::read_csv(path) |> select(-1) |> filter(house_type != 'Total units')
starts_year <- starts_csv |> as_tsibble(index=`year`, key= c(`house_type`))
starts_year <- starts_year |> filter_index('1960'~'2022')
starts_year <- starts_year[starts_year$house_type != 'Multiples',]

# Use aggregate_key() to create hierarchical time series
starts_hts <- starts_year |>
  aggregate_key(`house_type`, `value`= sum(`value`))

starts_hts

# Some Plotting
starts_hts |> autoplot(value)
starts_hts |> autoplot(value, size=1.2) + facet_wrap(vars(house_type)) + 
  theme_classic(base_size=20)

# You can take single level approaches -> top down vs bottom up.
# General hierarchical workflow:
# data |> aggregate_key() |> model() |> reconcile() |> forecast()

# Top-down approaches are always biased (11.3), generally Minimum Trace
# (MinT) approach is good (Wickramasuriya et al. 2019). It minimizes
# the variances of forecasts by selecting G ( Y = SGy)

#-------------------------------------------------------------------------------
# EXAMPLE - TESTING METHODS
# Create a training set
starts_train <- starts_hts |> filter_index(. ~ '2012')

starts_train_fit <- starts_train |>
  model(base = ETS(value)) |>
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method='ols'),
    mint_s = min_trace(base, method='wls_var') #sub timeseries relatively large.
  )

starts_train_fc <- starts_train_fit |> forecast(h=10)

# Plot against actual data, measure RMSE and other indicators of training
starts_train_fc |>
  autoplot(starts_hts |> filter(year >= 1960),
           level=80) +
  labs(y='Housing Starts') + facet_wrap(vars(house_type), scales='free_y') +
  theme_classic()

starts_train_fc |> 
  filter(is_aggregated(house_type)) |>
  autoplot(starts_hts, alpha=0.7, level=90)

# Bottom up model gives the best estimate on the train set. Go forward with it.
# also, ARIMA isn't even close, ETS is the dominate model for this data.
# addend: bottom up is best on 8 step, mint is best on 10-step.
starts_train_fc |> accuracy(data=starts_hts) |>
  group_by(.model) |>
  summarise(rmse = mean(RMSE), 
            mase = mean(MASE),
            mape = mean(MAPE),
            rmsse = mean(RMSSE),
            acf1 = mean(ACF1)
            )


#-------------------------------------------------------------------------------
### FORECAST
# If we want to maintain positive then we need to take the log
# We can do the log transform, but the function does not return a confidence
# interval. May have to message Hyndman about this.

starts_fit <- starts_hts |>
  model(base = ETS(value)) |>
  reconcile(
    `Bottom Up` = bottom_up(base),
    `MinT Var Scaling` = min_trace(base, method='wls_var'),
    `OLS` = min_trace(base, method='ols')
  )

starts_fc <- starts_fit |> forecast(h=10)

# plot
starts_fc |> filter(.model %in% c("Bottom Up",'MinT Var Scaling', 'OLS')) |>
  autoplot(starts_hts |> filter(year >= 1960),
           alpha = 0.6, level=80, size=1.2) +
  labs(y='Housing Starts') + facet_wrap(vars(house_type), scales='free_y') +
  theme_classic(base_size=20) +
  labs(title="Disaggregated H Forecasts", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        #legend.position='none'
        ) #+ ylim(0,150000)

starts_fc |> filter(.model %in% c("Bottom Up","MinT Var Scaling", 'OLS')) |>
  filter(is_aggregated(house_type)) |>
  autoplot(starts_hts, alpha=0.8, level=90, size=0.7) + theme_classic(base_size=20) +
  labs(title="Aggregated Forecasts", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        #legend.position='none'
        ) #+ ylim(0,150000)


starts_fc



























