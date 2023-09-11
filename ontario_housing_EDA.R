### Keagan H Rankin
### 02/5/2023

### THIS FILE performs EDA on the housing time series as per Chapters 2-4
### methods of the FPP textbook (and methods I already know)

rm(list = ls())
#Ctrl L to clear console.
dev.off(dev.list()["RStudioGD"]) #Use to clear graphics

# Load required packages
library(fpp3)
library(GGally)
library(reshape2)

#-----------------------------------------------------------------------------
## Import the three .csv of the time series and format in tsibble
starts <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/house_starts.csv")
construction <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/house_under_construction.csv")
completions <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/house_completions.csv")

starts <- starts %>%
  mutate(time_index = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(index = time_index)

construction <- construction %>%
  mutate(time_index = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(index = time_index)

completions <- completions %>%
  mutate(time_index = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(index = time_index)

starts


#-----------------------------------------------------------------------------
## Create Time plots
autoplot(starts) +
  labs(y = "Housing Starts",
       title = "Housing Starts",
       subtitle = "Time Series Plot")

autoplot(construction) +
  labs(y = "Housing Constructions",
       title = "Housing Constructions",
       subtitle = "Time Series Plot")

autoplot(completions) +
  labs(y = "Housing Completions",
       title = "Housing Completions",
       subtitle = "Time Series Plot")

#-----------------------------------------------------------------------------
## Create seasonal plots 
starts %>% gg_season(period = 'year', size=0.65) +
  theme(legend.position = 'none') +
  labs(y='Starts', title='Starts per year plot')

construction %>% gg_season(period = 'year', size=0.65) +
  theme(legend.position = 'none') +
  labs(y='Constructions', title='Under Construction per year plot')

completions %>% gg_season(period = 'year', size=0.65) +
  theme(legend.position = 'none') +
  labs(y='Completions', title='Completions per year plot')


starts %>% gg_subseries() +
  labs(
    y = "starts",
    title = "subseries starts"
  )

construction %>% gg_subseries() +
  labs(
    y = "under construction",
    title = "subseries starts"
  )

completions %>% gg_subseries() +
  labs(
    y = "completions",
    title = "subseries starts"
  )

# What if we want to see breakdown by type. We have to melt and remake tsibble
starts_m <- starts[-(1:25),] %>% select(-c(year, quarter))
starts_m <- melt(starts_m, id.vars='time_index', variable.name='type')

#starts_m <- starts_m %>%
#  select(-time_index) %>%
#  as_tsibble(key = c(type),
#             index = time_index)


#ggplot(starts_m, aes(time_index, value), geom_line(aes(colour=type)))

plot.ts(starts[-(1:25),] %>% select(-c(year, quarter)))


#-----------------------------------------------------------------------------
## CREATE SCATTERPLOTS
starts[-(1:25),] %>% select(-c(year, quarter,'Apartment and other unit type')) %>% 
  GGally::ggpairs()
#just do this pairplot in python, there is something preventing it here in R.


#-----------------------------------------------------------------------------
## AUTOCORRELATION PLOTS
# Create some simple lag plots
starts %>% gg_lag(`Total units`, geom='point')
construction %>% gg_lag(`Total units`, geom='point')
completions %>% gg_lag(`Total units`, geom='point')

# Now the autocorrelation plots
starts %>% ACF(`Total units`, lag_max = 200) %>%
  autoplot() + labs(title='Total Starts')

construction %>% ACF(`Total units`, lag_max = 200) %>% 
  autoplot() + labs(title='Total Under Construction')

completions %>% ACF(`Total units`, lag_max = 200) %>%
  autoplot() + labs(title='Total Completions')


#-----------------------------------------------------------------------------
## X11 DECOMPOSITION
# Decompose the housing time series

# Create a function that decomposes and creates some plots:
housing_x11 <- function(house_sibble) {
  x11_dcmp <- house_sibble %>%
    model(x11 = X_13ARIMA_SEATS(`Total units` ~ x11())) %>% components()
  
  return(x11_dcmp)
}

# run the function and plot
starts_x11 = housing_x11(starts)

starts_x11 %>% autoplot() +
  labs(title ="Decomposition of housing starts using X-11.")

# look at the variation of the seasonal component over time
starts_x11 %>% gg_subseries(seasonal)

# Constructions and completions
cons_x11 = housing_x11(construction)
cons_x11 %>% autoplot() +
  labs(title ="Decomposition of housing under construction using X-11.")
cons_x11 %>% gg_subseries(seasonal)

complete_x11 = housing_x11(completions)
complete_x11 %>% autoplot() +
  labs(title ="Decomposition of housing completions using X-11.")
complete_x11 %>% gg_subseries(seasonal)

# Plot the seasonally adjusted data
starts_x11 %>%
  ggplot(aes(x = time_index)) +
  geom_line(aes(y = `Total units`, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Starts", title = "Housing Starts") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

cons_x11 %>%
  ggplot(aes(x = time_index)) +
  geom_line(aes(y = `Total units`, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "under cons", title = "Housing Under Construction") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

complete_x11 %>%
  ggplot(aes(x = time_index)) +
  geom_line(aes(y = `Total units`, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Completions", title = "Housing Completions") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

## STL DECOMPOSITION
completions %>%
  model(STL(`Total units` ~ trend(window = 13) +
          season(window = 13),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#This STL is probably "too rigid" as txt puts it. Trend is smooth and 
#"leaks" more into the remainder.


# TRANSFORMATION
# Try using a Box-Cox adjustment on the starts and under construction data?
#completions have small seasonal variations, where as construction is in
#between and starts has very high variations.

# Feature PCA for dimension reduction and data understanding
starts_m_ts <- starts_m |> mutate(time_index = yearquarter(time_index)) |>
  as_tsibble(key=type, index=time_index)
starts_m_ts |> features(value, feat_stl) #where starts_m = tsibble of current form.


























