### Keagan H Rankin
### 3/13/2023

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
starts_csv <- readr::read_csv("C:/Users/Keagan Rankin/OneDrive - University of Toronto/Saxe - Rankin/Project 2. Housing Projections/Data/houses/house_starts.csv")

# group by year
starts_year <- starts_csv |> 
  group_by(`year`) |> 
  summarise(`Total units` = sum(`Total units`),
            `Apartment` = sum(`Apartment and other unit type`),
            `Multiples` = sum(`Multiples`),
            `Row` = sum(`Row`),
            `Semi-detached` = sum(`Semi-detached`),
            `Single-detached` = sum(`Single-detached`),
            ) |> as_tsibble(index=year)

starts_year <- starts_year |> filter_index("1960" ~ "2022")

autoplot(starts_year |> filter_index("1960" ~ "2022"))

# Setting for conf interval in plots
conf_int = 80

#-----------------------------------------------------------------------------
## BASELINE MODEL CROSS VAL
## CROSS VALIDATION FOR MODEL SELECTION AND HYPERPARAMETERS
startsy_cv <- starts_year |> stretch_tsibble(.init=3, .step=1)

startsy_cv_models <-  startsy_cv |>
  model(
    Mean = MEAN(`Total units`),
    `Naïve` = NAIVE(`Total units`),
    `Seasonal naïve` = SNAIVE(`Total units`),
    Drift = RW(`Total units` ~ drift())
  )

startsy_cv_models |> forecast(h=10) |> accuracy(starts_year)

# Which forecast looks best for h step forward forecast.
fc <- startsy_cv_models |>
  forecast(h = 10) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Total units", distribution = `Total units`)

fc |> accuracy(starts_year, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE, colour=.model)) +
  geom_point() + geom_line()

# Fit and Plot
starts_fit_baseline <- starts_year |> model(`Naive` = NAIVE(`Total units`),
                                            `Drift`= RW(`Total units` ~ drift()))

starts_fc_baseline <- starts_fit_baseline |> forecast(h=10)


baseline_a <- starts_fc_baseline |> filter(.model == 'Drift') |> 
  autoplot(size=1.5, level=conf_int, color='deeppink') +
  autolayer(starts_year, color='grey', size=1.5) + 
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title='Drift Model', y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        legend.position="none")
  

baseline_a

baseline_b <- starts_fc_baseline |> filter(.model == 'Naive') |> 
  autoplot(size=1.5, level=conf_int, color='cornflowerblue') +
  autolayer(starts_year, color='grey', size=1.5) + 
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title='Naive Model', y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        legend.position='none')

baseline_b

grid.arrange(baseline_a, baseline_b, nrow=2)

## EVALUATING BASELINE MODELS (without a test set)
# Check innovation residual properties: uncorrelated, zero mean
baseline_aug <- starts_fit_baseline |> augment()

# Mean check as per 5.4
starts_fit_baseline |> select(`Drift`) |> gg_tsresiduals()

# ljung test, l = 2m = 2*4 = 8 or l = T/5 which is > 8
baseline_aug |> filter(.model=='Drift') |> features(.innov, ljung_box, lag=10)

#--
# BASELINE EVALS:
# The mean model has lower RMSE at 8 steps, BUT the drift model, while wider,
# is uncorrolated and passes ljung-box/residual diagnostics. They are both
# okay baselines with wide margin for error.
#--




#-------------------------------------------------------------------------------
starts_train <- starts_year |> filter_index(. ~ '2012') #FIX SHOULD BE 2014
#-------------------------------------------------------------------------------
## ETS MODEL

#Cross Validation and check models
starts_train_ets <- starts_train |> 
  model(
    ANN = ETS(`Total units`~ error("A") + trend("N")), 
    AAN= ETS(`Total units`~ error("A") + trend("A")),
    AAN= ETS(`Total units`~ error("A") + trend("A")),
    AadN = ETS(`Total units`~ error("A") + trend("Ad")),
    MNA = ETS(`Total units` ~ error('M') + trend('N')),
    MA = ETS(`Total units` ~ error('M') + trend('A')),
    ) 

starts_train_ets |> forecast(h=10) |> accuracy(starts_year) |> arrange(RMSE)

#Refit to data
starts_fit_ets <- starts_year |> 
  model(ETS(`Total units` ~ error("A") + trend("A")))

starts_fit_ets_aa <- starts_year |> 
  model(ETS(`Total units` ~ error("A") + trend("A")))

starts_fit_ets_aad <- starts_year |> 
  model(ETS(`Total units` ~ error("A") + trend("Ad")))

# Model AA performs better RMSE but fails the box jung test, where as the 
# damped model passes. We likely want to use both and average
starts_fit_ets |> gg_tsresiduals() # residuals not normal, bootstrap.
augment(starts_fit_ets) |> features(.innov, ljung_box, lag=10)

# Forecast final model
starts_fc_ets <- starts_fit_ets |> forecast(h=10, bootstrap=TRUE)
starts_fc_ets_aa <- starts_fit_ets_aa |> forecast(h=10, bootstrap=TRUE)
starts_fc_ets_aad <- starts_fit_ets_aad |> forecast(h=10, bootstrap=TRUE)

# Plot final model
ets_plot <-starts_fc_ets |>
  autoplot(size=1.5, level=conf_int, color='darkviolet', alpha=1) +
  autolayer(starts_year, color='grey', size=1.5) + 
  autolayer(starts_fc_ets_aad, level=conf_int, 
            size=1.5, color='light blue', alpha=0.7) +
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title="Holt's ETS Model AA and AAd", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        legend.position='none')

ets_plot

starts_fc_ets |> as_tibble() |> select(.mean) |> sum()


#-------------------------------------------------------------------------------
## ARIMA MODEL
# Check out residuals
starts_year |> gg_tsdisplay(`Total units`)
starts_year |> gg_tsdisplay(difference(`Total units`), plot_type='partial')

# Training set
starts_train_arima <- starts_train |> 
  model(
    ar110 = ARIMA(`Total units` ~ pdq(1,1,0)),
    ar210 = ARIMA(`Total units` ~ pdq(2,1,0)),
    ar111 = ARIMA(`Total units` ~ pdq(1,1,1)),
    ar201 = ARIMA(`Total units` ~ pdq(2,0,1)),
    ar100 = ARIMA(`Total units` ~ pdq(1,0,0)),
    ar101 = ARIMA(`Total units` ~ pdq(1,0,1)),
    arfoo = ARIMA(`Total units` ~ pqd(2,1,1)),
    auto_ar = ARIMA(`Total units`, stepwise = FALSE, approx = FALSE),
    
    )

# Choose best model from training
starts_train_arima |> forecast(h=10) |> accuracy(starts_year) |> arrange(RMSE)
glance(starts_train_arima) |> arrange(AICc)

# 210 is about the same AICc as 110 (penalized a bit for model complexity)
# BUT its RMSE is much lower, so I would say it is a better model.
# 111, 110, 210 all perform about the same

starts_train_arima |> select(ar210) |> gg_tsresiduals()
arima_res <- augment(starts_train_arima) |> select(.resid)
qqnorm(arima_res$.resid, frame=FALSE)
qqline(arima_res$.resid, col='steelblue', lwd=2)
augment(starts_train_arima) |> features(.innov, ljung_box, lag = 1)


# Train final model
starts_fit_arima <- starts_year |> 
  model(ARIMA(`Total units`~pdq(2,1,0))) #,stepwise=FALSE, approx=FALSE))

# Forecast final model
starts_fc_arima <- starts_fit_arima |> forecast(h=10, bootstrap=TRUE)

# Plot final model
arima_plot <-starts_fc_arima |>
  autoplot(size=1.5, level=conf_int, color='darkseagreen') +
  autolayer(starts_year, color='grey', size=1.5) + 
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title="210 ARIMA Model", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        legend.position='none')

arima_plot

#btw, this is how you return the confidence interval
starts_fc_arima |> hilo(level=95)

#-------------------------------------------------------------------------------
## SOME EXPERIMENTING WITH MORE ADVANCED METHODS: BOOTSTRAPPING AND NN

## Neural Net Autoregression
starts_train_NN <- starts_train |> 
  model(
    NNETAR(`Total units`)
    )

# Looks good. Also look at RMSE to see how it compares to previous models
starts_train_NN
starts_train_NN |> gg_tsresiduals()
NN_res <- augment(starts_train_NN) |> select(.resid)
qqnorm(NN_res$.resid, frame=FALSE)
qqline(NN_res$.resid, col='steelblue', lwd=2)
augment(starts_train_NN) |> features(.innov, ljung_box, lag = 1)

starts_train_NN |> forecast(h=10, times=1000) |> accuracy(starts_year) # It sucks.

# Train final model 
starts_fit_NN <- starts_year |> 
  model(NNETAR(`Total units`)) 

# Forecast final model
starts_fc_NN <- starts_fit_NN |> forecast(h=10, times=1000)

# Plot final model
NN_plot <-starts_fc_NN |>
  autoplot(size=1.5, level=conf_int, color='bisque4') +
  autolayer(starts_year, color='grey', size=1.5) + 
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title="Neural Net AR Model", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        legend.position='none')

NN_plot


## Bootstrapping Method
# First we need to decompose the time series, which I did in Housing EDA file
starts_y_stl <- starts_year |> model(stl = STL(`Total units`))

starts_y_stl |> components() |> autoplot()

# generate new bootstrapped data on the same time period as the original from
# the decomp
starts_y_stl |> 
  generate(new_data=starts_year,
           times=25,
           boostrap_block_size=20) |>
  autoplot(.sim) +
  autolayer(starts_year) +
  guides(colour='none') + theme_classic()

# Now store these
starts_sim <- starts_y_stl |> 
  generate(new_data = starts_year,
           times=200,
           bootstrap_block_size=20)

starts_sim <- starts_sim |> select(-.model, -`Total units`)

# Fit an ETS model on each series. Bag the models.
ets_forecasts <- starts_sim |> 
  model(
    ets = ETS(.sim) #~ error("A") + trend("A"))
    ) |> 
  forecast(h=10)

ets_forecasts |> update_tsibble(key = .rep) |>
  autoplot(.mean) +
  autolayer(starts_year) +
  guides(colour='none') + theme_classic()

# you can do the same with any model, like ARIMA or others
ar_forecasts <- starts_sim |> 
  model(
    ets = ARIMA(.sim) #~ error("A") + trend("A"))
  ) |> 
  forecast(h=10)

ar_forecasts |> update_tsibble(key = .rep) |>
  autoplot(.mean) +
  autolayer(starts_year) +
  guides(colour='none') + theme_classic()

bagged_ar <- ar_forecasts |> summarise(bagged_mean = mean(.mean))

#Now bag and plot the ETS
bagged <- ets_forecasts |> summarise(bagged_mean = mean(.mean))

bagged

starts_year |> 
  model(ets=ETS(`Total units`)) |>
  forecast(h=10) |> autoplot(starts_year) +
  autolayer(bagged, bagged_mean)

bagged_plot <- autoplot(bagged, size=1.5, color='blue') +
  autolayer(starts_year, color='grey', size=1.5) +
  autolayer(bagged_ar, color='darkorange2', size=1.5) +
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title="Bagged Models", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22))

bagged_plot



# COMBINE ALL PLOTS
a = 20000
b = 165000

grid.arrange(baseline_a + ylim(a,b) + labs(x=''),
             baseline_b + labs(y='', x='') + ylim(a,b),
             arima_plot + labs(y='', x='') + ylim(a,b),
             ets_plot + ylim(a,b),
             bagged_plot + labs(y='') + ylim(a,b),
             NN_plot + labs(y='') + ylim(a,b),
             nrow=2)#,top='80% CI Housing Starts Forecast in Ontario, Canada')


starts_fc_arima |> hilo(level=95) |> select(`95%`)

starts_fc_arima |> hilo(level=95) |> 
  mutate(lower=`95%`$lower,
         upper=`95%`$upper) |>
  select(c(.model, upper,lower, .mean)) #|>
  #summarize(up_s = sum(`upper`))


###-----------------------------------------------------------------------------
## CALCULATING LOWER AND UPPER 95, PLOTTING
# Create a function that returns the upper and lower 95
u_l_m <- function(forecast, lvl) {
  # Create upper lower mean df and return
  up_low_m <- forecast |> 
    hilo(level=lvl) |>
    mutate(lower=`95%`$lower,
           upper=`95%`$upper) |>
    select(c(.model, upper,lower, .mean,))
  
  return(up_low_m)
  
}

# Put all of the forecasts in a list, loop through them and apply the function.
fc_list <- list(starts_fc_baseline |> filter(.model == 'Drift'),
             starts_fc_baseline |> filter(.model == 'Naive'),
             starts_fc_arima,
             starts_fc_ets,
             starts_fc_NN
            )

ulm_list <- c()

for (f in fc_list) {
  ulm_list <- c(ulm_list, u_l_m(f, 95))
}

# Returns a large list with all of the upper, lowers and means
# Store all of the values in a data frame for plotting.
ulm_list[2:4] |> sapply(sum)

ulm.data <- data.frame(
  Interval = c('upper 95%','lower 95%', 'mean'),
  Drift = ulm_list[2:4] |> sapply(sum),
  Naive = ulm_list[7:9] |> sapply(sum),
  `Arima_210` = ulm_list[12:14] |> sapply(sum),
  `Holt_ETS` = ulm_list[17:19] |> sapply(sum),
  `Bagged_ETS` = c(0, 0, 747958.5),
  `NeuralNet_AR` = ulm_list[22:24] |> sapply(sum)
 )

ulm.data

# Rehape ulm to correctly plot
library(reshape)
ulm.data |> melt(id.vars=c('Interval'))

# Plot in bar chart
label_scatter <- tibble(
  differential = c(5.1,4.3), y = c(1440000, 1740000), 
  label = c("Gov Ont Bill 23 goal", 
            "CMHC 2030 estimate to achieve affordability"),
  color = c("#737373", "#737373")
)

label_scatter
library(ggtext)


ggplot(ulm.data|> melt(id.vars=c('Interval')), 
       aes(x=variable, y=value, fill=Interval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_hline(yintercept = 1500000, size = 1, color = "#737373", linetype='dotted') +
  geom_hline(yintercept = 1800000, size = 1, color = "light grey", linetype='dotted') +
  theme_classic(base_size=20) +
  scale_fill_manual(values=c('#ACE4AA','#90708C','#49306B')) +
  labs(title='Total Housing Starts vs. Policy Goal', y='Total starts', x='Model') +
  theme(plot.title = element_text(face="bold", size=22)) +
  geom_richtext(
    data = label_scatter,
    aes(x = differential, y = y, label = label, color = I(color)),
    fill = "#FFFFFF", label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
    family = "Chivo", hjust = 0.1, fontface = "bold",
    size = 4
  )



### ----------------------------------------------------------------------------
## COMBINED MODEL
# We create an average model of all of the best ETS and ARIMA models,
# and compare. This is likely the model to be used in the greater FIG model.
# use simple average.
starts_train_combined <- starts_train |> 
  model(
    AAN= ETS(`Total units`~ error("A") + trend("A")),
    AadN = ETS(`Total units`~ error("A") + trend("Ad")),
    ar110 = ARIMA(`Total units` ~ pdq(1,1,0)),
    ar210 = ARIMA(`Total units` ~ pdq(2,1,0)),
    ar111 = ARIMA(`Total units` ~ pdq(1,1,1)),
  ) |>
  mutate(combination = (AAN + AadN)/2)

# Choose best model from training
starts_train_combined |> forecast(h=10) |> accuracy(starts_year) |> arrange(RMSE)

# Looks like the combined model and ETS(AA) are the best, though AA does
# does not pass the ljung test for autocorrelation.
# The combination model with the best RMSE on the test set is a 
# combination of AAN, AadN simply averaged.
# residuals are not normally distributed, ljung box is better!
starts_train_combined |> select(combination) |> gg_tsresiduals()
combi_res <- augment(starts_train_combined) |> select(.resid)
qqnorm(combi_res$.resid, frame=FALSE)
qqline(combi_res$.resid, col='steelblue', lwd=2)
augment(starts_train_combined) |> features(.innov, ljung_box, lag = 10)


# Train final model
starts_fit_combined <- starts_year |> 
  model(
    AAN= ETS(`Total units`~ error("A") + trend("A")),
    AadN = ETS(`Total units`~ error("A") + trend("Ad")),
  ) |>
  mutate(combination = (AAN + AadN)/2)

starts_fc_combined <- starts_fit_combined |> forecast(h=10)


# Plot final model
combined_plot <- starts_fc_combined|>#[starts_fc_combined$.model == 'combination',] |>
  autoplot(size=1.5, level=conf_int) +
  autolayer(starts_year, color='grey', size=1.5) + 
  #scale_colour_manual(values=c('darkviolet','violet','#3A4E48')) +
  #autolayer() +
  #autolayer() +
  ylim(20000,200000) +
  theme_classic(base_size=20) +
  labs(title="Combined ETS Model", y='Housing starts', x='Year') +
  theme(plot.title = element_text(face="bold", size=22),
        #legend.position='none'
        )

combined_plot


# Generate samples
combined_futures <- starts_fit_combined |>
  generate(h=10, times=10000)

combined_futures <- combined_futures[combined_futures$.model == 'combination',]

#write.csv(combined_futures, 'combined_model_future_starts.csv')













