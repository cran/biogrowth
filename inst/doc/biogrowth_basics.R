## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)

## -----------------------------------------------------------------------------
library(tidyverse)

## -----------------------------------------------------------------------------
set.seed(1241)

## -----------------------------------------------------------------------------
data("example_cardinal")
head(example_cardinal)

## -----------------------------------------------------------------------------
data("example_env_conditions")
head(example_env_conditions)

## -----------------------------------------------------------------------------
data("example_dynamic_growth")
head(example_dynamic_growth)

## -----------------------------------------------------------------------------
my_model <- "modGompertz"

## -----------------------------------------------------------------------------
my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)

## -----------------------------------------------------------------------------
my_time <- seq(0, 100, length = 1000)

## -----------------------------------------------------------------------------
static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)


## -----------------------------------------------------------------------------
static_prediction$simulation

## -----------------------------------------------------------------------------
# this is a bit confusing to me, as normally plot() refers to base R plotting functions (but I will adapt :P)
plot(static_prediction)

## -----------------------------------------------------------------------------
plot(static_prediction) +
  xlab("Storage time (h)") +
  ylab("Microbial count (log CFU/ml)") +
  theme_gray()

## -----------------------------------------------------------------------------
my_conditions <- tibble(time = c(0, 5, 40),
                         temperature = c(20, 30, 35),
                         pH = c(7, 6.5, 5)
                         )

## -----------------------------------------------------------------------------
ggplot(my_conditions) + 
  geom_line(aes(x = time, y = temperature))

## -----------------------------------------------------------------------------
ggplot(my_conditions) + 
  geom_line(aes(x = time, y = pH))

## -----------------------------------------------------------------------------
# can you give a sort of range for this theoretical substance? Or should the user play around a bit to find a value that suits the needs? 
my_primary <- list(mu_opt = 2,
             Nmax = 1e8,
             N0 = 1e0,
             Q0 = 1e-3)

## -----------------------------------------------------------------------------
sec_temperature <- list(model = "Zwietering",
                        xmin = 25,
                        xopt = 35,
                        n = 1)

## -----------------------------------------------------------------------------
sec_pH = list(model = "CPM",
              xmin = 5.5,
              xopt = 6.5,
              xmax = 7.5,
              n = 2)

## -----------------------------------------------------------------------------
my_secondary <- list(
    temperature = sec_temperature,
    pH = sec_pH
    )

## -----------------------------------------------------------------------------
my_times <- seq(0, 50, length = 1000)

## -----------------------------------------------------------------------------
dynamic_prediction <- predict_dynamic_growth(my_times,
                       my_conditions, my_primary,
                       my_secondary)

## -----------------------------------------------------------------------------
dynamic_prediction$simulation

## -----------------------------------------------------------------------------
plot(dynamic_prediction)

## -----------------------------------------------------------------------------
plot(dynamic_prediction, add_factor = "temperature")

## -----------------------------------------------------------------------------
plot(dynamic_prediction, add_factor = "temperature", ylims= c(0, 8), 
     label_y1 = "Microbial count (log CFU/ml)", label_y2 = "Storage temperature (ºC)")

## -----------------------------------------------------------------------------
time_to_logcount(static_prediction, 2.5)

## -----------------------------------------------------------------------------
time_to_logcount(dynamic_prediction, 5)

## -----------------------------------------------------------------------------
time_to_logcount(dynamic_prediction, 10)

## -----------------------------------------------------------------------------
my_data <- tibble(time = c(0, 25, 50, 75, 100),
                  logN = c(2, 2.5, 7, 8, 8))

## -----------------------------------------------------------------------------
my_model <- "Baranyi"

## -----------------------------------------------------------------------------
known <- c(mu = .2)

## -----------------------------------------------------------------------------
start = c(logNmax = 8, lambda = 25, logN0 = 2)

## -----------------------------------------------------------------------------
static_fit <- fit_isothermal_growth(my_data, my_model,
                      start, known
                      )

## -----------------------------------------------------------------------------
summary(static_fit)

## -----------------------------------------------------------------------------
plot(static_fit)

## -----------------------------------------------------------------------------
data("example_cardinal")
head(example_cardinal)

## -----------------------------------------------------------------------------
sec_model_names <- c(temperature = "Zwietering",
                     pH = "CPM")

## -----------------------------------------------------------------------------
known_pars <- list(mu_opt = 1.2,
                   temperature_n = 1,
                   pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2
                   )


## -----------------------------------------------------------------------------
my_start <- list(temperature_xmin = 5, temperature_xopt = 35,
               pH_xopt = 6.5)

## -----------------------------------------------------------------------------
fit_cardinal <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)

## -----------------------------------------------------------------------------
summary(fit_cardinal)

## -----------------------------------------------------------------------------
data("example_dynamic_growth")
data("example_env_conditions")

## -----------------------------------------------------------------------------
head(example_env_conditions)

## -----------------------------------------------------------------------------
ggplot(example_env_conditions, aes(x = time, y = temperature)) + 
  geom_line() +
  geom_point()

## -----------------------------------------------------------------------------
head(example_dynamic_growth)

## -----------------------------------------------------------------------------
sec_model_names <- c(temperature = "CPM",
                     aw= "CPM")

## -----------------------------------------------------------------------------
known_pars <- list(Nmax = 1e4,  # Nmax for primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1,  # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
)

## -----------------------------------------------------------------------------
my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
               temperature_xmax = 40,
               aw_xopt = .95)


## -----------------------------------------------------------------------------
my_dyna_fit <- fit_dynamic_growth(example_dynamic_growth, example_env_conditions, 
                         my_start,
                         known_pars, 
                         sec_model_names)

## -----------------------------------------------------------------------------
summary(my_dyna_fit)

## -----------------------------------------------------------------------------
plot(my_dyna_fit)

## -----------------------------------------------------------------------------
plot(my_dyna_fit, add_factor = "aw",
     label_y1 = "Log count (log CFU/ml)",
     label_y2 = "Water activity")

## -----------------------------------------------------------------------------
my_model <- "Trilinear"

## -----------------------------------------------------------------------------
my_times <- seq(0, 30, length = 100)

## -----------------------------------------------------------------------------
n_sims <- 1000

## -----------------------------------------------------------------------------
stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims,
  mean_logN0 = 0, sd_logN0 = .2,
  mean_sqmu = 2,sd_sqmu = .3,
  mean_sqlambda = 4, sd_sqlambda = .4,
  mean_logNmax = 6, sd_logNmax = .5)

## -----------------------------------------------------------------------------
plot(stoc_growth)

## -----------------------------------------------------------------------------
my_cor <- matrix(c(1,   0,   0, 0,
                   0,   1, 0.7, 0,
                   0, 0.7,   1, 0,
                   0,   0,   0, 1),
                 nrow = 4)

## -----------------------------------------------------------------------------
stoc_growth2 <- predict_stochastic_growth(my_model, my_times, n_sims,
  mean_logN0 = 0, sd_logN0 = .2,
  mean_sqmu = 2,sd_sqmu = .3,
  mean_sqlambda = 4, sd_sqlambda = .4,
  mean_logNmax = 6, sd_logNmax = .5,
  my_cor)

plot(stoc_growth2)

## -----------------------------------------------------------------------------

data("example_dynamic_growth")
data("example_env_conditions")

sec_model_names <- c(temperature = "CPM",
                     aw= "CPM")

known_pars <- list(Nmax = 1e4,  # Primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1,  # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
)

my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
                 temperature_xmax = 40,
                 aw_xopt = .95)


## -----------------------------------------------------------------------------
my_MCMC_fit <- fit_MCMC_growth(example_dynamic_growth, example_env_conditions, 
                                my_start,
                                known_pars, 
                                sec_model_names, 
                                niter = 500)


## -----------------------------------------------------------------------------
summary(my_MCMC_fit)

## -----------------------------------------------------------------------------
plot(my_MCMC_fit)

## -----------------------------------------------------------------------------
plot(my_MCMC_fit, add_factor = "temperature")

## -----------------------------------------------------------------------------
example_env_conditions

## -----------------------------------------------------------------------------
my_times <- seq(0, 15, length = 5)

## -----------------------------------------------------------------------------
niter <- 100

## ---- warning=FALSE-----------------------------------------------------------

my_MCMC_prediction <- predict_MCMC_growth(my_MCMC_fit, 
                                          my_times,
                                          example_env_conditions, 
                                          niter)

## -----------------------------------------------------------------------------
my_MCMC_prediction$quantiles

## -----------------------------------------------------------------------------
plot(my_MCMC_prediction)

## -----------------------------------------------------------------------------
better_prediction <- predict_MCMC_growth(my_MCMC_fit, 
                                          seq(0, 15, length = 100),
                                          example_env_conditions, 
                                          niter)

## -----------------------------------------------------------------------------
plot(better_prediction)

## -----------------------------------------------------------------------------
time_distrib <- distribution_to_logcount(stoc_growth, 4)

## -----------------------------------------------------------------------------
time_distrib$summary

## -----------------------------------------------------------------------------
plot(time_distrib)

