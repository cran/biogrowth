## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)

## -----------------------------------------------------------------------------
set.seed(1241)

## -----------------------------------------------------------------------------
# Specify the model
my_model <- "modGompertz"

# Define the starting parameters (`mu`, `lambda` and `C`), and the initial count in a list
my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)

# Define the storage times
my_time <- seq(0, 100, length = 1000)

# Call to predict_isothermal_growth using the arguments defined above
static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)

# Plotting
plot(static_prediction) 

## -----------------------------------------------------------------------------
# Plotting
plot(static_prediction, 
     line_col = "red", # also takes strings or hexadecimal code, try entering "#56B4E9" for blue 
     line_size = 1,
     line_type  = "dashed")


## -----------------------------------------------------------------------------
# Example data from the main vignette
my_conditions <- tibble(time = c(0, 5, 40),
                         temperature = c(20, 30, 35),
                         pH = c(7, 6.5, 5)
                         )
my_primary <- list(mu_opt = 2,
             Nmax = 1e8,
             N0 = 1e0,
             Q0 = 1e-3)

sec_temperature <- list(model = "Zwietering",
                        xmin = 25,
                        xopt = 35,
                        n = 1)

sec_pH = list(model = "CPM",
              xmin = 5.5,
              xopt = 6.5,
              xmax = 7.5,
              n = 2)

my_secondary <- list(
    temperature = sec_temperature,
    pH = sec_pH
    )

my_times <- seq(0, 50, length = 1000)

# Calling the function
dynamic_prediction <- predict_dynamic_growth(my_times,
                       my_conditions, my_primary,
                       my_secondary)


## -----------------------------------------------------------------------------
plot(dynamic_prediction, 
     add_factor = "temperature",
     line_col = "blue")

## -----------------------------------------------------------------------------
# Example data from the main vignette
my_data <- tibble(time = c(0, 25, 50, 75, 100),
                  logN = c(2, 2.5, 7, 8, 8))
my_model <- "Baranyi"
known <- c(mu = .2)
start = c(logNmax = 8, lambda = 25, logN0 = 2)

# Fitting
static_fit <- fit_isothermal_growth(my_data, my_model,
                      start, known
                      )


## -----------------------------------------------------------------------------
# Plotting
plot(static_fit, 
     point_col = "lightgreen", 
     point_size = 3, 
     line_type = 3)

## -----------------------------------------------------------------------------
# Example data from the main vignette
my_model <- "Trilinear"
my_times <- seq(0, 30, length = 100)
n_sims <- 1000

# Calling the function
stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims,
  mean_logN0 = 0, sd_logN0 = .2,
  mean_sqmu = 2,sd_sqmu = .3,
  mean_sqlambda = 4, sd_sqlambda = .4,
  mean_logNmax = 6, sd_logNmax = .5)

## -----------------------------------------------------------------------------
plot(stoc_growth, 
     line_type = 3,
     ribbon80_fill = "#00dbdb",
     ribbon90_fill = "#00dbdb",
     alpha80 = .5,
     alpha90 = .4)

## -----------------------------------------------------------------------------
plot(stoc_growth, 
     line_type = 3,
     ribbon80_fill = "red",
     ribbon90_fill = "grey",
     alpha80 = .5,
     alpha90 = .4)

## -----------------------------------------------------------------------------
time_distrib <- distribution_to_logcount(stoc_growth, 4)

## -----------------------------------------------------------------------------
plot(time_distrib, bin_width = 2)

## ---- fig.width=7, fig.height=5-----------------------------------------------

library(cowplot)

p1 <- plot(static_fit, 
     point_col = "lightgreen", 
     point_size = 3, 
     line_type = 3)

p2 <- plot(dynamic_prediction, 
     add_factor = "temperature",
     line_col = "blue")

p3 <- plot(stoc_growth, 
     line_type = 3,
     ribbon80_fill = "#00dbdb",
     ribbon90_fill = "#00dbdb",
     alpha80 = .5,
     alpha90 = .4)

p4 <- plot(stoc_growth, 
     line_type = 3,
     ribbon80_fill = "red",
     ribbon90_fill = "grey",
     alpha80 = .5,
     alpha90 = .4)

# Making a nice grid of plots
plot_grid(p1, p2, p3, p4, labels = "AUTO")



## -----------------------------------------------------------------------------
# Axis line thickness via theme()
plot(static_prediction, line_col = "red") + 
  theme(axis.line = element_line(colour = 'grey', size = 1)) + # Adjust size and colour to taste
  xlab("Time")

## -----------------------------------------------------------------------------
# From the previous example 
plot(static_prediction, line_col = "red") + 
  theme(axis.line = element_line(colour = 'grey', size = 0.8)) + # Adjust size and colour to taste
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 10)) # changing the axis limits

## -----------------------------------------------------------------------------
# We save the plot as p1, notice that it does not get drawn now
p1 <- plot(static_prediction, line_col = "red") + 
  theme(axis.line = element_line(colour = 'grey', size = 0.8))

# We save P1 as .pdf, as a 20x 20 cm square
# ggsave("static_prediction.pdf", p1, width = 20, height = 20, units = "cm")

## -----------------------------------------------------------------------------
# Vanilla biogrowth figure
static_prediction$simulation %>%
  ggplot(., aes(x= time, y =logN )) + 
  geom_line() +
  theme_cowplot()

## -----------------------------------------------------------------------------
# Directly accessing the simulation data from static_prediction
P <- ggplot(static_prediction$simulation, aes(x= .data$time, y =.data$logN)) + 
  geom_line(color="red") +
  theme_cowplot()

P +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 10)) +
  theme(axis.line = element_line(colour = 'black', size = 0.4))

## -----------------------------------------------------------------------------
# Example of full control over stochastic simulation

# Defining parameters 
my_model <- "Trilinear"
my_times <- seq(0, 30, length = 100)
n_sims <- 1000

# Calling the function
stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims,
  mean_logN0 = 0, sd_logN0 = .2,
  mean_sqmu = 2,sd_sqmu = .3,
  mean_sqlambda = 4, sd_sqlambda = .4,
  mean_logNmax = 6, sd_logNmax = .5)

# Plotting
ggplot(stoc_growth$quantiles, aes(x = .data$time)) +
        geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90), fill = "red",  alpha = .3) +
        geom_ribbon(aes(ymin = .data$q05, ymax = .data$q95), fill = "red",  alpha = .2) +
        geom_line(aes(y = .data$q50)) +
        xlab("Time") +
        ylab("logN") +
        cowplot::theme_cowplot() 
 

## ---- fig.width=7, fig.height=5-----------------------------------------------
# Plotting multiple plots together
my_model <- "modGompertz"

my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)
my_time <- seq(0, 100, length = 1000)
static_prediction1 <- predict_isothermal_growth(my_model, my_time, my_pars)
p1 <- plot(static_prediction1)

my_pars <- list(logN0 = 2, C = 6, mu = .5, lambda = 10)
my_time <- seq(0, 100, length = 1000)
static_prediction2 <- predict_isothermal_growth(my_model, my_time, my_pars)
p2 <- plot(static_prediction2)

my_pars <- list(logN0 = 2, C = 6, mu = .3, lambda = 15)
my_time <- seq(0, 100, length = 1000)
static_prediction3 <- predict_isothermal_growth(my_model, my_time, my_pars)
p3 <- plot(static_prediction3)


# Plotting using the cowplot plot_grid function
plot_grid(p1, p2, p3, labels = "auto")



## -----------------------------------------------------------------------------
bind_rows(
  tibble(Plot = rep("p1", 1000), time = static_prediction1$simulation$time, logN = static_prediction1$simulation$logN),
  tibble(Plot = rep("p2", 1000), time = static_prediction2$simulation$time, logN = static_prediction2$simulation$logN),
  tibble(Plot = rep("p3", 1000), time = static_prediction3$simulation$time, logN = static_prediction3$simulation$logN)) %>%
  ggplot(., aes(x = .data$time, y = .data$logN, col = .data$Plot)) + 
  geom_line() +
  theme_cowplot()


