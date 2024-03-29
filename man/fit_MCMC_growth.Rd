% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_dynamic_growth.R
\name{fit_MCMC_growth}
\alias{fit_MCMC_growth}
\title{Fit growth models using MCMC}
\usage{
fit_MCMC_growth(
  fit_data,
  env_conditions,
  starting_point,
  known_pars,
  sec_model_names,
  niter,
  ...,
  check = TRUE,
  formula = logN ~ time,
  logbase_mu = logbase_logN,
  logbase_logN = 10
)
}
\arguments{
\item{fit_data}{Tibble with the data to use for model fit. It must
contain a column with the elapsed time (named "time" by default) and another one
with the decimal logarithm of the observed population size (named "logN" by default).
Different column names can be specified using the "formula" argument.}

\item{env_conditions}{Tibble with the (dynamic) environmental conditions
during the experiment. It must have one column with the
elapsed time (named "time" by default) and as many columns as required with the
environmental conditions. A different column name can be specified using the
"formula" argument, although it must be the same one as in "fit_data".
Note that only those defined in "sec_model_names" will be considered for
the model fit.}

\item{starting_point}{A named vector of starting values for the model parameters.
Parameters for the primary model must be named in the usual way. Parameters for the
secondary model are named as \code{env_factor}+'_'+\code{parameter}. For instance,
the maximum growth temperature shall be named 'temperature_xmax'.}

\item{known_pars}{A named vector of known model parameters (i.e. not fitted). They
must be named using the same convention as for \code{starting_point}.}

\item{sec_model_names}{A named character vector defining the secondary model for each
environmental factor. The names define the factor and the value the type of model.
Names must match columns in \code{fit_data} and \code{env_conditions}.}

\item{niter}{number of iterations of the MCMC algorithm.}

\item{...}{Additional arguments passed to modFit.}

\item{check}{Whether to check model parameters (TRUE by default).}

\item{formula}{an object of class "formula" describing the x and y variables.
\code{logN ~ time} as a default.}

\item{logbase_mu}{Base of the logarithm the growth rate is referred to.
By default, the same as logbase_logN. See vignette about units for details.}

\item{logbase_logN}{Base of the logarithm for the population size. By default,
10 (i.e. log10). See vignette about units for details.}
}
\value{
An instance of \code{\link[=FitDynamicGrowthMCMC]{FitDynamicGrowthMCMC()}}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

The function \code{\link[=fit_MCMC_growth]{fit_MCMC_growth()}} has been superseded by the top-level
function \code{\link[=fit_growth]{fit_growth()}}, which provides a unified approach for growth modelling.

But, it can still fit a growth model to a data obtained under dynamic conditions
using the one-step approach (MCMC algorithm).
}
\examples{
\donttest{
## We use the example data included in the package

data("example_dynamic_growth")
data("example_env_conditions")

## Definition of the secondary models
sec_model_names <- c(temperature = "CPM", aw= "CPM")

## Any model parameter can be fixed
known_pars <- list(Nmax = 1e4,  # Primary model
    N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
    mu_opt = 4, # mu_opt of the gamma model
    temperature_n = 1,  # Secondary model for temperature
    aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
    )

## We need starting values for the remaining parameters

my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
    temperature_xmax = 40,
    aw_xopt = .95)

## We can now call the fitting function

set.seed(12124) # Setting seed for repeatability

my_MCMC_fit <- fit_MCMC_growth(example_dynamic_growth, example_env_conditions,
    my_start, known_pars, sec_model_names, niter = 3000)

## Always check the MCMC chain!!

plot(my_MCMC_fit$fit_results)

## We can compare data against fitted curve

plot(my_MCMC_fit)

## Any environmental factor can be included using add_factor

plot(my_MCMC_fit, add_factor = "temperature",
    label_y1 = "Count (log CFU/ml)", label_y2 = "Temperature (C)")

}

}
