% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_isothermal_growth.R
\name{predict_isothermal_growth}
\alias{predict_isothermal_growth}
\title{Isothermal microbial growth}
\usage{
predict_isothermal_growth(
  model_name,
  times,
  model_pars,
  check = TRUE,
  logbase_mu = 10,
  logbase_logN = 10
)
}
\arguments{
\item{model_name}{Character defining the growth model.}

\item{times}{Numeric vector of storage times for the predictions.}

\item{model_pars}{Named vector or list defining the values of the model parameters.}

\item{check}{Whether to do basic checks (TRUE by default).}

\item{logbase_mu}{Base of the logarithm the growth rate is referred to.
By default, the same as logbase_logN. See vignette about units for details.}

\item{logbase_logN}{Base of the logarithm for the population size. By default,
10 (i.e. log10). See vignette about units for details.}
}
\value{
An instance of \code{\link[=IsothermalGrowth]{IsothermalGrowth()}}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

The function \code{\link[=predict_isothermal_growth]{predict_isothermal_growth()}} has been superseded by the top-level
function \code{\link[=predict_growth]{predict_growth()}}, which provides a unified approach for growth modelling.

Regardless of that, it can still be used to predict population growth under static
environmental conditions (i.e. using primary models).
}
\examples{
## Define the simulations parameters

my_model <- "modGompertz"
my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)
my_time <- seq(0, 100, length = 1000)

## Do the simulation

static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)

## Plot the results

plot(static_prediction)


}
