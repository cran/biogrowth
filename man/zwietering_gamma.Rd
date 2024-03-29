% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamma_models.R
\name{zwietering_gamma}
\alias{zwietering_gamma}
\title{Zwietering gamma model}
\usage{
zwietering_gamma(x, xmin, xopt, n)
}
\arguments{
\item{x}{Value of the environmental factor.}

\item{xmin}{Minimum value of the environmental factor for growth.}

\item{xopt}{Maximum value for growth}

\item{n}{Exponent of the secondary model}
}
\value{
The corresponding gamma factor.
}
\description{
Gamma model as defined by Zwietering et al. (1992). To avoid unreasonable predictions,
it has been modified setting gamma=0 for values of x outside (xmin, xopt)
}
