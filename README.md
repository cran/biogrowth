
<!-- README.md is generated from README.Rmd. Please edit that file -->

biogrowth <img src="man/figures/logo.png" align="right" width="120" />
======================================================================

<!-- badges: start -->

[![CRAN
checks](https://cranchecks.info/badges/summary/badger)](https://cran.r-project.org/web/checks/check_results_badger.html)
[![](https://www.r-pkg.org/badges/version/biogrowth?color=green)](https://cran.r-project.org/package=biogrowth)
[![](http://cranlogs.r-pkg.org/badges/last-month/biogrowth?color=green)](https://cran.r-project.org/package=biogrowth)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of biogrowth is to ease the development of mathematical models
to describe population growth. It includes functions for:

-   making predictions under static environmental conditions.
-   making predictions under dynamic environmental conditions.
-   making predictions under static or dynamic conditions considering
    parameter uncertainty.
-   fitting models to data gathered under static environmental
    conditions.
-   fitting models to data gathered under dynamic environmental
    conditions.

The fuctions in biogrowth follow the methods of predictive microbiology,
where the modelling process is divided two steps: primary and secondary
modelling. The user has the flexibility to choose between several
primary (Baranyi, modified Gompertz and Trilinear) and secondary models
(cardinal parameter model, Zwietering-type model, full Ratkowsky model).

Authors
-------

The biogrowth package has been developed by researchers of the Food
Microbiology Laboratory of Wageningen University and Research.

-   Alberto Garre,
-   Jeroen Koomen,
-   Heidy den Besten,
-   Marcel Zwietering.

Questions and comments can be directed to Alberto Garre
(alberto.garreperez (at) wur.nl). For bug reports, please use the GitHub
page of the project.

Installation
------------

You can install the released version of biogrowth from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("biogrowth")

And the development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("albgarre/biogrowth")

Example
-------

As an example of the features included in the package, the following
code chunk generates a prediction of microbial growth under dynamic
conditions considering parameter uncertainty.

    library(biogrowth)
    set.seed(1241)

    my_model <- "Baranyi"
    my_times <- seq(0, 30, length = 100)
    n_sims <- 3000

    stoc_growth <- predict_stochastic_growth(my_model, my_times, n_sims,
        mean_logN0 = 0, sd_logN0 = .2,
        mean_sqmu = 2,sd_sqmu = .3,
        mean_sqlambda = 4, sd_sqlambda = .4,
        mean_logNmax = 6, sd_logNmax = .5)

    plot(stoc_growth)

<img src="man/figures/README-example-1.png" width="100%" />

As an additional example, the following code chunk fits a model to a set
of experiments under dynamic conditions.


    ## We will use the multiple_experiments data set

    data("multiple_experiments")

    ## For each environmental factor, we need to defined a model

    sec_names <- c(temperature = "CPM", pH = "CPM")

    ## Any model parameter can be fixed

    known <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
        temperature_n = 2, temperature_xmin = 20, temperature_xmax = 35,
        pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)

    ## The rest require starting values for model fitting

    start <- list(mu_opt = .8, temperature_xopt = 30)

    ## We can now call the fitting function

    global_fit <- fit_multiple_growth(start, multiple_experiments, known, sec_names)

    ## Any single environmental factor can be added to the plot using add_factor

    plot(global_fit, add_factor = "temperature")

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

This is only a small sample of the functions included in the package.
For a complete list, please check the package vignette.
