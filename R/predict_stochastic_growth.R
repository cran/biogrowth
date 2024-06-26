
#' Deprecated isothermal growth with parameter uncertainty
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' [predict_stochastic_growth()] was renamed [predict_growth_uncertainty()] because
#' the original function name may be misleading, as this is not a stochastic
#' differential equation
#' 
#' @inheritParams predict_growth_uncertainty
#' 
#' @importFrom lifecycle deprecate_warn
#' @export
#' 
predict_stochastic_growth <- function(model_name, 
                                      times, 
                                      n_sims,
                                      pars, 
                                      corr_matrix = diag(nrow(pars)),
                                      check = TRUE
                                      ) {
    
    deprecate_warn("1.0.0", "predict_stochastic_growth()", 
                   "predict_growth_uncertainty()")
    
    predict_growth_uncertainty(model_name, 
                               times, 
                               n_sims,
                               pars, 
                               corr_matrix,
                               check
                               )
    
}


#' Isothermal growth with parameter uncertainty
#' 
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Simulation of microbial growth considering uncertianty in the model parameters.
#' Calculations are based on 
#' Monte Carlo simulations, considering the parameters follow a multivariate normal
#' distribution. 
#' 
#' @details
#' The distributions of the model parameters are
#' defined in the `pars` argument using a tibble with 4 columns:
#' \itemize{
#'     \item{par: identifier of the model parameter (according to [primary_model_data()])},
#'     \item{mean: mean value of the model parameter.},
#'     \item{sd: standard deviation of the model parameter.},
#'     \item{scale: scale at which the model parameter is defined. Valid values are
#'     'original' (no transformation), 'sqrt' square root or 'log' log-scale. The
#'     parameter sample is generated considering the parameter follows a marginal
#'     normal distribution at this scale, and is later converted to the original scale
#'     for calculations.}
#' }
#'
#' @param model_name Character describing the primary growth model.
#' @param times Numeric vector of storage times for the simulations.
#' @param n_sims Number of simulations.
#' @param pars A tibble describing the parameter uncertainty (see details).
#' @param corr_matrix Correlation matrix of the model parameters. Defined in the
#' same order as in `pars`. An identity matrix by default
#' (uncorrelated parameters).
#' @param check Whether to do some tests. `FALSE` by default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, the same as logbase_logN. See vignette about units for details. 
#' @param logbase_logN Base of the logarithm for the population size. By default,
#' 10 (i.e. log10). See vignette about units for details.
#'
#' @return An instance of [GrowthUncertainty()].
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom rlang set_names .data
#' @importFrom purrr map
#' @importFrom purrr imap_dfr
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## Definition of the simulation settings
#'
#' my_model <- "Baranyi"
#' my_times <- seq(0, 30, length = 100)
#' n_sims <- 3000
#' 
#' library(tibble)
#' 
#' pars <- tribble(
#'     ~par, ~mean, ~sd, ~scale,
#'     "logN0", 0, .2, "original",
#'     "mu", 2, .3, "sqrt",
#'     "lambda", 4, .4, "sqrt",
#'     "logNmax", 6, .5, "original"
#' )
#' 
#' ## Calling the function
#' 
#' stoc_growth <- predict_growth_uncertainty(my_model, my_times, n_sims, pars)
#'
#' ## We can plot the results
#'
#' plot(stoc_growth)
#'
#' ## Adding parameter correlation
#'
#' my_cor <- matrix(c(1,   0,   0, 0,
#'     0,   1, 0.7, 0,
#'     0, 0.7,   1, 0,
#'     0,   0,   0, 1),
#'     nrow = 4)
#'
#' stoc_growth2 <- predict_growth_uncertainty(my_model, my_times, n_sims, pars, my_cor)
#'
#' plot(stoc_growth2)
#' 
#' ## The time_to_size function can calculate the median growth curve to reach a size
#' 
#' time_to_size(stoc_growth, 4)
#' 
#' ## Or the distribution of times
#' 
#' dist <- time_to_size(stoc_growth, 4, type = "distribution")
#' plot(dist)
#' 
#' }
#' 
#'
predict_growth_uncertainty <- function(model_name, 
                                      times, 
                                      n_sims,
                                      pars, 
                                      corr_matrix = diag(nrow(pars)),
                                      check = TRUE,
                                      logbase_mu = logbase_logN, logbase_logN = 10
                                      ) {
    
    ## Checks
    
    if (isTRUE(check)) {
        
        check_stochastic_pars(model_name, pars, corr_matrix)
        
    }
    
    ## Generate the parameter sample
    
    mus <- pars$mean
    stdevs <- pars$sd
    b <- stdevs %*% t(stdevs)
    cov_matrix <- b * corr_matrix
    
    par_sample <- as.data.frame(mvrnorm(n_sims,
                                        mus,
                                        cov_matrix)
                                ) %>%
        set_names(pars$par)
    
    ## Undo the transformation
    
    for (i in 1:nrow(pars)) {
        transf <- pars$scale[i]
        
        new_col <- switch(transf,
                          original = par_sample[,i],
                          sqrt = par_sample[,i]^2,
                          log = 10^par_sample[,i],
                          stop("Unknown scale:", transf)
        )
        
        par_sample[,i] <- new_col
    }

    ## Do the simulations

    aa <- par_sample %>%
        mutate(iter = row_number())

    my_sims <- split(aa, aa$iter) %>%
        # split(.$iter) %>%
        map(as.list) %>%
        map(~ predict_isothermal_growth(model_name, times, ., check = FALSE,
                                        logbase_mu = logbase_mu,
                                        logbase_logN = logbase_logN)) %>%
        map(~ .$simulation) %>%
        imap_dfr(~ mutate(.x, iter = .y))

    ## Extract quantiles

    q_values <- my_sims %>%
        group_by(.data$time) %>%
        summarize(q50 = quantile(.data$logN, probs = .5, na.rm=TRUE),
                  q10 = quantile(.data$logN, probs = .1, na.rm=TRUE),
                  q90 = quantile(.data$logN, probs = .9, na.rm=TRUE),
                  q05 = quantile(.data$logN, probs = .05, na.rm=TRUE),
                  q95 = quantile(.data$logN, probs = .95, na.rm=TRUE),
                  m_logN= mean(.data$logN, na.rm=TRUE)
        )


    ## Output

    out <- list(sample = par_sample,
                simulations = my_sims,
                quantiles = q_values,
                model = model_name,
                mus = mus,
                sigma = cov_matrix,
                logbase_logN = logbase_logN,
                logbase_mu = logbase_mu)

    class(out) <- c("GrowthUncertainty", class(out))

    out

}


#' Model definition checks for predict_stochastic_growth
#' 
#' Does several checks of the model parameters. Besides those by 
#' check_primary_pars, it checks that corr_matrix is square, that pars and
#' corr_matrix have compatible dimensions, and that pars has the correct names.
#' 
#' @inheritParams predict_stochastic_growth
#' 
check_stochastic_pars <- function(model_name, pars, corr_matrix) {
    
    ## Check that corr_matrix is square
    
    if (nrow(corr_matrix) != ncol(corr_matrix)) {
        stop("corr_matrix is not square.")
    }
    
    ## Check the dimensions of corr_matrix and pars
    
    if (nrow(pars) != nrow(corr_matrix)) {
        stop("Incompatible dimensions between pars and corr_matrix.")
    }
    
    ## Check column names in pars
    
    if (isFALSE( all(c("par", "mean", "sd", "scale") %in% names(pars)) )) {
        stop("The names of the argument par must be 'par', 'mean', 'sd' and 'scale'")
    }
    
    ## Call the checks of predict_isothermal_growth
    
    pars_as_list <- as.list(pars$mean)
    names(pars_as_list) <- pars$par
    
    check_primary_pars(model_name, pars_as_list)
    
}









