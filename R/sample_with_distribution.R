#' Creates Sample Using Existing Distribution
#'
#' Using an existing vector of numeric values, attempts to
#' fit a distribution and then generates a random sample
#' following that distribution.
#'
#' @param x numeric vector to model distribution from
#' @param size A non-negative integer giving the number of items to choose
#' @param seed Optional seed for reproducibility
#'
#' @returns A numeric vector of size `size` approximating the distribution of `x`
sample_with_distribution <- function(x, size, seed = 1234) {

  # validate input
  attempt::stop_if_not(x, is.numeric,
                       msg = "x must be numeric")
  attempt::stop_if_not(size, is.numeric,
                       msg = "size must be numeric")
  attempt::stop_if_not(size, ~ .x > 0,
                      msg = "size must be greater than zero.")

  # FIT DISTRIBUTION

  distribution_attempts <- c("normal", "lognormal", "exponential",
                             "geometric", "logistic", "negative binomial",
                             "gamma", "Poisson", "beta", "cauchy",
                             "chi-squared", "t", "weibull")
  best_fit <- NULL
  best_aic <- Inf
  best_dist <- NA_character_



  for (dist in distribution_attempts) {
    tryCatch(
      {
        cli::cli_inform("Attempting to fit {.emph {dist}} distribution:")
        fit <- suppressWarnings(MASS::fitdistr(x, dist))

        fit_aic <- AIC(fit)
        if (fit_aic < best_aic) {
          best_fit <- fit
          best_aic <- fit_aic
          best_dist <- dist
        }

        distname <- dist
        fit_estimate <- as.list(fit$estimate)

        params <- paste0(names(fit_estimate), ": ", unlist(fit_estimate), collapse = ", ")
        cli::cli_inform("- aic is {fit_aic} for {.emph {dist}} distribution, with parameters {params}.")

      },
      error = function(e) {
        cli::cli_alert_info("! {dist} fit failed with error '{e$message}'.")
        fit <- NULL
      }
    )


    # END LOOP
  }

  message(glue::glue("Best Fit Distribution is {best_dist} with aic {best_aic}."))

  # RANDOM GENERATION
  cli::cli_inform("Generating random data")
  best_fit_estimate <- best_fit$estimate
  withr::with_seed(seed, {
  switch(best_dist,
                    "normal" = stats::rnorm(size,
                                            mean = best_fit_estimate[["mean"]],
                                            sd = best_fit_estimate[["sd"]]
                                            ),
                    "lognormal" = stats::rlnorm(size,
                                                meanlog = best_fit_estimate[["meanlog"]],
                                                sdlog = best_fit_estimate[["sdlog"]]),
                    "exponential" = stats::rexp(size, rate = best_fit_estimate[["rate"]]),
                    "geometric" = stats::rgeom(size, prob = best_fit_estimate[["prob"]]),
                    "logistic" = stats::rlogis(size, location = best_fit_estimate[["location"]],
                                               scale = best_fit_estimate[["scale"]]),
                    "negative binomial" = stats::rnbinom(size, size = best_fit_estimate[["size"]],
                                                         mu = best_fit_estimate[["mu"]]),
                    "gamma" = stats::rgamma(size, shape = best_fit_estimate[["shape"]],
                                            rate = best_fit_estimate[["rate"]]),
                    "Poisson" = stats::rpois(size, lambda = best_fit_estimate[["lambda"]]),
                    "cauchy" = stats::rcauchy(size, location = best_fit_estimate[["location"]],
                                              scale = best_fit_estimate[["scale"]]),
                    "chi-squared" = stats::rchisq(size, df = best_fit_estimate[["df"]]),
                    "t" = stats::rt(size, df = best_fit_estimate[["df"]]),
                    "weibull" = stats::rweibull(size, shape = best_fit_estimate[["shape"]],
                                                scale = best_fit_estimate[["scale"]])
  )
  })
}

