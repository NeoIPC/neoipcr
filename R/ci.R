#' Exact Poisson confidence interval
#'
#' Computes exact Poisson confidence intervals for count-over-exposure metrics
#' (incidence densities, utilisation densities, procedure rates, antibiotic
#' treatment days, agents per infection). Uses [stats::poisson.test()] which
#' handles zero events gracefully.
#'
#' @param events Integer. Number of observed events (infections, device-days,
#'   procedures, etc.). Must be non-negative.
#' @param exposure Numeric. Total exposure time or denominator (patient-days,
#'   device-days, number of infections, number of patients). Must be positive.
#' @param multiplier Numeric. Scaling factor for the rate. Default 1000
#'   (per 1,000 patient-days). Use 100 for utilisation densities and procedure
#'   rates.
#' @param conf.level Numeric. Confidence level. Default 0.95.
#'
#' @returns A named list with three elements:
#'   \describe{
#'     \item{rate}{The point estimate: `events / exposure * multiplier`}
#'     \item{lower}{Lower bound of the confidence interval, scaled by multiplier}
#'     \item{upper}{Upper bound of the confidence interval, scaled by multiplier}
#'   }
#'
#' @examples
#' # BSI incidence: 47 infections over 28,904 patient-days
#' neoipc_poisson_ci(47, 28904, multiplier = 1000)
#'
#' # Zero events: returns valid CI with lower = 0
#' neoipc_poisson_ci(0, 5000, multiplier = 1000)
#'
#' # Utilisation density (per 100 patient-days)
#' neoipc_poisson_ci(350, 5000, multiplier = 100)
#'
#' @export
neoipc_poisson_ci <- function(events, exposure,
                               multiplier = 1000,
                               conf.level = 0.95) {
  check_number_whole(events, min = 0)
  check_number_decimal(exposure, min = .Machine$double.eps)
  check_number_decimal(multiplier, min = .Machine$double.eps)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  pt <- stats::poisson.test(events, T = exposure, conf.level = conf.level)
  list(
    rate  = events / exposure * multiplier,
    lower = pt$conf.int[1] * multiplier,
    upper = pt$conf.int[2] * multiplier
  )
}

#' Wilson binomial confidence interval
#'
#' Computes Wilson score confidence intervals for true proportions (detection
#' rates, patient proportions). Implements the Wilson score formula directly,
#' which provides better coverage properties than exact (Clopper-Pearson)
#' intervals, particularly for small samples.
#'
#' At the boundaries (`x = 0` or `x = n`) the Wilson interval is degenerate:
#' `margin == center` so the lower bound is exactly 0 at `x = 0` and the upper
#' bound is exactly 1 at `x = n`. In the interior the Wilson interval is
#' narrower than the Clopper-Pearson interval, which gives it superior
#' coverage properties for typical sample sizes.
#'
#' @param x Integer. Number of successes (infections with pathogen, patients
#'   with antibiotic). Must be non-negative.
#' @param n Integer. Number of trials (total infections, total patients).
#'   Must be positive and >= x.
#' @param conf.level Numeric. Confidence level. Default 0.95.
#'
#' @returns A named list with three elements:
#'   \describe{
#'     \item{proportion}{The point estimate: `x / n`}
#'     \item{lower}{Lower bound of the Wilson confidence interval}
#'     \item{upper}{Upper bound of the Wilson confidence interval}
#'   }
#'
#' @examples
#' # Detection rate: 220 infections with pathogen out of 283 total
#' neoipc_wilson_ci(220, 283)
#'
#' # Zero successes: lower bound is exactly 0 at the boundary
#' neoipc_wilson_ci(0, 50)
#'
#' # All successes: upper bound is exactly 1 at the boundary
#' neoipc_wilson_ci(50, 50)
#'
#' @export
neoipc_wilson_ci <- function(x, n, conf.level = 0.95) {
  check_number_whole(x, min = 0)
  check_number_whole(n, min = 1)
  if (x > n) rlang::abort("`x` must be <= `n`.")
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n) / denom

  list(
    proportion = p_hat,
    lower      = center - margin,
    upper      = center + margin
  )
}

# --- Internal vectorized wrappers for table generators ---

#' Compute Poisson CI columns for a vector of events/exposure pairs
#'
#' Returns a two-column tibble (`ci_lower`, `ci_upper`) suitable for
#' `dplyr::bind_cols()`. Rows with NA events, NA exposure, or zero exposure
#' return NA — these represent structurally absent metrics, not zero-event
#' observations.
#'
#' @param events Integer vector.
#' @param exposure Numeric vector (or scalar, recycled).
#' @param multiplier Numeric scalar.
#' @returns A tibble with columns `ci_lower` and `ci_upper`.
#' @noRd
poisson_ci_cols <- function(events, exposure, multiplier) {
  purrr::pmap_dfr(
    list(events = events, exposure = exposure),
    function(events, exposure) {
      if (is.na(events) || is.na(exposure) || exposure == 0) {
        return(tibble::tibble(ci_lower = NA_real_, ci_upper = NA_real_))
      }
      ci <- neoipc_poisson_ci(events, exposure, multiplier = multiplier)
      tibble::tibble(ci_lower = ci$lower, ci_upper = ci$upper)
    })
}

#' Compute Wilson CI columns for a vector of x/n pairs
#'
#' Returns a two-column tibble (`ci_lower`, `ci_upper`) suitable for
#' `dplyr::bind_cols()`. Bounds are multiplied by `scale` to match the
#' rate column's unit (e.g., scale = 100 for percentages). Rows with NA x,
#' NA n, or zero n return NA.
#'
#' @param x Integer vector of successes.
#' @param n Integer vector of trials.
#' @param scale Numeric scalar. Multiplier for the CI bounds. Default 1.
#' @returns A tibble with columns `ci_lower` and `ci_upper`.
#' @noRd
wilson_ci_cols <- function(x, n, scale = 1) {
  purrr::pmap_dfr(
    list(x = x, n = n),
    function(x, n) {
      if (is.na(x) || is.na(n) || n == 0) {
        return(tibble::tibble(ci_lower = NA_real_, ci_upper = NA_real_))
      }
      ci <- neoipc_wilson_ci(x, n)
      tibble::tibble(ci_lower = ci$lower * scale, ci_upper = ci$upper * scale)
    })
}

#' Two-level parametric bootstrap CI for benchmark quartiles
#'
#' Computes confidence intervals for the departmental quartiles (Q1, Q2, Q3)
#' of a surveillance metric. This is the only CI method in the pipeline that
#' uses simulation, because no closed-form solution exists for the two-layer
#' uncertainty structure: within-department sampling noise and cross-department
#' quantile estimation uncertainty.
#'
#' Uses Jeffreys non-informative priors to propagate parameter uncertainty even
#' for zero-count departments. For Poisson rates, the prior is
#' `Gamma(0.5, 0)`; for binomial proportions, `Beta(0.5, 0.5)`. This ensures
#' honest uncertainty propagation where naive resampling from `Poisson(0)`
#' would always return 0.
#'
#' @param events Integer vector. Observed event counts per department. Must be
#'   non-negative.
#' @param exposure Numeric vector. Denominators per department (patient-days,
#'   device-days, total infections, etc.). Must be strictly positive. Same
#'   length as `events`. Departments with zero denominators must be excluded
#'   upstream.
#' @param type Character. Rate type: `"poisson"` for count-over-exposure
#'   densities, `"binomial"` for true proportions. Determines the resampling
#'   distribution.
#' @param multiplier Numeric. Scaling factor applied to bootstrap rates before
#'   quantile computation. Default 1. Use 1000 for incidence densities, 100
#'   for utilisation densities.
#' @param B Integer. Number of bootstrap iterations. Default 2000.
#' @param conf.level Numeric. Confidence level. Default 0.95.
#' @param seed Numeric or NULL. RNG seed for reproducibility. Default 42.
#'   Set to NULL to skip seeding (non-deterministic).
#'
#' @returns A one-row tibble with six columns:
#'   `q1_ci_lower`, `q1_ci_upper`, `q2_ci_lower`, `q2_ci_upper`,
#'   `q3_ci_lower`, `q3_ci_upper`.
#'
#' @examples
#' # Poisson: 6 departments with infection counts over patient-days
#' bootstrap_quantile_ci(
#'   events = c(5, 0, 12, 3, 8, 20),
#'   exposure = c(1000, 800, 1200, 600, 900, 1500),
#'   type = "poisson", multiplier = 1000)
#'
#' # Binomial: detection proportions
#' bootstrap_quantile_ci(
#'   events = c(15, 8, 22, 5, 18, 30),
#'   exposure = c(20, 10, 25, 8, 20, 35),
#'   type = "binomial", multiplier = 100)
#'
#' @export
bootstrap_quantile_ci <- function(events, exposure,
                                   type = c("poisson", "binomial"),
                                   multiplier = 1,
                                   B = 2000,
                                   conf.level = 0.95,
                                   seed = 42) {
  type <- rlang::arg_match(type)
  check_number_decimal(multiplier, min = .Machine$double.eps)
  check_number_whole(B, min = 1)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  if (length(events) != length(exposure)) {
    rlang::abort("`events` and `exposure` must have the same length.")
  }
  if (!is.numeric(events) || any(na.omit(events) != as.integer(na.omit(events)))) {
    rlang::abort("`events` must be a vector of whole numbers (NA allowed).")
  }
  if (!is.numeric(exposure)) {
    rlang::abort("`exposure` must be a numeric vector (NA allowed).")
  }

  # Filter out NA pairs — departments without this metric are structurally
  # absent, not zero-event observations
  valid <- !is.na(events) & !is.na(exposure)
  events <- events[valid]
  exposure <- exposure[valid]

  if (any(events < 0)) {
    rlang::abort("`events` must be non-negative.")
  }
  if (any(exposure <= 0)) {
    rlang::abort("`exposure` must be strictly positive.")
  }
  if (type == "binomial" && any(events > exposure)) {
    rlang::abort("`events` must be <= `exposure` for binomial type.")
  }
  if (type == "binomial" && any(exposure != as.integer(exposure))) {
    rlang::abort(c(
      "`exposure` must be whole numbers for binomial type.",
      "i" = "`exposure` is the trial count `n`; non-integer values would be coerced by `stats::rbinom()`."))
  }

  k <- length(events)
  if (k < 2) {
    return(tibble::tibble(
      q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
      q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
      q3_ci_lower = NA_real_, q3_ci_upper = NA_real_))
  }
  alpha <- 1 - conf.level

  # RNG isolation: save and restore .Random.seed
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()))
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()))
    }
    set.seed(seed)
  }

  # Pre-allocate bootstrap quantile storage
  boot_q <- matrix(NA_real_, nrow = B, ncol = 3)

  for (b in seq_len(B)) {
    if (type == "poisson") {
      # Jeffreys posterior: λ ~ Gamma(events + 0.5, exposure)
      lambda <- stats::rgamma(k, shape = events + 0.5, rate = exposure)
      boot_events <- stats::rpois(k, lambda = lambda * exposure)
      boot_rates <- boot_events / exposure * multiplier
    } else {
      # Jeffreys posterior: p ~ Beta(events + 0.5, exposure - events + 0.5)
      p <- stats::rbeta(k,
                        shape1 = events + 0.5,
                        shape2 = exposure - events + 0.5)
      boot_events <- stats::rbinom(k, size = exposure, prob = p)
      boot_rates <- boot_events / exposure * multiplier
    }
    boot_q[b, ] <- stats::quantile(boot_rates, probs = c(0.25, 0.5, 0.75),
                                    names = FALSE)
  }

  tibble::tibble(
    q1_ci_lower = stats::quantile(boot_q[, 1], probs = alpha / 2, names = FALSE),
    q1_ci_upper = stats::quantile(boot_q[, 1], probs = 1 - alpha / 2, names = FALSE),
    q2_ci_lower = stats::quantile(boot_q[, 2], probs = alpha / 2, names = FALSE),
    q2_ci_upper = stats::quantile(boot_q[, 2], probs = 1 - alpha / 2, names = FALSE),
    q3_ci_lower = stats::quantile(boot_q[, 3], probs = alpha / 2, names = FALSE),
    q3_ci_upper = stats::quantile(boot_q[, 3], probs = 1 - alpha / 2, names = FALSE)
  )
}
