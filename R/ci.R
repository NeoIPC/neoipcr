#' Exact Poisson confidence interval
#'
#' Computes exact Poisson confidence intervals for count-over-exposure metrics
#' (incidence densities, utilization densities, procedure rates, antibiotic
#' treatment days, agents per infection). Uses [stats::poisson.test()] which
#' handles zero events gracefully.
#'
#' @param events Integer. Number of observed events (infections, device-days,
#'   procedures, etc.). Must be non-negative.
#' @param exposure Numeric. Total exposure time or denominator (patient-days,
#'   device-days, number of infections, number of patients). Must be positive.
#' @param multiplier Numeric. Scaling factor for the rate. Default 1000
#'   (per 1,000 patient-days). Use 100 for utilization densities and procedure
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
  if (x > n) rlang::abort(gettextf("%s must be <= %s.", "`x`", "`n`"))
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
  if (length(events) == 0L)
    return(tibble::tibble(ci_lower = numeric(), ci_upper = numeric()))
  purrr::pmap(
    list(events = events, exposure = exposure),
    function(events, exposure) {
      if (is.na(events) || is.na(exposure) || exposure == 0) {
        return(tibble::tibble(ci_lower = NA_real_, ci_upper = NA_real_))
      }
      ci <- neoipc_poisson_ci(events, exposure, multiplier = multiplier)
      tibble::tibble(ci_lower = ci$lower, ci_upper = ci$upper)
    }) |>
    purrr::list_rbind()
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
#' @param conf.level Numeric scalar. Confidence level passed on to
#'   [neoipc_wilson_ci()]. Default 0.95.
#' @returns A tibble with columns `ci_lower` and `ci_upper`.
#' @noRd
wilson_ci_cols <- function(x, n, scale = 1, conf.level = 0.95) {
  if (length(x) == 0L)
    return(tibble::tibble(ci_lower = numeric(), ci_upper = numeric()))
  purrr::pmap(
    list(x = x, n = n),
    function(x, n) {
      if (is.na(x) || is.na(n) || n == 0) {
        return(tibble::tibble(ci_lower = NA_real_, ci_upper = NA_real_))
      }
      ci <- neoipc_wilson_ci(x, n, conf.level = conf.level)
      tibble::tibble(ci_lower = ci$lower * scale, ci_upper = ci$upper * scale)
    }) |>
    purrr::list_rbind()
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
#'   for utilization densities.
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
    rlang::abort(gettextf("%s and %s must have the same length.",
                          "`events`", "`exposure`"))
  }
  if (!is.numeric(events) || any(stats::na.omit(events) != as.integer(stats::na.omit(events)))) {
    rlang::abort(gettextf("%s must be a vector of whole numbers (%s allowed).",
                          "`events`", "`NA`"))
  }
  if (!is.numeric(exposure)) {
    rlang::abort(gettextf("%s must be a numeric vector (%s allowed).",
                          "`exposure`", "`NA`"))
  }

  # Filter out NA pairs — departments without this metric are structurally
  # absent, not zero-event observations
  valid <- !is.na(events) & !is.na(exposure)
  events <- events[valid]
  exposure <- exposure[valid]

  if (any(events < 0)) {
    rlang::abort(gettextf("%s must be non-negative.", "`events`"))
  }
  if (any(exposure <= 0)) {
    rlang::abort(gettextf("%s must be strictly positive.", "`exposure`"))
  }
  if (type == "binomial" && any(events > exposure)) {
    rlang::abort(gettextf("%s must be <= %s for binomial type.",
                          "`events`", "`exposure`"))
  }
  if (type == "binomial" && any(exposure != as.integer(exposure))) {
    rlang::abort(c(
      gettextf("%s must be whole numbers for binomial type.", "`exposure`"),
      "i" = gettextf("%s is the trial count %s; non-integer values would be coerced by %s.",
                     "`exposure`", "`n`", "`stats::rbinom()`")))
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

# --- Intervals on effective counts -------------------------------------------
#
# The methods of the confidence intervals that account for the clustering of
# episodes within infants and within departments: an order-statistic interval
# for the departments' quartiles, the exact gamma and Clopper-Pearson
# intervals on counts divided by a design effect for every pooled and
# department figure, the CR2 design effect itself, and the flags that tell a
# reader when an interval rests on too little to carry its own width. They
# are closed-form in base R's stats, deterministic, and need no seed.

#' Order-statistic confidence interval for the departments' quartiles
#'
#' The 95% interval of each quartile (Q1, Q2 and Q3) runs between two order
#' statistics of the departments' rates, with the ranks from the binomial
#' distribution so that each tail holds at most 2.5%. Each department's rate
#' counts once, so the interval carries the clustering within patients and
#' within departments and the uncertainty of quartiles from few departments.
#' A side is open where its rank does not exist or would rest on the lowest
#' or highest department alone; an open limit is stored as `NA` with its side
#' flagged open, and prints as the measure's minimum (lower) or as open
#' (upper). A Q1 of 0 with the interval [0, 0] carries `q1_zero_note`, whose
#' note counts the departments that recorded none of the row's events.
#'
#' @param events Numeric vector. Event counts per department. Non-negative,
#'   `NA` allowed: a department missing this metric is structurally absent.
#' @param exposure Numeric vector. Denominators per department, strictly
#'   positive for the same reason as in [bootstrap_quantile_ci()].
#' @param multiplier Numeric scalar. Scaling factor for the rates.
#' @param conf.level Numeric scalar. Confidence level. Default 0.95.
#'
#' @returns A one-row tibble: `q1_ci_lower`, `q1_ci_upper`, `q2_ci_lower`,
#'   `q2_ci_upper`, `q3_ci_lower`, `q3_ci_upper`; the open-side flags
#'   `q1_ci_lower_open`, `q1_ci_upper_open`, `q2_ci_lower_open`,
#'   `q2_ci_upper_open`, `q3_ci_lower_open`, `q3_ci_upper_open`;
#'   `q1_zero_note`, `zero_departments` and `entering_departments`.
#'
#' @noRd
.order_statistic_quartile_ci <- function(events, exposure,
                                         multiplier = 1,
                                         conf.level = 0.95) {
  check_number_decimal(multiplier, min = .Machine$double.eps)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  if (length(events) != length(exposure)) {
    rlang::abort(gettextf("%s and %s must have the same length.",
                          "`events`", "`exposure`"))
  }
  if (!is.numeric(events)) {
    rlang::abort(gettextf("%s must be a numeric vector (%s allowed).",
                          "`events`", "`NA`"))
  }
  if (!is.numeric(exposure)) {
    rlang::abort(gettextf("%s must be a numeric vector (%s allowed).",
                          "`exposure`", "`NA`"))
  }

  valid <- !is.na(events) & !is.na(exposure)
  events <- events[valid]
  exposure <- exposure[valid]

  if (any(events < 0)) {
    rlang::abort(gettextf("%s must be non-negative.", "`events`"))
  }
  if (any(exposure <= 0)) {
    rlang::abort(gettextf("%s must be strictly positive.", "`exposure`"))
  }

  n <- length(events)
  zero_departments <- sum(events == 0)
  if (n == 0L) {
    return(tibble::tibble(
      q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
      q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
      q3_ci_lower = NA_real_, q3_ci_upper = NA_real_,
      q1_ci_lower_open = TRUE, q1_ci_upper_open = TRUE,
      q2_ci_lower_open = TRUE, q2_ci_upper_open = TRUE,
      q3_ci_lower_open = TRUE, q3_ci_upper_open = TRUE,
      q1_zero_note = FALSE,
      zero_departments = 0L, entering_departments = 0L))
  }

  rates <- sort(events / exposure * multiplier)
  tail_prob <- (1 - conf.level) / 2

  # The innermost ranks whose binomial tails hold at most tail_prob each:
  # P(r <= B < s) covers the quantile, B binomial on n trials and the
  # quantile's probability. A rank of 1 (lower) or n (upper) rests on the
  # lowest or highest department alone and opens its side; so does a side
  # with no rank at all.
  side_ranks <- function(p) {
    cdf <- stats::pbinom(seq_len(n) - 1L, size = n, prob = p)
    r <- if (any(cdf <= tail_prob)) max(which(cdf <= tail_prob)) else NA_integer_
    s <- if (any(cdf >= 1 - tail_prob)) min(which(cdf >= 1 - tail_prob)) else NA_integer_
    lower_open <- is.na(r) || r == 1L
    upper_open <- is.na(s) || s == n
    list(r = r, s = s, lower_open = lower_open, upper_open = upper_open)
  }
  limit <- function(rank, open) if (open) NA_real_ else rates[[rank]]

  q1 <- side_ranks(0.25)
  q2 <- side_ranks(0.50)
  q3 <- side_ranks(0.75)

  tibble::tibble(
    q1_ci_lower = limit(q1$r, q1$lower_open),
    q1_ci_upper = limit(q1$s, q1$upper_open),
    q2_ci_lower = limit(q2$r, q2$lower_open),
    q2_ci_upper = limit(q2$s, q2$upper_open),
    q3_ci_lower = limit(q3$r, q3$lower_open),
    q3_ci_upper = limit(q3$s, q3$upper_open),
    q1_ci_lower_open = q1$lower_open, q1_ci_upper_open = q1$upper_open,
    q2_ci_lower_open = q2$lower_open, q2_ci_upper_open = q2$upper_open,
    q3_ci_lower_open = q3$lower_open, q3_ci_upper_open = q3$upper_open,
    q1_zero_note = !q1$upper_open && zero_departments >= q1$s,
    zero_departments = as.integer(zero_departments),
    entering_departments = as.integer(n))
}

#' Exact gamma (Fay-Feuer) interval for a rate or ratio
#'
#' The gamma interval on the given counts and exposure, which may be the
#' effective counts of a clustered row: [F^-1(alpha/2; y), F^-1(1-alpha/2;
#' y+1)] / n on a rate y/n, F^-1 the gamma quantile with unit scale. On whole
#' counts it is the exact Poisson (Garwood) interval of
#' [stats::poisson.test()], which `stats::poisson.test()` cannot compute on
#' the fractional effective counts.
#'
#' @param events Numeric scalar >= 0. Count or effective count.
#' @param exposure Numeric scalar > 0. Denominator or effective denominator.
#' @param multiplier Numeric scalar. Scaling factor. Default 1.
#' @param conf.level Numeric scalar. Default 0.95.
#'
#' @returns A named list: `rate`, `lower`, `upper`.
#'
#' @noRd
.gamma_rate_ci <- function(events, exposure,
                           multiplier = 1,
                           conf.level = 0.95) {
  check_number_decimal(events, min = 0)
  check_number_decimal(exposure, min = .Machine$double.eps)
  check_number_decimal(multiplier, min = .Machine$double.eps)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  alpha <- 1 - conf.level
  list(
    rate  = events / exposure * multiplier,
    lower = stats::qgamma(alpha / 2, shape = events) / exposure * multiplier,
    upper = stats::qgamma(1 - alpha / 2, shape = events + 1) / exposure * multiplier
  )
}

#' Exact Clopper-Pearson interval for a share
#'
#' The Clopper-Pearson interval on the given successes and trials, which may
#' be effective counts: the equal-tailed beta quantiles, computed with
#' [stats::qbeta()] directly because `stats::binom.test()` and the Wilson
#' function refuse fractional counts. Its bounds stay inside the share: the
#' upper limit of a share never passes its whole.
#'
#' @param x Numeric scalar >= 0. Successes or effective successes.
#' @param n Numeric scalar > 0. Trials or effective trials, >= x.
#' @param scale Numeric scalar. Multiplier for the bounds and proportion.
#'   Default 1; use 100 for percentages.
#' @param conf.level Numeric scalar. Default 0.95.
#'
#' @returns A named list: `proportion`, `lower`, `upper`.
#'
#' @noRd
.clopper_pearson_ci <- function(x, n,
                                scale = 1,
                                conf.level = 0.95) {
  check_number_decimal(x, min = 0)
  check_number_decimal(n, min = .Machine$double.eps)
  if (x > n) rlang::abort(gettextf("%s must be <= %s.", "`x`", "`n`"))
  check_number_decimal(scale, min = .Machine$double.eps)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  alpha <- 1 - conf.level
  lower <- if (x == 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1)
  upper <- if (x == n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x)
  list(
    proportion = x / n * scale,
    lower      = lower * scale,
    upper      = upper * scale
  )
}

#' Design effect from the seven steps of the API design
#'
#' The bias-reduced cluster-robust (CR2) variance of Bell and McCaffrey over
#' the Poisson or binomial variance of the pooled figure, at least 1 and
#' widened for its Bell-McCaffrey degrees of freedom, as the API design's
#' §13.1.4 gives the steps: the DerSimonian-Laird moment estimator of the
#' variance between the clusters, the working variance of each cluster's own
#' figure, the ratio estimator's leverage, the CR2 variance in
#' Pustejovsky and Tipton's weighted form, and the design effect times
#' (t / 1.96)^2 on those degrees of freedom. Closed form: every step from
#' sums over the clusters, no matrix built. With clusters as the departments
#' it gives the network's design effect of a pooled figure; with a
#' department's patients as the clusters, the department's own.
#'
#' @param y Numeric vector >= 0. Each cluster's events (effective counts
#'   allowed).
#' @param w Numeric vector > 0. Each cluster's denominator, same length.
#' @param family `"poisson"` for rates and ratios, `"binomial"` for shares.
#' @param conf.level Numeric scalar. The t quantile and the normal quantile
#'   the widening compares follow it. Default 0.95.
#'
#' @returns A named list: `status` (`"ok"`, `"few_clusters"`,
#'   `"zero_events"`, `"full_share"`, `"share_exceeded"` or
#'   `"not_estimable"`), `effect`, `df`,
#'   `variance` and `tau2` (the three `NA` unless `status` is `"ok"`), and
#'   the pooled figure `pooled`.
#'
#' @noRd
.design_effect <- function(y, w,
                           family = c("poisson", "binomial"),
                           conf.level = 0.95) {
  family <- rlang::arg_match(family)
  check_number_decimal(conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)
  if (length(y) != length(w)) {
    rlang::abort(gettextf("%s and %s must have the same length.", "`y`", "`w`"))
  }
  if (!is.numeric(y) || !is.numeric(w)) {
    rlang::abort(gettextf("%s and %s must be numeric vectors.", "`y`", "`w`"))
  }
  if (length(y) == 0L) {
    rlang::abort(gettextf("%s and %s must not be empty.", "`y`", "`w`"))
  }
  if (any(!is.finite(y)) || any(!is.finite(w))) {
    rlang::abort(gettextf("%s and %s must be finite.", "`y`", "`w`"))
  }
  if (any(y < 0)) rlang::abort(gettextf("%s must be non-negative.", "`y`"))
  if (any(w <= 0)) rlang::abort(gettextf("%s must be strictly positive.", "`w`"))

  G <- length(y)
  S <- sum(w)
  Y <- sum(y)
  R <- Y / S
  if (G < 2L) return(.degenerate_effect("few_clusters"))
  if (Y == 0) return(.degenerate_effect("zero_events"))
  if (family == "binomial") {
    if (any(y > w)) return(.degenerate_effect("share_exceeded"))
    if (R == 1) return(.degenerate_effect("full_share"))
  }

  b <- if (family == "poisson") R else R * (1 - R)
  e <- .cr2_effect(y, w, b, conf.level = conf.level)
  if (is.nan(e$effect) || is.nan(e$df) || is.nan(e$variance)) {
    # Denominators small enough for the working variance b / w[i] to overflow
    # leave the traces without a value; the seven steps estimate nothing here.
    return(.degenerate_effect("not_estimable"))
  }
  list(status = "ok", effect = e$effect, df = e$df,
       variance = e$variance, tau2 = e$tau2, pooled = R)
}

#' The result shape of the engines when no design effect can be estimated
#'
#' @param status One of `few_clusters`, `zero_events`, `full_share`,
#'   `share_exceeded` or `not_estimable`.
#'
#' @returns A named list with `effect`, `df`, `variance`, `tau2` and `pooled`
#'   all `NA`.
#'
#' @noRd
.degenerate_effect <- function(status) {
  list(status = status, effect = NA_real_, df = NA_real_,
       variance = NA_real_, tau2 = NA_real_, pooled = NA_real_)
}

#' The design effect from the CR2 core of one working variance
#'
#' The widening of step 6, shared by the count families and the mean: the
#' CR2 variance over the working variance `b` of the pooled figure, at least
#' 1 and times (t / 1.96)^2 on the Bell-McCaffrey degrees of freedom. Where
#' both variances are 0, as when every value is equal, the effect is 1;
#' where `b` is 0 but the CR2 variance is not, as when the values within
#' every cluster are equal but the clusters' means differ, the effect is
#' unbounded.
#'
#' @param y Numeric vector >= 0. Each cluster's events or totals.
#' @param w Numeric vector > 0. Each cluster's denominator, same length.
#' @param b Numeric scalar >= 0. The working variance parameter of the
#'   pooled figure: cluster `i` carries `b / w[i]`.
#' @param conf.level Numeric scalar. Default 0.95.
#'
#' @returns A named list: `effect`, `df`, `variance` and `tau2`.
#'
#' @noRd
.cr2_effect <- function(y, w, b, conf.level = 0.95) {
  G <- length(y)
  S <- sum(w)
  core <- .cr2_core(y, w, b, S, G)
  if (b == 0 && core$v == 0) {
    return(list(effect = 1, df = NA_real_, variance = 0, tau2 = core$tau2))
  }
  t975 <- stats::qt(1 - (1 - conf.level) / 2, df = core$df)
  widened <- (t975 / stats::qnorm(1 - (1 - conf.level) / 2))^2
  list(effect = max(1, core$v * S / b) * widened, df = core$df,
       variance = core$v, tau2 = core$tau2)
}

#' CR2 core of the seven steps, shared by the count and mean engines
#'
#' With the working variance of each cluster's own figure `b / w[i] + tau2`,
#' the DerSimonian-Laird moment estimator for `tau2`, the leverage rows
#' `1 - w / S` and the CR2 variance `sum(u^2 * phi / M) / S^2`. The trace
#' terms of the Bell-McCaffrey degrees of freedom come from power sums over
#' the clusters: with `B = diag(c) - c h^T - h c^T + C0 h h^T` for the
#' leverage `h = w / S` and `c = h^2 phi / M`, the sandwich `P` equals
#' `diag(sqrt(phi)) B diag(sqrt(phi))`, so `tr P = sum(c phi (1 - 2h)) +
#' C0 sum(phi h^2)` and `tr P^2` is the squared Frobenius norm of `P`,
#' written out below.
#'
#' @param y Numeric vector >= 0. Each cluster's events or totals.
#' @param w Numeric vector > 0. Each cluster's denominator, same length.
#' @param b Numeric scalar >= 0. The working variance parameter, cluster `i`
#'   carrying `b / w[i]`.
#' @param S Numeric scalar. `sum(w)`, the pooled denominator.
#' @param G Integer. `length(y)`, the number of clusters.
#'
#' @returns A named list: `v`, the CR2 variance; `tau2`; `df`, the
#'   Bell-McCaffrey degrees of freedom; and the traces `trP` and `trP2`.
#'
#' @noRd
.cr2_core <- function(y, w, b, S, G) {
  R <- sum(y) / S
  u <- y - R * w
  tau2 <- max(0, (sum(y^2 / w) - sum(y)^2 / S - (G - 1) * b) /
                 (S - sum(w^2) / S))
  phi <- b / w + tau2
  h <- w / S
  if (all(phi == 0)) {
    # No working variance and no variance between the clusters: every value
    # equal, the variance of the pooled figure is 0 with no degrees of
    # freedom, and the design effect stands at 1.
    return(list(v = 0, tau2 = tau2, df = NA_real_, trP = 0, trP2 = 0))
  }
  Phi <- sum(h^2 * phi)
  mii <- phi * (1 - 2 * h) + Phi
  v <- sum(u^2 * phi / mii) / S^2
  cc <- h^2 * phi / mii
  C0 <- sum(cc)
  trP <- sum(cc * phi * (1 - 2 * h)) + C0 * sum(phi * h^2)
  # tr P^2: the diagonal of P^2 summed over every pair of clusters, the
  # off-diagonal squares of B expanded into the power sums below.
  gii <- -2 * cc * h + C0 * h^2
  trP2 <- sum(phi^2 * (cc + gii)^2) +
    2 * (sum(phi * cc^2) * sum(phi * h^2) - sum(phi^2 * cc^2 * h^2)) +
    2 * (sum(phi * cc * h)^2 - sum(phi^2 * cc^2 * h^2)) -
    4 * C0 * (sum(phi * cc * h) * sum(phi * h^2) - sum(phi^2 * cc * h^3)) +
    C0^2 * (sum(phi * h^2)^2 - sum(phi^2 * h^4))
  df <- trP^2 / trP2
  list(v = v, tau2 = tau2, df = df, trP = trP, trP2 = trP2)
}

#' Design effect for a department's mean days, for the flag alone
#'
#' The flag on the pooled mean days per exposed patient divides the CR2
#' variance of the pooled mean, with the departments as clusters, by its
#' variance with the patients as clusters: the steps of [.design_effect()]
#' with the working variance `b / n[i]` for `b` the pooled within-department
#' variance of the days. Where both variances are 0, as when every exposed
#' patient has the same days, the design effect is 1; where the values
#' within every department are equal but the departments' means differ, the
#' variance with the patients as clusters is 0 while the CR2 variance is
#' not, and the design effect is unbounded.
#'
#' @param clusters List of numeric vectors, the values of one department
#'   each.
#' @param conf.level Numeric scalar. Default 0.95.
#'
#' @returns The same shape as [.design_effect()].
#'
#' @noRd
.design_effect_mean <- function(clusters, conf.level = 0.95) {
  if (!is.list(clusters) || !all(vapply(clusters, is.numeric, logical(1)))) {
    rlang::abort(gettextf("%s must be a list of numeric vectors.", "`clusters`"))
  }
  if (any(vapply(clusters, function(x) !all(is.finite(x)), logical(1)))) {
    rlang::abort(gettextf("%s must hold only finite values.", "`clusters`"))
  }
  G <- length(clusters)
  if (G < 2L) return(.degenerate_effect("few_clusters"))
  ns <- vapply(clusters, length, integer(1))
  if (any(ns < 1L)) {
    rlang::abort(gettextf("%s must hold at least one value per cluster.", "`clusters`"))
  }
  totals <- vapply(clusters, sum, numeric(1))
  if (sum(totals) == 0) return(.degenerate_effect("zero_events"))
  N <- sum(ns)
  M <- sum(totals) / N
  within_ss <- sum(vapply(clusters, function(x) sum((x - mean(x))^2), numeric(1)))
  b <- if (N > G) within_ss / (N - G) else 0
  e <- .cr2_effect(totals, as.numeric(ns), b, conf.level = conf.level)
  if (is.nan(e$effect) || is.nan(e$df) || is.nan(e$variance)) {
    return(.degenerate_effect("not_estimable"))
  }
  list(status = "ok", effect = e$effect, df = e$df,
       variance = e$variance, tau2 = e$tau2, pooled = M)
}

#' Kish's effective number of a department's patients
#'
#' (sum m)^2 / sum m^2 over the patients' days at risk - patient-days for a
#' share of patient-days, substance-days for the Access share. It is the
#' effective number the day-clustered design effect gives when every patient
#' was exposed on all or none of its days, the worst case for the clustering
#' within patients where exposure is unrelated to the length of stay.
#'
#' @param m Numeric vector >= 0. Each patient's days.
#'
#' @returns A numeric scalar.
#'
#' @noRd
.kish_effective_number <- function(m) {
  if (!is.numeric(m) || length(m) == 0L || any(!is.finite(m))) {
    rlang::abort(gettextf("%s must be a non-empty finite numeric vector.", "`m`"))
  }
  if (any(m < 0)) rlang::abort(gettextf("%s must be non-negative.", "`m`"))
  if (sum(m) == 0) {
    rlang::abort(gettextf("%s must hold at least one positive value.", "`m`"))
  }
  sum(m)^2 / sum(m^2)
}

#' Whether degrees of freedom rest below 8
#'
#' Equal denominators give exactly G - 1 degrees of freedom only in exact
#' arithmetic, so the comparison with 8 happens after rounding to 6 decimal
#' places, as the API design's §13.1.4 states.
#'
#' @param df Numeric scalar or `NA`.
#'
#' @returns A logical scalar.
#'
#' @noRd
.df_below8 <- function(df) {
  !is.na(df) && round(df, 6) < 8
}

#' The design effect a department's figure takes
#'
#' The choice of §13.1.4 item 4: a share whose denominator counts the
#' department's patients once takes 1, since its patients are its clusters
#' and leave no clustering to correct for. A figure of 0, a share of 100%, a
#' figure from a single patient and one whose own design effect rests below 8
#' degrees of freedom take the network's where a reference stores it, and
#' otherwise the fallback for rows without a reference: Kish's effective
#' number of the patients for a row that counts days, an effective count
#' equal to the number of patients for the mean days per exposed patient and
#' the operations per patient operated on, and 1 otherwise. A figure of 2 or
#' more patients with a design effect of its own, without a reference, takes
#' the larger of that fallback and its own, so that no interval falls below
#' the figure's own estimate.
#'
#' @param own Named list `list(effect, df)` or `NULL` where the figure has
#'   no design effect of its own.
#' @param network Numeric scalar or `NULL`. The reference's design effect
#'   for the row and the department dataset's birth-weight class, already
#'   fallen back to the reference's whole range where it stores none for the
#'   class.
#' @param has_reference Logical. Whether a reference dataset feeds this
#'   figure.
#' @param counts_days Logical. Whether the row counts days over
#'   patient-days (a share of days, an AWaRe or ATC group's use rate).
#' @param effective_count_patients Logical. Whether the measure takes an
#'   effective count equal to its number of patients (mean days per exposed
#'   patient, operations per patient operated on).
#' @param counts_patients_once Logical. Whether the denominator counts each
#'   patient once.
#' @param is_zero,is_full_share Logical. Whether the figure is a 0 or a
#'   share of 100%.
#' @param denominator,numerator Numeric scalars. The figure's raw totals.
#' @param n_patients Integer. The department's patients with a positive
#'   denominator of the row.
#' @param kish Numeric scalar or `NULL`. The department's Kish effective
#'   number of its patients, required when `counts_days` is `TRUE`.
#'
#' @returns A named list: `effect`, the design effect the figure takes; `df`,
#'   the degrees of freedom of that effect (a number only with `source`
#'   `"own"`, `NA` otherwise); `own_df`, the degrees of freedom of the
#'   figure's own design effect as given, which the flag of §13.1.2 tests
#'   whatever effect the figure takes; and `source` (`"own"`, `"network"`,
#'   `"fallback"` or `"patients_once"`).
#'
#' @noRd
.department_design_effect <- function(own = NULL,
                                      network = NULL,
                                      has_reference = TRUE,
                                      counts_days = FALSE,
                                      effective_count_patients = FALSE,
                                      counts_patients_once = FALSE,
                                      is_zero = FALSE,
                                      is_full_share = FALSE,
                                      denominator = 0,
                                      numerator = 0,
                                      n_patients = 0,
                                      kish = NULL) {
  own_df <- if (!is.null(own)) own$df else NA_real_
  if (counts_patients_once) {
    return(list(effect = 1, df = NA_real_, own_df = own_df,
                source = "patients_once"))
  }
  own_ok <- !is.null(own) && !is_zero && !is_full_share &&
    !is.na(own$effect) && !is.na(own$df)
  if (own_ok && !.df_below8(own$df)) {
    return(list(effect = own$effect, df = own$df, own_df = own_df,
                source = "own"))
  }
  if (has_reference && !is.null(network)) {
    return(list(effect = network, df = NA_real_, own_df = own_df,
                source = "network"))
  }
  fallback <- if (counts_days) {
    if (is.null(kish)) {
      rlang::abort(gettextf("%s is required for a row that counts days.", "`kish`"))
    }
    denominator / kish
  } else if (effective_count_patients) {
    numerator / max(n_patients, 1L)
  } else {
    1
  }
  if (own_ok && n_patients >= 2 && own$effect > fallback) {
    return(list(effect = own$effect, df = own$df, own_df = own_df,
                source = "own"))
  }
  list(effect = fallback, df = NA_real_, own_df = own_df, source = "fallback")
}

#' The flags that tell how much an interval carries
#'
#' The sparse-data flag marks a figure whose effective count, effective
#' count of days or effective denominator falls below the threshold - below
#' 16 independent events a count's relative standard error exceeds 25% -
#' and always marks a figure that has no interval: a pooled 0, a pooled
#' share of 100%, a pooled figure from fewer than 2 departments and a share
#' whose days exceed its denominator. The degrees-of-freedom flag marks a
#' figure whose own design effect, or a pooled interval, rests below 8
#' degrees of freedom. The few-patients flag marks a department's figure to
#' which fewer than 9 patients contribute a positive denominator of the row,
#' whatever design effect it takes; a share whose denominator counts the
#' patients once carries the flag on its effective denominator alone, which
#' already falls below 16 with fewer than 16 patients.
#'
#' @param effective_value Numeric scalar or `NULL`. The count, days or
#'   denominator divided by the design effect the figure's interval takes.
#' @param df Numeric scalar or `NULL`. The degrees of freedom the flag
#'   tests: a department figure's own ones, `own_df` of
#'   [.department_design_effect()], or, for a pooled figure, the pooled
#'   interval's.
#' @param n_patients Integer or `NULL`. The department's patients
#'   contributing a positive denominator of the row.
#' @param counts_patients_once Logical. As in
#'   [.department_design_effect()].
#' @param no_interval Logical. Whether the figure has no interval.
#' @param threshold Numeric scalar. The sparse-data threshold, configurable,
#'   16 by default.
#'
#' @returns A named list of logicals: `sparse_data`, `df_low`,
#'   `few_patients`.
#'
#' @noRd
.figure_flags <- function(effective_value = NULL,
                          df = NULL,
                          n_patients = NULL,
                          counts_patients_once = FALSE,
                          no_interval = FALSE,
                          threshold = 16) {
  check_number_decimal(threshold, min = .Machine$double.eps)
  if (!is.null(n_patients)) {
    check_number_whole(n_patients, min = 0)
  }
  sparse_data <- isTRUE(no_interval) ||
    (!is.null(effective_value) && !is.na(effective_value) && effective_value < threshold)
  df_low <- !is.null(df) && .df_below8(df)
  few_patients <- !is.null(n_patients) && !counts_patients_once && n_patients < 9
  list(sparse_data = sparse_data, df_low = df_low, few_patients = few_patients)
}

#' Cut a lower limit at its measure's minimum
#'
#' No lower limit prints below its measure's minimum - 0, but 1 for the
#' operations per patient operated on and the mean days per exposed patient,
#' and 100 for the infectious agents per 100 confirmed infections - since the
#' true value never lies below it; the cut keeps every interval's coverage.
#' An open limit stays `NA` here: it prints as the minimum by the report's
#' rule for open limits.
#'
#' @param x Numeric vector. Lower limits, `NA` for open.
#' @param minimum Numeric scalar. The measure's minimum, from
#'   [.measure_minimum()].
#'
#' @returns A numeric vector.
#'
#' @noRd
.cut_ci_lower <- function(x, minimum) {
  check_number_decimal(minimum, min = -Inf)
  ifelse(is.na(x), NA_real_, base::pmax(x, minimum))
}

#' The minimum of a measure
#'
#' Measures in which every unit of the denominator holds at least 1 of the
#' numerator: the operations per patient operated on and the mean days per
#' exposed patient at 1, the infectious agents per 100 confirmed infections
#' at 100. Everything that can be 0 is `"rate"` at 0.
#'
#' @param measure One of `"rate"`, `"operations_per_patient"`,
#'   `"mean_days_exposed"` or `"agents_per_infection"`.
#'
#' @returns A numeric scalar.
#'
#' @noRd
.measure_minimum <- function(measure) {
  switch(rlang::arg_match(measure,
                          c("rate", "operations_per_patient",
                            "mean_days_exposed", "agents_per_infection")),
         rate = 0,
         operations_per_patient = 1,
         mean_days_exposed = 1,
         agents_per_infection = 100)
}
