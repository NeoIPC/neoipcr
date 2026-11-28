# --- neoipc_poisson_ci() ---------------------------------------------------

test_that("neoipc_poisson_ci returns correct structure", {
  result <- neoipc_poisson_ci(47, 28904)
  expect_type(result, "list")
  expect_named(result, c("rate", "lower", "upper"))
  expect_true(is.numeric(result$rate))
  expect_true(is.numeric(result$lower))
  expect_true(is.numeric(result$upper))
})

test_that("neoipc_poisson_ci matches known poisson.test result", {
  # Verify against direct poisson.test() call
  pt <- stats::poisson.test(47, T = 28904, conf.level = 0.95)
  result <- neoipc_poisson_ci(47, 28904, multiplier = 1000)
  expect_equal(result$rate, 47 / 28904 * 1000)
  expect_equal(result$lower, pt$conf.int[1] * 1000)
  expect_equal(result$upper, pt$conf.int[2] * 1000)
})

test_that("neoipc_poisson_ci handles zero events", {
  result <- neoipc_poisson_ci(0, 5000, multiplier = 1000)
  expect_equal(result$rate, 0)
  expect_equal(result$lower, 0)
  expect_true(result$upper > 0)
})

test_that("neoipc_poisson_ci handles single event", {
  result <- neoipc_poisson_ci(1, 10000, multiplier = 1000)
  expect_equal(result$rate, 0.1)
  expect_true(result$lower < result$rate)
  expect_true(result$upper > result$rate)
})

test_that("neoipc_poisson_ci respects multiplier", {
  r1000 <- neoipc_poisson_ci(50, 10000, multiplier = 1000)
  r100  <- neoipc_poisson_ci(50, 10000, multiplier = 100)
  r1    <- neoipc_poisson_ci(50, 10000, multiplier = 1)
  expect_equal(r1000$rate, r100$rate * 10)
  expect_equal(r1000$rate, r1$rate * 1000)
  expect_equal(r1000$lower, r100$lower * 10)
  expect_equal(r1000$upper, r100$upper * 10)
})

test_that("neoipc_poisson_ci respects conf.level", {
  ci95 <- neoipc_poisson_ci(47, 28904)
  ci99 <- neoipc_poisson_ci(47, 28904, conf.level = 0.99)
  # 99% CI must be wider than 95% CI
  expect_true(ci99$lower < ci95$lower)
  expect_true(ci99$upper > ci95$upper)
})

test_that("neoipc_poisson_ci CI ordering is consistent", {
  result <- neoipc_poisson_ci(3, 10, multiplier = 1000)
  expect_true(result$lower <= result$rate)
  expect_true(result$upper >= result$rate)
})

test_that("neoipc_poisson_ci validates inputs", {
  expect_error(neoipc_poisson_ci(-1, 100))
  expect_error(neoipc_poisson_ci(2.5, 100))
  expect_error(neoipc_poisson_ci(5, 0))
  expect_error(neoipc_poisson_ci(5, -10))
  expect_error(neoipc_poisson_ci("a", 100))
})

test_that("neoipc_poisson_ci errors on zero exposure", {
  expect_error(neoipc_poisson_ci(0, 0))
})

# --- neoipc_wilson_ci() ----------------------------------------------------

test_that("neoipc_wilson_ci returns correct structure", {
  result <- neoipc_wilson_ci(220, 283)
  expect_type(result, "list")
  expect_named(result, c("proportion", "lower", "upper"))
  expect_true(is.numeric(result$proportion))
  expect_true(is.numeric(result$lower))
  expect_true(is.numeric(result$upper))
})

test_that("neoipc_wilson_ci matches known Wilson formula result", {
  # x = 220, n = 283 — verify against hand-computed Wilson score
  z <- stats::qnorm(0.975)
  p_hat <- 220 / 283
  denom <- 1 + z^2 / 283
  center <- (p_hat + z^2 / (2 * 283)) / denom
  margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * 283)) / 283) / denom
  expected_lower <- center - margin
  expected_upper <- center + margin

  result <- neoipc_wilson_ci(220, 283)
  expect_equal(result$proportion, 220 / 283)
  expect_equal(result$lower, expected_lower)
  expect_equal(result$upper, expected_upper)
})

test_that("neoipc_wilson_ci handles zero successes", {
  result <- neoipc_wilson_ci(0, 50)
  expect_equal(result$proportion, 0)
  # Wilson score: when x = 0, margin = center exactly, so lower = 0
  expect_equal(result$lower, 0)
  expect_true(result$upper > 0)
})

test_that("neoipc_wilson_ci handles all successes", {
  result <- neoipc_wilson_ci(50, 50)
  expect_equal(result$proportion, 1)
  # Wilson score: when x = n, center + margin = 1 exactly
  expect_equal(result$upper, 1)
  expect_true(result$lower < 1)
})

test_that("neoipc_wilson_ci handles small sample", {
  result <- neoipc_wilson_ci(1, 3)
  expect_equal(result$proportion, 1 / 3)
  expect_true(result$lower < result$proportion)
  expect_true(result$upper > result$proportion)
  # Wide CI for small sample
  expect_true((result$upper - result$lower) > 0.3)
})

test_that("neoipc_wilson_ci handles n = 1 edge cases", {
  r0 <- neoipc_wilson_ci(0, 1)
  expect_equal(r0$proportion, 0)
  expect_true(r0$upper > 0)

  r1 <- neoipc_wilson_ci(1, 1)
  expect_equal(r1$proportion, 1)
  expect_true(r1$lower < 1)
})

test_that("neoipc_wilson_ci CI ordering is consistent", {
  result <- neoipc_wilson_ci(15, 100)
  expect_true(result$lower <= result$proportion)
  expect_true(result$upper >= result$proportion)
})

test_that("neoipc_wilson_ci respects conf.level", {
  ci95 <- neoipc_wilson_ci(100, 200)
  ci99 <- neoipc_wilson_ci(100, 200, conf.level = 0.99)
  expect_true(ci99$lower < ci95$lower)
  expect_true(ci99$upper > ci95$upper)
})

test_that("neoipc_wilson_ci validates inputs", {
  expect_error(neoipc_wilson_ci(10, 5))   # x > n
  expect_error(neoipc_wilson_ci(-1, 10))  # negative x
  expect_error(neoipc_wilson_ci(0, 0))    # zero n
  expect_error(neoipc_wilson_ci(5, -1))   # negative n
  expect_error(neoipc_wilson_ci("a", 10)) # non-numeric
})

# --- poisson_ci_cols() (internal vectorized wrapper) ------------------------

test_that("poisson_ci_cols returns correct structure", {
  result <- neoipcr:::poisson_ci_cols(c(10, 20), c(1000, 2000), multiplier = 1000)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ci_lower", "ci_upper"))
  expect_equal(nrow(result), 2)
})

test_that("poisson_ci_cols handles zero events with valid exposure", {
  result <- neoipcr:::poisson_ci_cols(c(0, 5), c(1000, 1000), multiplier = 1000)
  expect_equal(result$ci_lower[1], 0)
  expect_true(result$ci_upper[1] > 0)
  expect_true(result$ci_lower[2] > 0)
})

test_that("poisson_ci_cols returns NA for NA/zero-exposure inputs", {
  result <- neoipcr:::poisson_ci_cols(c(5, NA, 0, 3), c(1000, 1000, 0, NA),
                                       multiplier = 1000)
  expect_equal(nrow(result), 4)
  expect_false(is.na(result$ci_lower[1]))
  expect_true(is.na(result$ci_lower[2]))
  expect_true(is.na(result$ci_lower[3]))
  expect_true(is.na(result$ci_lower[4]))
})

test_that("poisson_ci_cols handles scalar exposure (recycled)", {
  result <- neoipcr:::poisson_ci_cols(c(5, 10, 15), 1000, multiplier = 1000)
  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$ci_lower)))
})

# --- wilson_ci_cols() (internal vectorized wrapper) -------------------------

test_that("wilson_ci_cols returns correct structure", {
  result <- neoipcr:::wilson_ci_cols(c(10, 20), c(50, 100), scale = 100)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ci_lower", "ci_upper"))
  expect_equal(nrow(result), 2)
})

test_that("wilson_ci_cols applies scale correctly", {
  raw <- neoipcr:::wilson_ci_cols(c(10), c(50), scale = 1)
  scaled <- neoipcr:::wilson_ci_cols(c(10), c(50), scale = 100)
  expect_equal(scaled$ci_lower, raw$ci_lower * 100)
  expect_equal(scaled$ci_upper, raw$ci_upper * 100)
})

test_that("wilson_ci_cols handles zero successes with valid n", {
  result <- neoipcr:::wilson_ci_cols(c(0, 10), c(50, 50), scale = 100)
  expect_equal(result$ci_lower[1], 0)
  expect_true(result$ci_upper[1] > 0)
})

test_that("wilson_ci_cols returns NA for NA/zero-n inputs", {
  result <- neoipcr:::wilson_ci_cols(c(5, NA, 0), c(50, 50, 0), scale = 100)
  expect_equal(nrow(result), 3)
  expect_false(is.na(result$ci_lower[1]))
  expect_true(is.na(result$ci_lower[2]))
  expect_true(is.na(result$ci_lower[3]))
})

test_that("wilson_ci_cols passes conf.level through to the interval", {
  ci95 <- neoipcr:::wilson_ci_cols(10, 50, scale = 100)
  ci99 <- neoipcr:::wilson_ci_cols(10, 50, scale = 100, conf.level = 0.99)
  expect_true(ci99$ci_lower < ci95$ci_lower)
  expect_true(ci99$ci_upper > ci95$ci_upper)
})

test_that("poisson_ci_cols returns 0x2 tibble for empty input", {
  result <- neoipcr:::poisson_ci_cols(integer(), numeric(), 1000)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ci_lower", "ci_upper"))
  expect_equal(nrow(result), 0L)
})

test_that("wilson_ci_cols returns 0x2 tibble for empty input", {
  result <- neoipcr:::wilson_ci_cols(integer(), integer())
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ci_lower", "ci_upper"))
  expect_equal(nrow(result), 0L)
})

test_that("poisson_ci_cols CI ordering is consistent", {
  events <- c(0, 1, 5, 50, 200)
  exposure <- c(1000, 1000, 1000, 1000, 1000)
  result <- neoipcr:::poisson_ci_cols(events, exposure, multiplier = 1000)
  rates <- events / exposure * 1000
  for (i in seq_along(events)) {
    expect_true(result$ci_lower[i] <= rates[i])
    expect_true(result$ci_upper[i] >= rates[i])
  }
})

# --- bootstrap_quantile_ci() -----------------------------------------------

test_that("bootstrap_quantile_ci returns correct structure", {
  result <- bootstrap_quantile_ci(
    c(20, 15, 30, 25, 18, 22),
    c(1000, 800, 1200, 900, 700, 1100),
    type = "poisson", multiplier = 1000)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_named(result, c("q1_ci_lower", "q1_ci_upper",
                          "q2_ci_lower", "q2_ci_upper",
                          "q3_ci_lower", "q3_ci_upper"))
})

test_that("bootstrap_quantile_ci is deterministic with fixed seed", {
  args <- list(events = c(20, 15, 30, 25, 18), exposure = c(1000, 800, 1200, 900, 700),
               type = "poisson", multiplier = 1000, seed = 123)
  r1 <- do.call(bootstrap_quantile_ci, args)
  r2 <- do.call(bootstrap_quantile_ci, args)
  expect_equal(r1, r2)
})

test_that("bootstrap_quantile_ci differs with different seeds", {
  args <- list(events = c(20, 15, 30, 25, 18), exposure = c(1000, 800, 1200, 900, 700),
               type = "poisson", multiplier = 1000)
  r1 <- do.call(bootstrap_quantile_ci, c(args, seed = 1))
  r2 <- do.call(bootstrap_quantile_ci, c(args, seed = 2))
  expect_false(identical(r1, r2))
})

test_that("bootstrap_quantile_ci contains observed quartiles for large counts", {
  # Large counts: Jeffreys prior has negligible influence, containment holds
  events <- c(50, 40, 80, 60, 35, 70, 55, 45)
  exposure <- rep(1000, 8)
  rates <- events / exposure * 1000
  observed_q <- stats::quantile(rates, probs = c(0.25, 0.5, 0.75), names = FALSE)
  result <- bootstrap_quantile_ci(events, exposure, type = "poisson",
                                   multiplier = 1000, B = 5000)
  expect_true(result$q1_ci_lower <= observed_q[1])
  expect_true(result$q1_ci_upper >= observed_q[1])
  expect_true(result$q2_ci_lower <= observed_q[2])
  expect_true(result$q2_ci_upper >= observed_q[2])
  expect_true(result$q3_ci_lower <= observed_q[3])
  expect_true(result$q3_ci_upper >= observed_q[3])
})

test_that("bootstrap_quantile_ci handles zero-event departments (Jeffreys prior)", {
  # Several zero-count departments: Jeffreys prior produces non-degenerate CIs
  # The bootstrap CI may NOT contain the point-estimate Q1 (which is 0.0) —
  # this is expected behaviour: the prior propagates uncertainty that the
  # point estimate ignores.
  events <- c(0, 0, 0, 5, 10, 8)
  exposure <- c(1000, 800, 1200, 900, 700, 1100)
  result <- bootstrap_quantile_ci(events, exposure, type = "poisson",
                                   multiplier = 1000, B = 2000)
  # CI width should be > 0 (non-degenerate)
  expect_true(result$q1_ci_upper > result$q1_ci_lower)
  expect_true(result$q2_ci_upper > result$q2_ci_lower)
})

test_that("bootstrap_quantile_ci works for binomial type", {
  result <- bootstrap_quantile_ci(
    c(15, 8, 22, 5, 18, 30),
    c(20, 10, 25, 8, 20, 35),
    type = "binomial", multiplier = 100)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  # Proportions scaled by 100: CIs should be in 0-100 range
  expect_true(result$q1_ci_lower >= 0)
  expect_true(result$q3_ci_upper <= 100)
})

test_that("bootstrap_quantile_ci conf.level = 0.99 gives wider CIs", {
  args <- list(events = c(20, 15, 30, 25, 18, 22),
               exposure = c(1000, 800, 1200, 900, 700, 1100),
               type = "poisson", multiplier = 1000)
  ci95 <- do.call(bootstrap_quantile_ci, c(args, conf.level = 0.95))
  ci99 <- do.call(bootstrap_quantile_ci, c(args, conf.level = 0.99))
  expect_true(ci99$q2_ci_upper - ci99$q2_ci_lower >=
              ci95$q2_ci_upper - ci95$q2_ci_lower)
})

test_that("bootstrap_quantile_ci validates inputs", {
  expect_error(bootstrap_quantile_ci(c(-1, 5), c(100, 100)))
  expect_error(bootstrap_quantile_ci(c(5, 5), c(0, 100)))
  expect_error(bootstrap_quantile_ci(c(5, 5), c(100)))
  expect_error(bootstrap_quantile_ci(c(10, 5), c(5, 10), type = "binomial"))
})

test_that("bootstrap_quantile_ci does not corrupt global RNG state", {
  set.seed(999)
  expected <- stats::runif(1)
  set.seed(999)
  bootstrap_quantile_ci(c(5, 10), c(100, 100), type = "poisson", seed = 42)
  actual <- stats::runif(1)
  expect_equal(actual, expected)
})

# --- .order_statistic_quartile_ci() -------------------------------------------

test_that(".order_statistic_quartile_ci takes the decided ranks at 5, 15 and 30 departments", {
  # The rank table of the API design's §13.1.4 item 1, on strictly
  # increasing rates: the r-th order statistic is the r-th value.
  res5 <- neoipcr:::.order_statistic_quartile_ci(1:5, rep(1, 5))
  expect_equal(res5$q1_ci_lower, NA_real_)
  expect_true(res5$q1_ci_lower_open)
  expect_equal(res5$q1_ci_upper, 4)
  expect_false(res5$q1_ci_upper_open)
  expect_equal(res5$q2_ci_lower, NA_real_)
  expect_true(res5$q2_ci_lower_open)
  expect_equal(res5$q2_ci_upper, NA_real_)
  expect_true(res5$q2_ci_upper_open)
  expect_equal(res5$q3_ci_lower, 2)
  expect_false(res5$q3_ci_lower_open)
  expect_equal(res5$q3_ci_upper, NA_real_)
  expect_true(res5$q3_ci_upper_open)
  expect_equal(res5$entering_departments, 5L)

  res15 <- neoipcr:::.order_statistic_quartile_ci(1:15, rep(1, 15))
  expect_true(res15$q1_ci_lower_open)
  expect_equal(res15$q1_ci_upper, 8)
  expect_equal(res15$q2_ci_lower, 4)
  expect_equal(res15$q2_ci_upper, 12)
  expect_equal(res15$q3_ci_lower, 8)
  expect_true(res15$q3_ci_upper_open)

  res30 <- neoipcr:::.order_statistic_quartile_ci(1:30, rep(1, 30))
  expect_false(res30$q1_ci_lower_open)
  expect_equal(res30$q1_ci_lower, 3)
  expect_equal(res30$q1_ci_upper, 13)
  expect_equal(res30$q2_ci_lower, 10)
  expect_equal(res30$q2_ci_upper, 21)
  expect_equal(res30$q3_ci_lower, 18)
  expect_equal(res30$q3_ci_upper, 28)
  expect_false(res30$q3_ci_upper_open)
})

test_that(".order_statistic_quartile_ci keeps the open sides the API design names", {
  # Q1's lower and Q3's upper limit close from 20 departments, each limit of
  # Q2 from 9; the ranks before that do not exist or fall on the lowest or
  # highest department, so the side is open.
  for (n in 5:19) {
    res <- neoipcr:::.order_statistic_quartile_ci(seq_len(n), rep(1, n))
    expect_true(res$q1_ci_lower_open, info = paste0("Q1 lower open at n = ", n))
    expect_true(res$q3_ci_upper_open, info = paste0("Q3 upper open at n = ", n))
  }
  res20 <- neoipcr:::.order_statistic_quartile_ci(1:20, rep(1, 20))
  expect_false(res20$q1_ci_lower_open)
  expect_equal(res20$q1_ci_lower, 2)
  expect_false(res20$q3_ci_upper_open)
  expect_equal(res20$q3_ci_upper, 19)

  for (n in 5:8) {
    res <- neoipcr:::.order_statistic_quartile_ci(seq_len(n), rep(1, n))
    expect_true(res$q2_ci_lower_open, info = paste0("Q2 lower open at n = ", n))
    expect_true(res$q2_ci_upper_open, info = paste0("Q2 upper open at n = ", n))
  }
  res9 <- neoipcr:::.order_statistic_quartile_ci(1:9, rep(1, 9))
  expect_equal(res9$q2_ci_lower, 2)
  expect_equal(res9$q2_ci_upper, 8)
})

test_that(".order_statistic_quartile_ci widens when the departments differ", {
  # Same pooled figure and same median, the departments' spread differing:
  # the interval runs between order statistics, so a wider spread of the
  # departments' rates widens it.
  exposure <- rep(1000, 30)
  even <- rep(100, 30)
  spread <- round(seq(20, 180, length.out = 30))
  expect_equal(sum(spread), sum(even))
  res_even <- neoipcr:::.order_statistic_quartile_ci(even, exposure)
  res_spread <- neoipcr:::.order_statistic_quartile_ci(spread, exposure)
  expect_equal(res_even$q2_ci_lower, res_even$q2_ci_upper)
  expect_true((res_spread$q2_ci_upper - res_spread$q2_ci_lower) >
                (res_even$q2_ci_upper - res_even$q2_ci_lower))
  expect_true((res_spread$q1_ci_upper - res_spread$q1_ci_lower) >
                (res_even$q1_ci_upper - res_even$q1_ci_lower))
})

test_that(".order_statistic_quartile_ci reports the Q1 of 0 with the interval [0, 0] and its note", {
  # Eight zero departments of 15: as many as Q1's upper rank, so the printed
  # Q1 is 0 with the interval [0, 0] and the note counts the zeros.
  events <- c(rep(0, 8), rep(50, 7))
  res <- neoipcr:::.order_statistic_quartile_ci(events, rep(1000, 15))
  expect_equal(res$q1_ci_lower, NA_real_)
  expect_true(res$q1_ci_lower_open)
  expect_equal(res$q1_ci_upper, 0)
  expect_true(res$q1_zero_note)
  expect_equal(res$zero_departments, 8L)
  expect_equal(res$entering_departments, 15L)

  # Seven zeros: x(8) is a positive rate, so the interval is not [0, 0] and
  # the note does not apply even though the Q1 is 0.
  events7 <- c(rep(0, 7), rep(50, 8))
  res7 <- neoipcr:::.order_statistic_quartile_ci(events7, rep(1000, 15))
  expect_equal(res7$q1_ci_upper, 50 / 1000)
  expect_false(res7$q1_zero_note)
  expect_equal(res7$zero_departments, 7L)
})

test_that(".order_statistic_quartile_ci scales by the multiplier", {
  # Nine departments: the median's interval [x(2), x(8)], on rates
  # events / exposure * multiplier.
  res <- neoipcr:::.order_statistic_quartile_ci(seq(2, 18, by = 2),
                                                 rep(1000, 9), multiplier = 1000)
  expect_equal(res$q2_ci_lower, 4)
  expect_equal(res$q2_ci_upper, 16)
})

test_that(".order_statistic_quartile_ci drops NA pairs", {
  # Thirteen departments enter and the ranks follow n = 13: Q1's lower limit
  # rests on the lowest rank and stays open, its upper limit is x(7) because
  # P(B <= 6) = 0.9757 already clears the 97.5% bound.
  events <- c(1:13, NA, NA)
  res_na <- neoipcr:::.order_statistic_quartile_ci(events, rep(1, 15))
  expect_equal(res_na$entering_departments, 13L)
  expect_true(res_na$q1_ci_lower_open)
  expect_equal(res_na$q1_ci_upper, 7)
})

test_that(".order_statistic_quartile_ci validates its inputs", {
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(-1, 5), c(100, 100)),
               "non-negative")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(5, 5), c(0, 100)),
               "strictly positive")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(5, 5), c(100)),
               "same length")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(5, NA), c(100, "x")),
               "numeric vector")
})

# --- .gamma_rate_ci() ---------------------------------------------------------

test_that(".gamma_rate_ci equals the exact Poisson interval on whole counts", {
  # Fay and Feuer's interval at a design effect of 1 is the exact (Garwood)
  # Poisson interval, which stats::poisson.test() computes.
  pt <- stats::poisson.test(47, T = 28904, conf.level = 0.95)
  ci <- neoipcr:::.gamma_rate_ci(47, 28904, multiplier = 1000)
  expect_equal(ci$rate, 47 / 28904 * 1000)
  expect_equal(ci$lower, pt$conf.int[1] * 1000)
  expect_equal(ci$upper, pt$conf.int[2] * 1000)
})

test_that(".gamma_rate_ci takes fractional effective counts", {
  # The effective counts are the counts divided by a design effect and are
  # almost never whole; poisson.test() refuses them, qgamma() does not.
  expect_error(stats::poisson.test(2.5, T = 10))
  ci <- neoipcr:::.gamma_rate_ci(47 / 2.5, 28904 / 2.5, multiplier = 1000)
  expect_true(is.finite(ci$lower) && is.finite(ci$upper))
  whole <- neoipcr:::.gamma_rate_ci(47, 28904, multiplier = 1000)
  expect_equal(ci$rate, whole$rate)
  expect_true(ci$lower < whole$lower && ci$upper > whole$upper)

  ci_zero <- neoipcr:::.gamma_rate_ci(0.4, 500, multiplier = 100)
  expect_true(ci_zero$lower >= 0 && ci_zero$upper > ci_zero$rate)
})

test_that(".gamma_rate_ci handles zero events", {
  ci <- neoipcr:::.gamma_rate_ci(0, 5000, multiplier = 1000)
  expect_equal(ci$rate, 0)
  expect_equal(ci$lower, 0)
  expect_equal(ci$upper, stats::qgamma(0.975, shape = 1) / 5000 * 1000)
})

test_that(".gamma_rate_ci validates its inputs", {
  expect_error(neoipcr:::.gamma_rate_ci(-1, 100), "larger than or equal to 0")
  expect_error(neoipcr:::.gamma_rate_ci(5, 0), "not the number 0")
  expect_error(neoipcr:::.gamma_rate_ci(5, -10), "not the number -10")
})

# --- .clopper_pearson_ci() -----------------------------------------------------

test_that(".clopper_pearson_ci equals the exact binomial interval on whole counts", {
  bt <- stats::binom.test(12, 40, conf.level = 0.95)$conf.int
  ci <- neoipcr:::.clopper_pearson_ci(12, 40, scale = 100)
  expect_equal(ci$proportion, 12 / 40 * 100)
  expect_equal(ci$lower, bt[1] * 100)
  expect_equal(ci$upper, bt[2] * 100)
})

test_that(".clopper_pearson_ci takes fractional effective counts", {
  ci <- neoipcr:::.clopper_pearson_ci(12 / 2.5, 40 / 2.5, scale = 1)
  expect_true(is.finite(ci$lower) && is.finite(ci$upper))
  expect_true(ci$lower < 12 / 40 && ci$upper > 12 / 40)
})

test_that(".clopper_pearson_ci keeps a share inside its bounds", {
  lo <- neoipcr:::.clopper_pearson_ci(0, 50, scale = 100)
  expect_equal(lo$lower, 0)
  expect_true(lo$upper > 0 && lo$upper <= 100)
  hi <- neoipcr:::.clopper_pearson_ci(50, 50, scale = 100)
  expect_equal(hi$upper, 100)
  expect_true(hi$lower < 100)
  # Where the exact Poisson rate interval on the same counts would pass the
  # whole, the share's interval stops at it.
  expect_gt(neoipcr:::.gamma_rate_ci(50, 50, multiplier = 100)$upper, 100)
  expect_lte(hi$upper, 100)
})

test_that(".clopper_pearson_ci validates its inputs", {
  expect_error(neoipcr:::.clopper_pearson_ci(30, 20), "must be <=")
  expect_error(neoipcr:::.clopper_pearson_ci(-1, 10), "larger than or equal to 0")
  expect_error(neoipcr:::.clopper_pearson_ci(5, 0), "not the number 0")
})

# --- .design_effect(): the seven steps ------------------------------------------

# The matrix form of the API design's steps 3 to 5, written as the matrices
# they describe, for the closed form to be checked against.
matrix_design_effect <- function(y, w, family = c("poisson", "binomial")) {
  family <- match.arg(family)
  G <- length(y)
  S <- sum(w)
  Y <- sum(y)
  R <- Y / S
  b <- if (family == "poisson") R else R * (1 - R)
  tau2 <- max(0, (sum(w * (y / w - R)^2) - (G - 1) * b) / (S - sum(w^2) / S))
  phi <- b / w + tau2
  H <- matrix(w / S, nrow = G, ncol = G, byrow = TRUE)
  A <- diag(G) - H
  M <- A %*% diag(phi, G) %*% t(A)
  MiM <- diag(M)
  v <- sum((y - R * w)^2 * (phi / MiM)) / S^2
  cc <- (w^2 * phi) / (MiM * S^2)
  P <- diag(sqrt(phi), G) %*% t(A) %*% diag(cc, G) %*% A %*% diag(sqrt(phi), G)
  df <- sum(diag(P))^2 / sum(diag(P %*% P))
  t975 <- stats::qt(0.975, df = df)
  d <- if (family == "poisson") {
    max(1, v * S^2 / Y) * (t975 / stats::qnorm(0.975))^2
  } else {
    max(1, v * S / (R * (1 - R))) * (t975 / stats::qnorm(0.975))^2
  }
  list(effect = d, df = df, variance = v, tau2 = tau2)
}

test_that(".design_effect matches the matrix form on Poisson clusters", {
  y <- c(5, 0, 12, 3, 8, 20)
  w <- c(1000, 800, 1200, 600, 900, 1500)
  expected <- matrix_design_effect(y, w, "poisson")
  got <- neoipcr:::.design_effect(y, w, family = "poisson")
  expect_equal(got$status, "ok")
  expect_equal(got$tau2, expected$tau2)
  expect_equal(got$variance, expected$variance)
  expect_equal(got$df, expected$df)
  expect_equal(got$effect, expected$effect)
})

test_that(".design_effect matches the matrix form on binomial clusters", {
  y <- c(15, 8, 22, 5, 18, 30)
  w <- c(20, 10, 25, 8, 20, 35)
  expected <- matrix_design_effect(y, w, "binomial")
  got <- neoipcr:::.design_effect(y, w, family = "binomial")
  expect_equal(got$status, "ok")
  expect_equal(got$tau2, expected$tau2)
  expect_equal(got$variance, expected$variance)
  expect_equal(got$df, expected$df)
  expect_equal(got$effect, expected$effect)
})

test_that(".design_effect matches the matrix form on strongly unequal denominators", {
  # A denominator of a million beside one of fifty: the squared Frobenius
  # norm of P is then a small sum among large products, and a closed form
  # that regroups the squares into products of sums loses their leading
  # digits - it has even turned the trace negative here.
  y <- c(100000, 45)
  w <- c(1000000, 50)
  for (family in c("poisson", "binomial")) {
    expected <- matrix_design_effect(y, w, family)
    got <- neoipcr:::.design_effect(y, w, family = family)
    expect_equal(got$status, "ok")
    expect_equal(got$tau2, expected$tau2)
    expect_equal(got$variance, expected$variance)
    expect_equal(got$df, expected$df)
    expect_equal(got$effect, expected$effect)
  }
})

test_that("two clusters give one degree of freedom whatever their denominators", {
  # With two clusters the leverage sandwich I - H has rank 1, so P does
  # and tr P^2 = (tr P)^2: the degrees of freedom are 1 even where one
  # denominator dwarfs the other.
  got <- neoipcr:::.design_effect(c(100000, 45), c(1000000, 50), "binomial")
  expect_equal(got$df, 1, tolerance = 1e-6)
})

test_that(".design_effect keeps its digits across a battery of unequal denominators", {
  set.seed(7)
  for (i in seq_len(8)) {
    G <- sample(3:12, 1)
    w <- round(10^runif(G, 1, 6))
    family <- c("poisson", "binomial")[sample(2, 1)]
    y <- if (family == "binomial") {
      round(pmin(w, w * runif(1, 0.05, 0.5) * runif(G, 0.2, 1.6)))
    } else {
      round(w * runif(1, 0.005, 0.05) * runif(G, 0.3, 1.7))
    }
    if (sum(y) == 0) next
    expected <- matrix_design_effect(y, w, family)
    got <- neoipcr:::.design_effect(y, w, family = family)
    expect_equal(got$df, expected$df, tolerance = 1e-6)
    expect_equal(got$effect, expected$effect, tolerance = 1e-6)
  }
})

test_that(".design_effect is at least 1 and widens with the clustering", {
  w <- rep(1000, 10)
  unclustered <- neoipcr:::.design_effect(rep(10, 10), w, "poisson")
  clustered <- neoipcr:::.design_effect(c(0, 50, 0, 50, 0, 50, 0, 50, 0, 50), w,
                                        "poisson")
  expect_gte(unclustered$effect, 1)
  expect_gte(clustered$effect, 1)
  expect_gt(clustered$effect, unclustered$effect)

  # The design effect never below 1 leaves the gamma interval never narrower
  # than the exact Poisson interval on the summed counts.
  ci_exact <- neoipcr:::.gamma_rate_ci(100, 10000, multiplier = 1000)
  ci_de <- neoipcr:::.gamma_rate_ci(100 / clustered$effect, 10000 / clustered$effect,
                                    multiplier = 1000)
  expect_lte(ci_exact$upper - ci_exact$lower, ci_de$upper - ci_de$lower)
})

test_that(".design_effect gives the degrees of freedom of equal denominators exactly", {
  got <- neoipcr:::.design_effect(c(3, 5), c(100, 100), "poisson")
  expect_equal(round(got$df, 6), 1)  # G - 1 in exact arithmetic
})

test_that(".design_effect reports the figures that leave no variance to estimate from", {
  expect_equal(neoipcr:::.design_effect(c(0, 0, 0), c(100, 200, 150), "poisson")$status,
               "zero_events")
  expect_equal(neoipcr:::.design_effect(c(20, 20), c(20, 20), "binomial")$status,
               "full_share")
  expect_equal(neoipcr:::.design_effect(c(30, 25), c(20, 20), "binomial")$status,
               "share_exceeded")
  # A single cluster's events over its own denominator beside a pooled
  # share below 100 %: §13.1.2's no-interval case is the figure's days
  # exceeding their denominator - the pooled Y against the pooled S - and
  # a cluster above its own denominator leaves the figure an interval.
  y <- c(3, 1, 1)
  w <- c(2, 10, 10)
  one_over <- neoipcr:::.design_effect(y, w, "binomial")
  expect_equal(one_over$status, "ok")
  expect_gte(one_over$effect, 1)
  expect_equal(one_over$effect, matrix_design_effect(y, w, "binomial")$effect)
  expect_equal(neoipcr:::.design_effect(7, 100, "poisson")$status, "few_clusters")
  expect_equal(neoipcr:::.design_effect(c(0, 0), c(20, 20), "binomial")$status,
               "zero_events")
  expect_true(is.na(neoipcr:::.design_effect(c(0, 0), c(100, 200), "poisson")$effect))
})

# --- .design_effect_mean() -------------------------------------------------------

test_that(".design_effect_mean rests on 1 when every patient has the same days", {
  # Every exposed patient the same days: no variance anywhere, and the
  # design effect is 1, read from the values themselves.
  clusters <- list(rep(7, 5), rep(7, 6), rep(7, 4))
  got <- neoipcr:::.design_effect_mean(clusters)
  expect_equal(got$status, "ok")
  expect_equal(got$effect, 1)
  expect_equal(got$df, NA_real_)
  expect_equal(got$variance, 0)
})

test_that(".design_effect_mean reads the equal days from the values, not the variances", {
  # Equal fractional days: the cluster totals round apart where the values
  # do not, leaving a tiny between-cluster variance over a baseline of no
  # variance at all - the design effect still stands at 1.
  got <- neoipcr:::.design_effect_mean(list(rep(0.1, 3), rep(0.1, 2),
                                            rep(0.1, 7)))
  expect_equal(got$status, "ok")
  expect_equal(got$effect, 1)
  expect_equal(got$df, NA_real_)
  expect_equal(got$variance, 0)
})

test_that(".design_effect_mean refuses unequal days whose variance underflows to nothing", {
  # Unequal days scaled past the range where a squared deviation stays a
  # number: the variance with the patients as clusters underflows to 0, as
  # the equal days leave theirs, and no baseline is left to estimate
  # against - the values themselves say these days are not equal.
  clusters <- list(c(2, 9, 4), c(5, 5, 30), c(1, 2), c(8, 7, 6, 5))
  got <- neoipcr:::.design_effect_mean(lapply(clusters, function(x) x * 1e-170))
  expect_equal(got$status, "not_estimable")
})

test_that(".design_effect_mean exceeds 1 when the departments' means differ", {
  set.seed(1)
  low <- stats::rgamma(10, shape = 2, rate = 0.5)
  high <- stats::rgamma(10, shape = 2, rate = 0.15)
  got <- neoipcr:::.design_effect_mean(list(low, high))
  expect_gt(got$effect, 1)
  # Two clusters give one degree of freedom in exact arithmetic.
  expect_gt(got$df, 0.999)
  expect_equal(neoipcr:::.design_effect_mean(list(low))$status, "few_clusters")
})

test_that(".design_effect_mean divides the CR2 variance by the patients' variance when the days are equal within departments", {
  # Two patients of two days each, two of five: the CR2 variance of the
  # pooled mean is 2.25, the variance with the patients as clusters is the
  # days' variance over their number, 3 / 4, and equal denominators leave
  # one degree of freedom - the seven steps' closed form by hand.
  got <- neoipcr:::.design_effect_mean(list(c(2, 2), c(5, 5)))
  expect_equal(got$variance, 2.25)
  expect_equal(got$df, 1)
  expect_equal(got$effect, 3 * (stats::qt(0.975, df = 1) / stats::qnorm(0.975))^2)
})

test_that(".design_effect_mean's baseline is the whole days' variance, not the within-department one", {
  # Days differing within each department too, of c(1, 3) and c(5, 7): the
  # CR2 variance is 4 and the variance with the patients as clusters is
  # (9 + 1 + 1 + 9) / 3 / 4, the whole variance of the days, while the
  # within-department variance 2 only feeds the working model.
  got <- neoipcr:::.design_effect_mean(list(c(1, 3), c(5, 7)))
  expect_equal(got$variance, 4)
  expect_equal(got$df, 1)
  expect_equal(got$effect, 2.4 * (stats::qt(0.975, df = 1) / stats::qnorm(0.975))^2)
})

test_that("the widening takes both quantiles at the level", {
  # §13.1.4 step 6 gives t and z one probability, 1 - (1 - level)/2, of the
  # t distribution on the degrees of freedom and of the normal: at 90% both
  # move to the 95th percentile of their distribution, and the factor tends
  # to 1 as the degrees of freedom grow. The hand-computed case of the
  # two-department rows above, one degree of freedom.
  got <- neoipcr:::.design_effect_mean(list(c(2, 2), c(5, 5)),
                                       conf.level = 0.90)
  expect_equal(got$effect,
               3 * (stats::qt(0.95, df = 1) / stats::qnorm(0.95))^2)
})

test_that("the count engine takes its widening at the level too", {
  # Two clusters of one shared denominator leave tau2 at 0, since the
  # weighted squared deviations 100 * 0.01^2 + 100 * 0.01^2 = 0.02 equal
  # (G - 1) * b = 0.02, and the equal denominators leave one degree of
  # freedom. The CR2 variance is 2 * 100^-2 over the baseline 0.02 / 200:
  # the base ratio is 0.5, so the effect is the widening alone, (t / z)^2
  # at the level's quantile.
  got <- neoipcr:::.design_effect(c(3, 1), c(100, 100), "poisson",
                                  conf.level = 0.90)
  expect_equal(got$status, "ok")
  expect_equal(got$df, 1)
  expect_equal(got$effect, (stats::qt(0.95, df = 1) / stats::qnorm(0.95))^2)
})

test_that(".design_effect_mean validates its clusters", {
  expect_error(neoipcr:::.design_effect_mean(list(c(1, NA), c(2, 3))),
               "hold only finite values")
  expect_error(neoipcr:::.design_effect_mean(list(c(-1, -1), c(-1, -1))),
               "non-negative")
  expect_error(neoipcr:::.design_effect_mean(list(c(-1, 2), c(3, 4))),
               "non-negative")
  expect_error(neoipcr:::.design_effect_mean(list(numeric(0), c(1, 2))),
               "at least one value")
  expect_error(neoipcr:::.design_effect_mean(c(1, 2, 3)),
               "list of numeric vectors")
})

test_that("the interval engines refuse a missing confidence level", {
  # A missing level is a value no interval rests on: every engine refuses
  # it at its entry, before any estimate is reported with it.
  expect_error(neoipcr:::.design_effect_mean(list(c(1, 3), c(5, 7)),
                                             conf.level = NA_real_),
               "not a numeric")
  expect_error(neoipcr:::.design_effect(c(5, 0, 12), c(1000, 800, 1200),
                                        "poisson", conf.level = NA_real_),
               "not a numeric")
  expect_error(neoipcr:::.gamma_rate_ci(5, 100, conf.level = NA_real_),
               "not a numeric")
  expect_error(neoipcr:::.clopper_pearson_ci(5, 20, conf.level = NA_real_),
               "not a numeric")
  expect_error(neoipcr:::.order_statistic_quartile_ci(
    events = c(5, 12, 30, 8), exposure = c(100, 200, 300, 150),
    conf.level = NA_real_), "not a numeric")
})

test_that("the interval engines refuse infinite inputs", {
  # An infinite count or denominator is a caller bug, not a degenerate
  # figure: the interval it would leave has no rate in it. A NaN is a
  # corrupt value too, not the structurally absent NA the quartile engine
  # admits.
  expect_error(neoipcr:::.clopper_pearson_ci(Inf, Inf), "not `Inf`")
  expect_error(neoipcr:::.clopper_pearson_ci(10, Inf), "not `Inf`")
  expect_error(neoipcr:::.gamma_rate_ci(5, Inf), "not `Inf`")
  expect_error(neoipcr:::.gamma_rate_ci(Inf, 100), "not `Inf`")
  expect_error(neoipcr:::.design_effect(c(1, 2), c(Inf, 100), "poisson"),
               "must be finite")
  expect_error(neoipcr:::.design_effect(c(1, Inf), c(100, 100), "poisson"),
               "must be finite")
  expect_error(neoipcr:::.design_effect_mean(list(c(1, Inf), c(2, 3))),
               "hold only finite values")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(1, Inf), c(100, 100)),
               "hold finite values")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(1, 2), c(100, Inf)),
               "hold finite values")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(1, NaN), c(100, 100)),
               "hold finite values")
  expect_error(neoipcr:::.order_statistic_quartile_ci(c(1, 2), c(100, 100),
                                                      multiplier = Inf),
               "not `Inf`")
})

test_that("both engines read the all-zero state alike", {
  # The same status vocabulary for the same degenerate input: no events, and
  # nothing for the design effect to estimate.
  expect_equal(
    neoipcr:::.design_effect(c(0, 0, 0), c(3, 3, 3), "poisson")$status,
    "zero_events")
  expect_equal(
    neoipcr:::.design_effect_mean(list(c(0, 0, 0), c(0, 0, 0)))$status,
    "zero_events")
})

test_that("the engines refuse a result the traces leave without a value", {
  # Denominators small enough for the working variance to overflow leave the
  # traces NaN; that is not an estimated design effect.
  got <- neoipcr:::.design_effect(c(1, 1), c(1e-200, 1e-200), "poisson")
  expect_equal(got$status, "not_estimable")
  expect_true(is.na(got$effect))
  expect_true(is.na(got$df))
  # The same class wherever an intermediate overflows: a pooled rate whose
  # own division overflows, and a within-department sum of squares that does.
  expect_equal(neoipcr:::.design_effect(c(2, 3), c(1e-320, 1e-320),
                                        "poisson")$status, "not_estimable")
  expect_equal(neoipcr:::.design_effect_mean(list(c(1e155, 1), c(1, 1)))$status,
               "not_estimable")
  # And its mirror: inputs that fit in doubles while their pooled sums do
  # not, leaving no pooled figure to estimate - where the sums stay finite,
  # as for every figure of ordinary size, the estimable zero keeps its own
  # status.
  expect_equal(neoipcr:::.design_effect(c(1, 2), c(1e308, 1e308),
                                        "poisson")$status, "not_estimable")
  expect_equal(neoipcr:::.design_effect(c(1, 2), c(9e307, 9e307),
                                        "binomial")$status, "not_estimable")
  expect_equal(
    neoipcr:::.design_effect_mean(list(c(1e308, 1e308), c(1, 1)))$status,
    "not_estimable")
  # And the shape of it where the products overflow to infinity without a
  # missing value anywhere: every sum and the design effect's intermediates
  # stay finite while the CR2 variance itself overflows, which an infinite
  # design effect would divide the counts away in the interval.
  expect_equal(neoipcr:::.design_effect(c(2e154, 0, 0), rep(1e150, 3),
                                        "poisson")$status, "not_estimable")
})

# --- .kish_effective_number() -----------------------------------------------------

test_that(".kish_effective_number is the squared sum over the sum of squares", {
  expect_equal(neoipcr:::.kish_effective_number(c(10, 20, 30)),
               (10 + 20 + 30)^2 / (10^2 + 20^2 + 30^2))
  # One infant is one effective infant however long its stay.
  expect_equal(neoipcr:::.kish_effective_number(250), 1)
  # Equal stays give the headcount.
  expect_equal(neoipcr:::.kish_effective_number(rep(40, 12)), 12)
  expect_error(neoipcr:::.kish_effective_number(c(10, -1)), "non-negative")
  expect_error(neoipcr:::.kish_effective_number(c(0, 0)),
               "at least one positive")
})

# --- the flags -------------------------------------------------------------------

test_that(".figure_flags marks the effective value below 16, never at 16", {
  expect_true(neoipcr:::.figure_flags(effective_value = 15.999)$sparse_data)
  expect_false(neoipcr:::.figure_flags(effective_value = 16)$sparse_data)
  expect_true(neoipcr:::.figure_flags(effective_value = 0)$sparse_data)
  # The threshold is configurable; 16 is its default.
  expect_false(neoipcr:::.figure_flags(effective_value = 15.999, threshold = 10)$sparse_data)
  # A value given but missing proves nothing; one not given leaves nothing
  # for the flag to test.
  expect_true(neoipcr:::.figure_flags(effective_value = NA_real_)$sparse_data)
  expect_false(neoipcr:::.figure_flags()$sparse_data)
})

test_that(".figure_flags marks the degrees of freedom below 8 after rounding", {
  expect_true(neoipcr:::.figure_flags(df = 7.9999991)$df_low)
  # Equal denominators give G - 1 only in exact arithmetic: a 7.9999996
  # rounds to 8 at six places and is not marked, while a 7.9999991 rounds
  # to 7.999999 and is.
  expect_false(neoipcr:::.figure_flags(df = 7.9999996)$df_low)
  expect_false(neoipcr:::.figure_flags(df = 8)$df_low)
  expect_false(neoipcr:::.figure_flags(df = NA_real_)$df_low)
})

test_that(".figure_flags marks a figure from fewer than 9 patients", {
  expect_true(neoipcr:::.figure_flags(n_patients = 8)$few_patients)
  expect_false(neoipcr:::.figure_flags(n_patients = 9)$few_patients)
  # A share whose denominator counts the patients once carries the flag on
  # its effective denominator alone.
  expect_false(neoipcr:::.figure_flags(effective_value = 5, n_patients = 5,
                                       counts_patients_once = TRUE)$few_patients)
  expect_error(neoipcr:::.figure_flags(n_patients = NA_real_), "whole number")
})

test_that(".figure_flags always marks a figure without an interval", {
  # A pooled 0, a pooled share of 100%, a pooled figure from fewer than 2
  # departments and a share whose days exceed its denominator.
  expect_true(neoipcr:::.figure_flags(no_interval = TRUE)$sparse_data)
  expect_true(neoipcr:::.figure_flags(no_interval = TRUE,
                                      effective_value = 500)$sparse_data)
})

# --- the lower limit's cut ---------------------------------------------------------

test_that(".cut_ci_lower never lets a lower limit below the measure's minimum", {
  expect_equal(neoipcr:::.cut_ci_lower(c(0.4, 1.5, 0), 1), c(1, 1.5, 1))
  expect_equal(neoipcr:::.cut_ci_lower(c(97.2, 100), 100), c(100, 100))
  expect_equal(neoipcr:::.cut_ci_lower(c(0, 3), 0), c(0, 3))
  expect_equal(neoipcr:::.cut_ci_lower(NA_real_, 1), NA_real_)
})

test_that(".measure_minimum knows the measures whose minimum is not 0", {
  expect_equal(neoipcr:::.measure_minimum("rate"), 0)
  expect_equal(neoipcr:::.measure_minimum("operations_per_patient"), 1)
  expect_equal(neoipcr:::.measure_minimum("mean_days_exposed"), 1)
  expect_equal(neoipcr:::.measure_minimum("agents_per_infection"), 100)
  expect_error(neoipcr:::.measure_minimum("share"), "must be one of")
})

# --- determinism, the untouched RNG, and the shared invariants -----------------------

test_that("the interval machinery is deterministic and leaves the RNG alone", {
  set.seed(999)
  expected <- stats::runif(1)
  set.seed(999)
  args <- list(y = c(5, 0, 12, 3, 8, 20), w = c(1000, 800, 1200, 600, 900, 1500))
  first <- do.call(neoipcr:::.design_effect, c(args, list(family = "poisson")))
  second <- do.call(neoipcr:::.design_effect, c(args, list(family = "poisson")))
  quartiles <- neoipcr:::.order_statistic_quartile_ci(args$y, args$w, multiplier = 1000)
  expect_equal(first, second)
  expect_equal(quartiles,
               neoipcr:::.order_statistic_quartile_ci(args$y, args$w, multiplier = 1000))
  actual <- stats::runif(1)
  expect_equal(actual, expected)
})

test_that("the interval machinery never consumes the RNG", {
  # The intervals are closed-form, and the API design's per-row budget
  # follows from that: a regression into resampling would consume the RNG,
  # which this catches on any machine at any load, unlike a timed bound.
  set.seed(999)
  expected <- stats::runif(3)
  set.seed(999)
  w30 <- rep(c(800, 950, 1100, 1300), length.out = 30)
  y30 <- seq(2, 31)
  neoipcr:::.gamma_rate_ci(37, 12345, multiplier = 1000)
  neoipcr:::.clopper_pearson_ci(12, 40, scale = 100)
  neoipcr:::.order_statistic_quartile_ci(y30, w30, multiplier = 1000)
  neoipcr:::.design_effect(y30, w30, "poisson")
  neoipcr:::.design_effect_mean(split(y30, rep(1:6, each = 5)))
  neoipcr:::.kish_effective_number(y30)
  expect_equal(stats::runif(3), expected)
})

test_that(".design_effect's closed form matches the matrix form at 30 clusters", {
  w30 <- rep(c(800, 950, 1100, 1300), length.out = 30)
  y30 <- seq(2, 31)
  expected <- matrix_design_effect(y30, w30, "poisson")
  got <- neoipcr:::.design_effect(y30, w30, family = "poisson")
  expect_equal(got$tau2, expected$tau2)
  expect_equal(got$variance, expected$variance)
  expect_equal(got$df, expected$df)
  expect_equal(got$effect, expected$effect)
})

test_that(".design_effect's closed form matches the matrix form at patient size", {
  # A department's patient clusters, denominators small counts of days: the
  # closed form sums its pair terms over the distinct denominators weighted
  # by their multiplicities, the matrix form knows only the clusters.
  set.seed(11)
  for (G in c(200L, 500L)) {
    w <- sample(1:20, G, replace = TRUE)
    y <- sample(0:3, G, replace = TRUE)
    if (sum(y) == 0) y[1] <- 1
    expected <- matrix_design_effect(y, w, "poisson")
    got <- neoipcr:::.design_effect(y, w, family = "poisson")
    expect_equal(got$df, expected$df, tolerance = 1e-6)
    expect_equal(got$effect, expected$effect, tolerance = 1e-6)
  }
  # One denominator value, many clusters: the within-value pair term with a
  # multiplicity the tests' small cases never reach.
  y <- sample(0:5, 300, replace = TRUE)
  expected <- matrix_design_effect(y, rep(1000, 300), "poisson")
  got <- neoipcr:::.design_effect(y, rep(1000, 300), family = "poisson")
  expect_equal(got$df, expected$df, tolerance = 1e-6)
  expect_equal(got$effect, expected$effect, tolerance = 1e-6)
})

test_that("a denominator shared by 46,342 clusters keeps the multiplicity in doubles", {
  # The within-value pair term multiplies mult (mult - 1) for a denominator
  # shared by mult clusters: taken in tabulate's integers the product
  # overflows to NA from 46,342 clusters, a size a network's share over
  # patients each exposed a day reaches.
  y <- rep(c(0, 1, 2, 3), length.out = 46342)
  got <- neoipcr:::.design_effect(y, rep(10, 46342), "poisson")
  expect_equal(got$status, "ok")
  expect_equal(round(got$df, 6), 46341)  # G - 1 in exact arithmetic
  expect_gte(got$effect, 1)
  expect_true(is.finite(got$variance))
})

test_that("an interval is never empty and a share stays inside its bounds", {
  y <- c(5, 0, 12, 3, 8, 20)
  w <- c(1000, 800, 1200, 600, 900, 1500)
  de <- neoipcr:::.design_effect(y, w, "poisson")
  ci <- neoipcr:::.gamma_rate_ci(sum(y) / de$effect, sum(w) / de$effect,
                                 multiplier = 1000)
  expect_lte(ci$lower, ci$rate)
  expect_gte(ci$upper, ci$rate)

  ys <- c(15, 8, 22, 5, 18, 30)
  ws <- c(20, 10, 25, 8, 20, 35)
  de_s <- neoipcr:::.design_effect(ys, ws, "binomial")
  ci_s <- neoipcr:::.clopper_pearson_ci(sum(ys) / de_s$effect, sum(ws) / de_s$effect,
                                        scale = 100)
  expect_gte(ci_s$lower, 0)
  expect_lte(ci_s$upper, 100)
})
