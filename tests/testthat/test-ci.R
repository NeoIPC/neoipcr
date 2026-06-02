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
  pt <- poisson.test(47, T = 28904, conf.level = 0.95)
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
  z <- qnorm(0.975)
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
  observed_q <- quantile(rates, probs = c(0.25, 0.5, 0.75), names = FALSE)
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
  expected <- runif(1)
  set.seed(999)
  bootstrap_quantile_ci(c(5, 10), c(100, 100), type = "poisson", seed = 42)
  actual <- runif(1)
  expect_equal(actual, expected)
})
