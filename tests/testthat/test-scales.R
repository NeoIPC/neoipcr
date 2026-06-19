test_that("birth-weight binning tolerates missing and all-missing values", {
  for (bin in list(neoipcr:::bw50, neoipcr:::bw125, neoipcr:::bw250, neoipcr:::bw500)) {
    # Some values missing: every present value still bins, NA stays NA.
    some_na <- bin(c(800, NA, 1200, 1500))
    expect_s3_class(some_na, "ordered")
    expect_length(some_na, 4)
    expect_true(is.na(some_na[2]))

    # All values missing: previously errored in seq() with a non-finite bound.
    all_na <- bin(c(NA_real_, NA_real_))
    expect_s3_class(all_na, "ordered")
    expect_length(all_na, 2)
    expect_true(all(is.na(all_na)))
    expect_length(levels(all_na), 0)

    # Empty input returns an empty factor.
    expect_length(bin(numeric(0)), 0)

    # as_factor = FALSE returns the numeric midpoints, preserving NA.
    mids <- bin(c(800, NA), as_factor = FALSE)
    expect_length(mids, 2)
    expect_true(is.na(mids[2]))
  }
})

test_that("bw50 groups finite weights into shared 50 g strata", {
  # m = floor((x - 25) / 50) * 50 + 50, so 475..524 all map to the 500 g midpoint.
  res <- neoipcr:::bw50(c(475, 500, 524, 525))
  expect_equal(as.character(res[1]), as.character(res[3]))
  expect_false(as.character(res[1]) == as.character(res[4]))
})
