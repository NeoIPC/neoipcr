ga7 <- function(x) {
  remainder <- as.integer(x %% 7)
  base <- as.integer(x / 7)
  7 * dplyr::case_when(
    remainder <= 3L ~ base,
    .default = base + 1L
  )
}

bw50 <- function(x, as_factor = TRUE) {
  m <- floor((x-25)/50)*50+50
  if(!as_factor)
    return(m)

  # No finite midpoints (empty or all-NA input) — seq() below needs a finite range.
  if(!any(is.finite(m)))
    return(ordered(m))

  lb <- m-25
  ub <- m+24
  ordered(
    m,
    levels = seq(min(m, na.rm = TRUE), max(m, na.rm = TRUE), 50),
    labels = paste0(format(seq(min(lb, na.rm = TRUE), max(lb, na.rm = TRUE), 50))," g - ",format(seq(min(ub, na.rm = TRUE), max(ub, na.rm = TRUE), 50))," g"))
}

bw125 <- function(x, as_factor = TRUE) {
  m <- floor((x-63)/125)*125+125
  if(!as_factor)
    return(m)

  # No finite midpoints (empty or all-NA input) — seq() below needs a finite range.
  if(!any(is.finite(m)))
    return(ordered(m))

  lb <- m-62
  ub <- m+62
  ordered(
    m,
    levels = seq(min(m, na.rm = TRUE), max(m, na.rm = TRUE), 125),
    labels = paste0(format(seq(min(lb, na.rm = TRUE), max(lb, na.rm = TRUE), 125))," g - ",format(seq(min(ub, na.rm = TRUE), max(ub, na.rm = TRUE), 125))," g"))
}

bw250 <- function(x, as_factor = TRUE) {
  m <- floor((x-125)/250)*250+250
  if(!as_factor)
    return(m)

  # No finite midpoints (empty or all-NA input) — seq() below needs a finite range.
  if(!any(is.finite(m)))
    return(ordered(m))

  lb <- m-125
  ub <- m+124
  ordered(
    m,
    levels = seq(min(m, na.rm = TRUE), max(m, na.rm = TRUE), 250),
    labels = paste0(format(seq(min(lb, na.rm = TRUE), max(lb, na.rm = TRUE), 250))," g - ",format(seq(min(ub, na.rm = TRUE), max(ub, na.rm = TRUE), 250))," g"))
}

bw500 <- function(x, as_factor = TRUE) {
  m <- as.integer(x/500)*500+250
  if(!as_factor)
    return(m)

  # No finite midpoints (empty or all-NA input) — seq() below needs a finite range.
  if(!any(is.finite(m)))
    return(ordered(m))

  lb <- m-250
  ub <- m+249
  ordered(
    m,
    levels = seq(min(m, na.rm = TRUE), max(m, na.rm = TRUE), 500),
    labels = paste0(format(seq(min(lb, na.rm = TRUE), max(lb, na.rm = TRUE), 500))," g - ",format(seq(min(ub, na.rm = TRUE), max(ub, na.rm = TRUE), 500))," g"))
}
