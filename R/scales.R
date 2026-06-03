ga7 <- function(x) {
  7 * dplyr::case_match(
  as.integer(x %% 7),
  0L ~ as.integer(x / 7),
  1L ~ as.integer(x / 7),
  2L ~ as.integer(x / 7),
  3L ~ as.integer(x / 7),
  4L ~ as.integer(x / 7) + 1,
  5L ~ as.integer(x / 7) + 1,
  6L ~ as.integer(x / 7) + 1)
}

bw50 <- function(x, as_factor = TRUE) {
  m <- floor((x-25)/50)*50+50
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-25
  ub <- m+24
  ordered(
    m,
    levels = seq(min(m), max(m), 50),
    labels = paste0(format(seq(min(lb), max(lb), 50))," g - ",format(seq(min(ub), max(ub), 50))," g"))
}

bw125 <- function(x, as_factor = TRUE) {
  m <- floor((x-63)/125)*125+125
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-62
  ub <- m+62
  ordered(
    m,
    levels = seq(min(m), max(m), 125),
    labels = paste0(format(seq(min(lb), max(lb), 125))," g - ",format(seq(min(ub), max(ub), 125))," g"))
}

bw250 <- function(x, as_factor = TRUE) {
  m <- floor((x-125)/250)*250+250
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-125
  ub <- m+124
  ordered(
    m,
    levels = seq(min(m), max(m), 250),
    labels = paste0(format(seq(min(lb), max(lb), 250))," g - ",format(seq(min(ub), max(ub), 250))," g"))
}

bw500 <- function(x, as_factor = TRUE) {
  m <- as.integer(x/500)*500+250
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-250
  ub <- m+249
  ordered(
    m,
    levels = seq(min(m), max(m), 500),
    labels = paste0(format(seq(min(lb), max(lb), 500))," g - ",format(seq(min(ub), max(ub), 500))," g"))
}
