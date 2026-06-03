add_class <- function(x, class_name) {
  check_character(class_name, allow_null = FALSE, allow_na = FALSE)
  class(x) <- c(class_name, class(x))
  return(x)
}

cache <- function(x, container, key) {
  container$.cache[[key]] = x
  return(x)
}

clean_cache <- function(x) {
  rm(list = ls(envir = x$.cache), envir = x$.cache)
}

new_cache <- function(x) {
  x$.cache <- new.env(parent = emptyenv())
  x
}

get_cached <- function(container, key) {
  if (!is.null(container$.cache) && !is.null(r <- get0(key, envir = container$.cache)))
    return(r)

  return(NULL)
}
