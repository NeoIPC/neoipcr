get_birthweight_figure_data <- function(x) {
  bw_quartiles <- x$patients$birth_weight |>
    stats::quantile(names = FALSE) |>
    as.integer()

  bw_mean = x$patients$birth_weight |>
    mean() |>
    as.integer()

  bw_scale_min <- as.integer(bw_quartiles[1] / 50L) * 50L - 50L
  bw_scale_max <- as.integer(bw_quartiles[5] / 50L) * 50L + 100L

  # density.default needs at least two points
  if (length(x$patients$birth_weight) > 1) {
    bw_density <- x$patients$birth_weight |>
      stats::density(from = bw_scale_min, to = bw_scale_max)

    density_bw <- bw_density$x
    density_val <-  bw_density$y / sum(bw_density$y)
  } else {
    density_bw <- double()
    density_val <-  double()
  }

  list(
    density = tibble::tibble(
      birth_weight = density_bw,
      density = density_val
    ),
    frequency = tibble::tibble(
      birth_weight_cat = x$patients$birth_weight |>
        bw50(as_factor = F)
    ) |>
      dplyr::group_by(.data$birth_weight_cat) |>
      dplyr::summarise(n = dplyr::n()),
    location_parameters = tibble::tibble(
      q1 = bw_quartiles[2],
      q2 = bw_quartiles[3],
      q3 = bw_quartiles[4],
      mean = bw_mean),
    scale = tibble::tibble(
      min = bw_scale_min,
      max = bw_scale_max
    )
  )
}

get_gestational_age_figure_data <- function(x) {
  ga_quartiles <- x$patients$total_gestation_days |>
    stats::quantile(names = FALSE) |>
    as.integer()

  ga_mean = x$patients$total_gestation_days |>
    mean() |>
    as.integer()

  ga_scale_min = as.integer(ga_quartiles[1] / 7L) * 7L - 7L
  ga_scale_max = as.integer(ga_quartiles[5] / 7L) * 7L + 14L

  # density.default needs at least two points
  if (length(x$patients$total_gestation_days) > 1) {
    ga_density <- x$patients$total_gestation_days |>
      stats::density(from = ga_scale_min, to = ga_scale_max)

    density_ga <- ga_density$x
    density_val <-  ga_density$y / sum(ga_density$y)
  } else {
    density_ga <- double()
    density_val <-  double()
  }

  list(
    density = tibble::tibble(
      gestational_age = density_ga,
      density = density_val
    ),
    frequency = tibble::tibble(
      gestational_age_cat = x$patients$total_gestation_days |>
        ga7()
    ) |>
      dplyr::group_by(.data$gestational_age_cat) |>
      dplyr::summarise(n = dplyr::n()),
    location_parameters = tibble::tibble(
      q1 = ga_quartiles[2],
      q2 = ga_quartiles[3],
      q3 = ga_quartiles[4],
      mean = ga_mean),
    scale = tibble::tibble(
      min = ga_scale_min,
      max = ga_scale_max
    )
  )
}

check_ds_and_try_get_table <- function(
    x, table_name, use_cache, include_quartiles) {
  # Only the reference dataset contains quartiles, so we have to be strict here
  if (include_quartiles) {
    check_neoipcr_ds_or_ref_ds(x)
  } else {
    check_neoipcr_ds_or_rep_ds(x)
  }

  # When x is a raw neoipcr_ds (not a pre-computed neoipcr_rep_ds /
  # neoipcr_ref_ds), the caller will compute the table from scratch via
  # the calc pipeline. That pipeline needs the link-privacy gates
  # non-"no" — check them here so every get_*_table() caller gets the
  # same uniform precondition without repeating the assertion 11 times.
  if (is_neoipcr_ds(x) && !is_neoipcr_rep_ds(x))
    assert_options_for(x, required = list(
      include_department = c("pseudo", "full"),
      include_patient    = c("pseudo", "full"),
      include_enrollment = c("pseudo", "full"),
      include_event      = c("pseudo", "full")
    ), fn_name = table_name)

  # First try, if it's a report dataset because in that case it already contains
  # the table and we can just return it (potentially after removing the quartile
  # columns)
  if(is_neoipcr_rep_ds(x))
  {
    if(!include_quartiles && is_neoipcr_ref_ds(x))
      return(
        x[[table_name]] |>
          dplyr::select(!tidyselect::any_of(c("q1", "q2", "q3",
            "q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
            "q3_ci_lower", "q3_ci_upper"))))

    return(x[[table_name]])
  }

  # Then try if we can find a cached table we can use.
  # If we find a cached version we need to check if it includes quartiles.
  # If it does but they are not requested, we just return a copy with the
  # quartiles removed.
  # If it does not but they were requested, we need to recalculate the table
  # with quartiles
  # Otherwise we can just return it.
  if(use_cache && !is.null(r <- get_cached(x, table_name))) {
    if(!include_quartiles) {
      if(any(c("q1", "q2", "q3") %in% rlang::names2(r))) {
        return(
          r |>
            dplyr::select(!tidyselect::any_of(c("q1", "q2", "q3",
            "q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
            "q3_ci_lower", "q3_ci_upper"))))
      }

      return(r)
    } else if (all(c("q1", "q2", "q3") %in% rlang::names2(r))) {
      return(r)
    }
  }

  return(NULL)
}

#' Get the table with usage density rates of the time dependent risk factors
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing usage density rates of the time dependent risk
#'  factors
#' @export
get_usage_density_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE){
  cache_key <- "usage_density_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  risk_time <- get_risk_time(x, use_cache = use_cache)
  r <- risk_time |>
    dplyr::select(!"patient_days" & tidyselect::ends_with("_days")) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_days"),
      names_to = "factor",
      values_to = "n") |>
    dplyr::mutate(
      factor = stringr::str_remove(.data$factor, "_days$"),
      .before = 1) |>
    dplyr::full_join(
      risk_time |>
        dplyr::select(tidyselect::ends_with("_rate")) |>
        tidyr::pivot_longer(
          cols = tidyselect::ends_with("_rate"),
          names_to = "factor",
          values_to = "pooled") |>
        dplyr::mutate(
          factor = stringr::str_remove(.data$factor, "_rate$")),
      dplyr::join_by("factor"))
  r <- r |>
    dplyr::bind_cols(poisson_ci_cols(r$n, risk_time$patient_days, multiplier = 100)) |>
    add_class("neoipcr_tbl_udr")

  if(include_quartiles) {
    dept_risk_time <- get_risk_time(
      x,
      group_cols = "department_key",
      use_cache = use_cache)

    n_deps <- length(dept_risk_time$patient_days)
    median_patient_days <- stats::median(dept_risk_time$patient_days)

    # Pivot dept-level data to long format for bootstrap
    dept_rates <- dept_risk_time |>
      dplyr::select("department_key", "patient_days",
                     tidyselect::ends_with("_days") & !"patient_days") |>
      tidyr::pivot_longer(
        cols = !c("department_key", "patient_days"),
        names_to = "factor",
        values_to = "events") |>
      dplyr::mutate(
        factor = stringr::str_remove(.data$factor, "_days$"))

    r <- r |>
      dplyr::inner_join(
        dept_risk_time |>
          dplyr::select(tidyselect::ends_with("_rate")) |>
          dplyr::reframe(
            dplyr::across(
              tidyselect::everything(),
              ~list(stats::quantile(.x, probs = quartile_probs, names = FALSE)))) |>
          tidyr::pivot_longer(
            cols = tidyselect::ends_with("_rate"),
            names_to = "factor",
            values_to = "q") |>
          tidyr::unnest_wider(q, names_sep = "") |>
          dplyr::mutate(factor = stringr::str_remove(.data$factor, "_rate$")),
        dplyr::join_by("factor")) |>
      ensure_quartile_cols() |>
      dplyr::mutate(
        drop_quartiles = tidyr::replace_na(
          n_deps < 5 | round(100 / .data$pooled) >= median_patient_days,
          TRUE),
        q1 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q1),
        q2 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q2),
        q3 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q3))

    # Bootstrap CIs for quartiles where gate passes
    # .y carries the group key through bind_rows for key-based join
    boot_cis <- r |>
      dplyr::group_by(.data$factor) |>
      dplyr::group_map(~ {
        ci <- if (.x$drop_quartiles[1]) {
          tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                         q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                         q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
        } else {
          d <- dept_rates |> dplyr::filter(.data$factor == .y$factor)
          bootstrap_quantile_ci(d$events, d$patient_days,
                                type = "poisson", multiplier = 100)
        }
        dplyr::bind_cols(.y, ci)
      }) |>
      dplyr::bind_rows()

    r <- r |>
      dplyr::left_join(boot_cis, by = "factor") |>
      dplyr::select(!"drop_quartiles") |>
      add_class("neoipcr_tbl_udr_ref")
  }

  expected_levels <- c(
    "cvc","pvc",
    "vs","inv","niv",
    "ab","a","w","r",
    "human_milk","probiotic","kangaroo_care")

  missing <- setdiff(expected_levels, r$factor)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(
      tibble::tibble(
        factor = missing,
        n = 0L,
        pooled = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_))

  r |>
    dplyr::mutate(factor = factor(.data$factor, levels = expected_levels)) |>
    dplyr::arrange(.data$factor) |>
    cache(x, cache_key)
}

#' Get the antibiotic utilisation table with ATC5/substance-level rates
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing antibiotic exposure density rates per substance
#'  and ATC5 group, with hierarchical row structure
#' @export
get_antibiotic_utilisation_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "antibiotic_utilisation_table"
  if (!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  patient_days <- get_risk_time(x, use_cache = use_cache)$patient_days

  # ATC5-level totals
  atc5_totals <- summarise_substance_days(x, level = "atc5", use_cache = use_cache) |>
    dplyr::rename(n = "days") |>
    dplyr::mutate(
      row_id = .data$code,
      atc5_group = .data$code,
      row_type = factor("atc5", levels = c("atc5", "substance")),
      aware = factor(NA_character_,
                     levels = c("WHO_AWARE_ACCESS", "WHO_AWARE_WATCH",
                                "WHO_AWARE_RESERVE")))

  # Substance-level totals
  substance_totals <- summarise_substance_days(
    x, level = "substance", use_cache = use_cache) |>
    dplyr::rename(n = "days") |>
    dplyr::inner_join(
      x$metadata$antimicrobialSubstances |>
        dplyr::select(tidyselect::all_of(c("code", "WHO_AWARE", "ATC5"))),
      dplyr::join_by("code")) |>
    dplyr::mutate(
      row_id = .data$code,
      atc5_group = as.character(.data$ATC5),
      row_type = factor("substance", levels = c("atc5", "substance")),
      aware = .data$WHO_AWARE) |>
    dplyr::select(!c("WHO_AWARE", "ATC5"))

  # Combine and compute rates + CIs
  r <- dplyr::bind_rows(atc5_totals, substance_totals) |>
    dplyr::filter(.data$n > 0L) |>
    dplyr::mutate(
      pooled = .data$n / patient_days * 100) |>
    (\(d) dplyr::bind_cols(d, poisson_ci_cols(d$n, patient_days, multiplier = 100)))()

  if (include_quartiles) {
    # Department-level aggregation for quartiles
    dept_substance <- summarise_substance_days(
      x, level = "substance", group_cols = "department_key",
      use_cache = use_cache) |>
      dplyr::rename(n = "days", row_id = "code") |>
      dplyr::mutate(row_type = "substance")

    dept_atc5 <- summarise_substance_days(
      x, level = "atc5", group_cols = "department_key",
      use_cache = use_cache) |>
      dplyr::rename(n = "days", row_id = "code") |>
      dplyr::mutate(row_type = "atc5")

    dept_patient_days <- get_risk_time(
      x, group_cols = "department_key", use_cache = use_cache) |>
      dplyr::select("department_key", "patient_days")

    dept_rates <- dplyr::bind_rows(dept_substance, dept_atc5) |>
      dplyr::inner_join(dept_patient_days, dplyr::join_by("department_key")) |>
      dplyr::mutate(rate = .data$n / .data$patient_days * 100)

    n_deps <- nrow(dept_patient_days)
    median_patient_days <- stats::median(dept_patient_days$patient_days)

    # Compute quartiles per row_id
    quartiles <- dept_rates |>
      dplyr::group_by(.data$row_id) |>
      dplyr::reframe(
        q = list(stats::quantile(.data$rate, probs = quartile_probs,
                                 names = FALSE))) |>
      tidyr::unnest_wider("q", names_sep = "") |>
      ensure_quartile_cols()

    r <- r |>
      dplyr::left_join(quartiles, dplyr::join_by("row_id")) |>
      dplyr::mutate(
        drop_quartiles = tidyr::replace_na(
          n_deps < 5 | round(100 / .data$pooled) >= median_patient_days,
          TRUE),
        q1 = dplyr::if_else(.data$drop_quartiles, NA, .data$q1),
        q2 = dplyr::if_else(.data$drop_quartiles, NA, .data$q2),
        q3 = dplyr::if_else(.data$drop_quartiles, NA, .data$q3))

    # Bootstrap CIs for quartiles where gate passes
    if (nrow(r) > 0L) {
      boot_cis <- r |>
        dplyr::group_by(.data$row_id) |>
        dplyr::group_map(~ {
          ci <- if (.x$drop_quartiles[1]) {
            tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                           q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                           q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
          } else {
            d <- dept_rates |> dplyr::filter(.data$row_id == .y$row_id)
            bootstrap_quantile_ci(d$n, d$patient_days,
                                  type = "poisson", multiplier = 100)
          }
          dplyr::bind_cols(.y, ci)
        }) |>
        dplyr::bind_rows()

      r <- r |>
        dplyr::left_join(boot_cis, by = "row_id")
    }

    r <- r |> dplyr::select(!"drop_quartiles")
  }

  # Sort: by ATC5 group, then ATC5 header before substances, then by code
  r |>
    dplyr::arrange(.data$atc5_group, .data$row_type, .data$row_id) |>
    dplyr::select("row_id", "atc5_group", "row_type", "display_name", "aware",
                  "n", "pooled", "ci_lower", "ci_upper",
                  tidyselect::any_of(c("q1", "q2", "q3",
                    "q1_ci_lower", "q1_ci_upper",
                    "q2_ci_lower", "q2_ci_upper",
                    "q3_ci_lower", "q3_ci_upper"))) |>
    cache(x, cache_key)
}

#' Get the table with rates of surgical procedures
#'
#' @param x A `neoipcr_ds` dataset.
#' @param use_cache Use the per-dataset cache to short-circuit recomputation.
#'
#' @returns A tibble of surgical-procedure rates per category, with pooled rate
#'  and CI columns.
#' @export
get_surgery_rate_table <- function(x, use_cache = TRUE) {
  # ToDo: Class for unit dataset
  check_neoipcr_ds(x)

  # if(is_neoipcr_ref_ds(x))
  #   return(x$surgery_rate_table)

  if(use_cache && !is.null(r <- get_cached(x, "surgery_rate_table")))
    return(r)

  tibble::tibble(
    pro_cat = "overall",
    n = get_procedures(x, use_cache = use_cache) |>
      dplyr::pull()) |>
    dplyr::bind_rows(
      get_procedures(
        x,
        group_cols = "pro_cat",
        use_cache = use_cache)
    ) |>
    dplyr::bind_cols(
      get_risk_population(x, use_cache = use_cache) |>
        dplyr::select("n_patients")
    ) |>
    dplyr::mutate(pooled = .data$n / .data$n_patients * 100) |>
    (\(d) dplyr::bind_cols(d, poisson_ci_cols(d$n, d$n_patients, multiplier = 100)))() |>
    dplyr::select(!"n_patients") |>
    add_class("neoipcr_tbl_sr") |>
    cache(x, "surgery_rate_table")
}


#' Get the reference table with rates of surgical procedures
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing the reference rates of surgical procedures and
#'  the 25%, 50%, and 75% quantiles
#' @export
get_ref_surgery_rate_table <- function(ref, use_cache = TRUE) {
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$surgery_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "ref_surgery_rate_table")))
    return(r)

  pats_per_dept <- ref |>
    get_risk_population(
      group_cols = "department_key",
      use_cache = use_cache) |>
    dplyr::select("department_key", "n_patients")

  n_deps <- nrow(pats_per_dept)
  median_patients <- stats::median(dplyr::pull(pats_per_dept, "n_patients"))

  r <- ref |>
    get_surgery_rate_table(use_cache = use_cache)

  if(nrow(r) == 1 && r$n == 0)
    return(
      dplyr::bind_cols(r, q1 = NA_real_, q2 = NA_real_, q3 = NA_real_) |>
        add_class("neoipcr_tbl_sr_ref") |>
        cache(ref, "ref_surgery_rate_table"))

  # Department-level procedure data: n procedures + n_patients per dept per category
  dept_rates <- get_procedures(
    ref,
    group_cols = c("department_key", "pro_cat"),
    use_cache = use_cache) |>
    dplyr::bind_rows(
      get_procedures(
        ref,
        group_cols = "department_key",
        use_cache = use_cache)) |>
    dplyr::right_join(
      pats_per_dept,
      dplyr::join_by("department_key")) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      pro_cat = tidyr::replace_na(
        as.character(.data$pro_cat), "overall"),
      pooled = .data$n / .data$n_patients * 100)

  # Compute quartiles from dept-level rates
  quartiles <- dept_rates |>
    dplyr::select(!c("n","n_patients")) |>
    tidyr::pivot_wider(
      names_from = "pro_cat",
      values_from = "pooled",
      values_fill = 0) |>
    dplyr::select(!"department_key") |>
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
    dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
    tidyr::pivot_wider(values_from = !"name") |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_pattern = "^(.+)_(q(?:1|2|3))$",
      names_to = c("pro_cat",".value"))

  r <- r |>
    dplyr::inner_join(quartiles, dplyr::join_by("pro_cat")) |>
    dplyr::mutate(
      drop_quartiles = tidyr::replace_na(
        n_deps < 5 | round(100 / .data$pooled) >= median_patients,
        TRUE),
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3))

  # Bootstrap CIs for quartiles where gate passes
  na_boot <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                            q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                            q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
  boot_cis <- r |>
    dplyr::group_by(.data$pro_cat) |>
    dplyr::group_map(~ {
      ci <- if (.x$drop_quartiles[1]) {
        na_boot
      } else {
        d <- dept_rates |>
          dplyr::filter(.data$pro_cat == .y$pro_cat, .data$n_patients > 0)
        if (nrow(d) < 2) na_boot
        else bootstrap_quantile_ci(d$n, d$n_patients,
                                    type = "poisson", multiplier = 100)
      }
      dplyr::bind_cols(.y, ci)
    }) |>
    dplyr::bind_rows()

  r |>
    dplyr::left_join(boot_cis, by = "pro_cat") |>
    dplyr::select(!"drop_quartiles") |>
    add_class("neoipcr_tbl_sr_ref") |>
    cache(ref, "ref_surgery_rate_table")
}

#' Get the table with incidence density rates of the infections with time
#'  dependent risks
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing incidence density rates of the infections with
#'  time dependent risks
#' @export
get_incidence_density_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "incidence_density_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  r <- x |>
    get_incidence_density_rates(use_cache = use_cache) |>
    dplyr::rename("pooled" = "rate")
  r <- r |>
    dplyr::bind_cols(poisson_ci_cols(r$n, r$patient_days, multiplier = 1000)) |>
    dplyr::select(!"patient_days") |>
    add_class("neoipcr_tbl_idr")

  if(include_quartiles) {
    # Calculate quartiles only when needed
    pat_days <- x |>
      get_risk_time(group_cols = "department_key", use_cache = use_cache) |>
      dplyr::pull("patient_days")

    n_deps <- length(pat_days)
    median_patient_days <- stats::median(pat_days)

    dept_rates <- x |>
      get_incidence_density_rates(
        group_cols = "department_key",
        use_cache = use_cache) |>
      dplyr::mutate(
        rate = tidyr::replace_na(.data$rate, 0))

    r <- r |>
      dplyr::full_join(
        dept_rates |>
          dplyr::group_by(.data$inf) |>
          dplyr::summarise(
            q = list(
              stats::quantile(
                .data$rate,
                probs = quartile_probs,
                names = FALSE))) |>
          tidyr::unnest_wider("q", names_sep = ""),
        dplyr::join_by("inf")) |>
      ensure_quartile_cols() |>
      dplyr::mutate(
        drop_quartiles = tidyr::replace_na(
          n_deps < 5 | round(1000 / .data$pooled) >= median_patient_days,
          TRUE),
        q1 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q1),
        q2 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q2),
        q3 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q3))

    # Bootstrap CIs for quartiles where gate passes
    if (nrow(r) > 0L) {
      boot_cis <- r |>
        dplyr::group_by(.data$inf) |>
        dplyr::group_map(~ {
          if (.x$drop_quartiles[1]) {
            ci <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                           q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                           q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
          } else {
            d <- dept_rates |> dplyr::filter(.data$inf == .y$inf)
            ci <- bootstrap_quantile_ci(d$n, d$patient_days,
                                  type = "poisson", multiplier = 1000)
          }
          dplyr::bind_cols(.y, ci)
        }) |>
        dplyr::bind_rows()

      r <- r |> dplyr::left_join(boot_cis, by = "inf")
    }

    r <- r |>
      dplyr::select(!"drop_quartiles") |>
      add_class("neoipcr_tbl_idr_ref")
  }

  expected_levels <- c("si","bsi","hap","nec")
  missing <- setdiff(expected_levels, r$inf)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(
      tibble::tibble(
        inf = missing,
        n = 0L,
        pooled = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_))

  r |>
    dplyr::mutate(
      inf = factor(.data$inf, levels = expected_levels)) |>
    dplyr::arrange(.data$inf) |>
    cache(x, cache_key)
}

#' Get the table with device associated incidence density rates of the
#'  infections with device associated risks
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing device associated incidence density rates of the
#'  infections with device associated risks
#' @export
get_dev_ass_incidence_density_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "dev_ass_incidence_density_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  r <- x |>
    get_dev_ass_incidence_density_rates(use_cache = use_cache)
  r <- r |>
    dplyr::bind_cols(poisson_ci_cols(r$n, r$days, multiplier = 1000)) |>
    dplyr::select(!"days") |>
    add_class("neoipcr_tbl_daidr")

  if(include_quartiles) {
    dev_days <- x |>
      get_risk_time(group_cols = "department_key", use_cache = use_cache) |>
      dplyr::select(
        tidyselect::any_of(
          c("department_key","cvc"="cvc_days","pvc"="pvc_days","vs"="vs_days",
            "inv"="inv_days","niv"="niv_days")))

    dep_stats <- dev_days |>
      tidyr::pivot_longer(cols = !"department_key", names_to = "dev") |>
      dplyr::filter(.data$value > 0) |>
      dplyr::group_by(.data$dev) |>
      dplyr::summarise(n_deps = dplyr::n()) |>
      dplyr::inner_join(
        dev_days |>
          dplyr::select(!"department_key") |>
          dplyr::summarise(dplyr::across(tidyselect::everything(), stats::median)) |>
          tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_to = "dev",
            values_to = "median"),
        dplyr::join_by("dev")
      )

    dept_rates <- x |>
      get_dev_ass_incidence_density_rates(
        group_cols = "department_key",
        use_cache = use_cache) |>
      dplyr::mutate(
        rate = tidyr::replace_na(.data$rate, 0))

    r <- r |>
      dplyr::full_join(
        dept_rates |>
          dplyr::group_by(.data$dev) |>
          dplyr::summarise(
            q = list(
              stats::quantile(
                .data$rate,
                probs = quartile_probs,
                names = FALSE))) |>
          tidyr::unnest_wider("q", names_sep = ""),
        dplyr::join_by("dev")) |>
      dplyr::full_join(dep_stats, dplyr::join_by("dev")) |>
      ensure_quartile_cols() |>
      dplyr::mutate(
        drop_quartiles = tidyr::replace_na(
          .data$n_deps < 5 | round(1000 / .data$rate) >= .data$median,
          TRUE),
        q1 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q1),
        q2 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q2),
        q3 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q3))

    # Bootstrap CIs for quartiles where gate passes
    boot_cis <- r |>
      dplyr::group_by(.data$dev) |>
      dplyr::group_map(~ {
        if (.x$drop_quartiles[1]) {
          ci <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                         q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                         q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
        } else {
          d <- dept_rates |> dplyr::filter(.data$dev == .y$dev,
                                                    .data$days > 0)
          ci <- bootstrap_quantile_ci(d$n, d$days,
                                type = "poisson", multiplier = 1000)
        }
        dplyr::bind_cols(.y, ci)
      }) |>
      dplyr::bind_rows()

    r <- r |>
      dplyr::left_join(boot_cis, by = "dev") |>
      dplyr::select(!c("drop_quartiles","n_deps","median")) |>
      add_class("neoipcr_tbl_daidr_ref")
  }

  expected_levels <- c("cvc","pvc","vs","inv","niv")
  missing <- setdiff(expected_levels, r$dev)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(
      tibble::tibble(
        dev = missing,
        n = 0L,
        pooled = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_))

  r |>
    dplyr::rename("pooled" = "rate") |>
    dplyr::mutate(dev = factor(.data$dev, expected_levels)) |>
    dplyr::arrange(.data$dev) |>
    cache(x, cache_key)
}

#' Get the table with infectious agent detection rates per type of infection
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing infectious agent detection rates per type of
#'  infection
#' @export
get_infectious_agent_detection_rate_per_inf_type_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "infectious_agent_detection_rate_per_inf_type_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  r <- dplyr::bind_rows(
    dplyr::bind_cols(
      event_type_key = "all",
      x |>
        get_infectious_agent_detection_rates(
          use_cache = use_cache) |>
        dplyr::select("inf_with_pathogen","total_inf","pooled"="iwp_per_t")),
    x |>
      get_infectious_agent_detection_rates(
        group_cols = "event_type_key",
        use_cache = use_cache) |>
      dplyr::select("event_type_key","inf_with_pathogen","total_inf","pooled"="iwp_per_t")
    )
  r <- r |>
    dplyr::bind_cols(wilson_ci_cols(r$inf_with_pathogen, r$total_inf, scale = 100)) |>
    dplyr::select(!"total_inf") |>
    add_class("neoipcr_tbl_iadrpit")

  if(include_quartiles) {

    dep_stats <- dplyr::bind_rows(
      dplyr::bind_cols(
        event_type_key = "all",
        x |>
          get_infection_counts(group_cols = "department_key") |>
          dplyr::filter(.data$n > 0)
        ),
      x |>
        get_infection_counts(group_cols = c("event_type_key","department_key")) |>
        dplyr::filter(.data$n > 0)
      ) |>
      dplyr::group_by(.data$event_type_key) |>
      dplyr::summarise(
        median = stats::median(.data$n),
        n = dplyr::n())

    dept_rates <- x |>
      get_infectious_agent_detection_rates(
        group_cols = "department_key",
        use_cache = use_cache) |>
      dplyr::bind_cols(event_type_key = "all") |>
      dplyr::bind_rows(
        x |>
          get_infectious_agent_detection_rates(
            group_cols = c("department_key","event_type_key"),
            use_cache = use_cache))

    r <- r |>
      dplyr::inner_join(
        dept_rates |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::summarise(
            q = list(
              stats::quantile(
                .data$iwp_per_t,
                probs = quartile_probs,
                # In this case NaN indicates that the department has not even
                # reported an infection.
                # Since infections with pathogens per total infections only
                # makes sense for departments with infections we remove those
                # with NaN here.
                na.rm = TRUE,
                names = FALSE))) |>
          tidyr::unnest_wider("q", names_sep = ""),
        dplyr::join_by("event_type_key")) |>
      dplyr::inner_join(
        dep_stats,
        dplyr::join_by("event_type_key")) |>
      ensure_quartile_cols() |>
      dplyr::mutate(
        drop_quartiles = tidyr::replace_na(
          .data$n < 5 | round(100 / .data$pooled) >= .data$median,
          TRUE),
        q1 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q1),
        q2 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q2),
        q3 = dplyr::if_else(
          .data$drop_quartiles,
          NA,
          .data$q3))

    # Bootstrap CIs for quartiles where gate passes
    if (nrow(r) > 0L) {
      boot_cis <- r |>
        dplyr::group_by(.data$event_type_key) |>
        dplyr::group_map(~ {
          if (.x$drop_quartiles[1]) {
            ci <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                           q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                           q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
          } else {
            d <- dept_rates |>
              dplyr::filter(.data$event_type_key == .y$event_type_key,
                            .data$total_inf > 0)
            ci <- bootstrap_quantile_ci(d$inf_with_pathogen, d$total_inf,
                                  type = "binomial", multiplier = 100)
          }
          dplyr::bind_cols(.y, ci)
        }) |>
        dplyr::bind_rows()

      r <- r |> dplyr::left_join(boot_cis, by = "event_type_key")
    }

    r <- r |>
      dplyr::select(!tidyselect::any_of(c("drop_quartiles", "n", "median"))) |>
      add_class("neoipcr_tbl_iadrpit_ref")
  }
  expected_levels <- c("all","bsi","hap","nec","ssi")
  missing <- setdiff(expected_levels, r$event_type_key)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(
      tibble::tibble(
        event_type_key = missing,
        inf_with_pathogen = 0L,
        pooled = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_))

  r |>
    dplyr::rename("n"="inf_with_pathogen") |>
    dplyr::mutate(
      inf = factor(.data$event_type_key, levels = expected_levels),
      .before = 1,
      .keep = "unused") |>
    dplyr::arrange(.data$inf) |>
    cache(x, cache_key)
}

#' Get the table with infectious agent detection rates of the pathogens in a
#'  somewhat meaningful taxonomic structure
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing infectious agent detection rates
#' @export
get_infectious_agent_detection_rate_per_agent_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "infectious_agent_detection_rate_per_agent_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  # Choose the appropriate helper function, the columns to include and the class
  # to return based on include_quartiles
  if(include_quartiles) {
    get_rates <- function(...) {
      get_infectious_agent_detection_rates_with_department_quartiles(...)
    }
    rate_cols <- c("n","inf_with_pathogen","rate","q1","q2","q3",
                   "q1_ci_lower","q1_ci_upper","q2_ci_lower","q2_ci_upper",
                   "q3_ci_lower","q3_ci_upper")
    return_class <- c("neoipcr_tbl_iadrpa_ref", "neoipcr_tbl_iadrpa")
  } else {
    get_rates <- function(...) {
      get_infectious_agent_detection_rates(...) |>
        dplyr::rename(rate = "n_per_iwp")
    }
    rate_cols <- c("n","inf_with_pathogen","rate")
    return_class <- "neoipcr_tbl_iadrpa"
  }

  lv0 <- dplyr::bind_cols(
    lv = 0L,
    tl = "none",
    .ontology_path = "",
    group = "Total",
    x |>
      get_rates(use_cache = use_cache) |>
      dplyr::select(tidyselect::all_of(rate_cols)))
  d <- x |>
    get_rates(
      group_cols = "domain",
      use_cache = use_cache) |>
    dplyr::select("group"="domain",tidyselect::all_of(rate_cols)) |>
    dplyr::arrange(dplyr::desc(.data$rate))
  for (i in seq_len(nrow(d))) {
    di <- d[i,]
    if(!is.na(di$group) && di$group == "Bacteria") {
      op_domain <- di$group
      lv1 <- dplyr::bind_cols(lv = 1L, tl = "domain",
        .ontology_path = op_domain, di)
      o <- x |>
        get_rates(
          group_cols = c("domain","order"),
          use_cache = use_cache) |>
        dplyr::filter(.data$domain == di$group) |>
        dplyr::select("group"="order",tidyselect::all_of(rate_cols)) |>
        dplyr::arrange(dplyr::desc(.data$rate))
      for (j in seq_len(nrow(o))) {
        oj <- o[j,]
        op_order <- paste0(op_domain, "|", oj$group)
        lv2 <- dplyr::bind_cols(lv = 2L, tl = "order",
          .ontology_path = op_order, oj)
        g <- x |>
          get_rates(
            group_cols = c("order","genus"),
            use_cache = use_cache) |>
          dplyr::filter(.data$order == oj$group) |>
          dplyr::select("group"="genus",tidyselect::all_of(rate_cols)) |>
          dplyr::arrange(dplyr::desc(.data$rate))
        for (k in seq_len(nrow(g))) {
          gk <- g[k,]
          op_genus <- paste0(op_order, "|", gk$group)
          lv3 <- dplyr::bind_cols(lv = 3L, tl = "genus",
            .ontology_path = op_genus,
            gk |> dplyr::mutate(group = paste(.data$group, "spp.")))
          if(gk$group == "Staphylococcus") {
            c <- x |>
              get_rates(
                group_cols = c("genus","coagulase"),
                use_cache = use_cache) |>
              dplyr::filter(.data$genus == "Staphylococcus") |>
              dplyr::select("group"="coagulase",tidyselect::all_of(rate_cols)) |>
              dplyr::arrange(dplyr::desc(.data$rate))
            for (l in seq_len(nrow(c))) {
              cl <- c[l,]
              c_text <- switch (as.character(cl$group),
                "n" = "Coagulase-negative staphylococci",
                "p" = "Coagulase-positive staphylococci",
                "Staphylococcus spp. n.o.s.")
              op_coag <- paste0(op_genus, "|",
                switch(as.character(cl$group),
                  "n" = "n", "p" = "p", "~"))
              lv4 <- dplyr::bind_cols(
                lv = 4L,
                tl = switch (as.character(cl$group), "n" = "coag_type",
                             "p" = "coag_type", "coag_type_nos"),
                .ontology_path = op_coag,
                cl |> dplyr::mutate(group = c_text))
              s <- x |>
                get_rates(
                  group_cols = c("genus","coagulase","species"),
                  use_cache = use_cache) |>
                dplyr::filter(.data$genus == "Staphylococcus" & .data$coagulase == cl$group) |>
                dplyr::select("group"="species",tidyselect::all_of(rate_cols)) |>
                dplyr::arrange(dplyr::desc(.data$rate))
              lv5 <- dplyr::bind_rows(
                dplyr::bind_cols(
                  lv = 5L,
                  tl = "species",
                  s |> dplyr::filter(!is.na(.data$group)) |>
                    dplyr::mutate(.ontology_path = paste0(
                      op_coag, "|", .data$group))),
                dplyr::bind_cols(
                  lv = 5L,
                  tl = "species_nos",
                  s |> dplyr::filter(is.na(.data$group)) |>
                    dplyr::mutate(
                      .ontology_path = paste0(op_coag, "|~"),
                      group = paste(c_text, "n.o.s."))))
              lv3 <- dplyr::bind_rows(lv3,lv4,lv5)
            }
          }
          else {
            s <- x |>
              get_rates(
                group_cols = c("genus","coagulase","species"),
                use_cache = use_cache) |>
              dplyr::filter(.data$genus == gk$group) |>
              dplyr::select("group"="species",tidyselect::all_of(rate_cols)) |>
              dplyr::arrange(dplyr::desc(.data$rate))
            lv4 <- dplyr::bind_rows(
              dplyr::bind_cols(
                lv = 4L,
                tl = "species",
                s |> dplyr::filter(!is.na(.data$group)) |>
                  dplyr::mutate(.ontology_path = paste0(
                    op_genus, "|", .data$group))),
              dplyr::bind_cols(
                lv = 4L,
                tl = "species_nos",
                s |> dplyr::filter(is.na(.data$group)) |>
                  dplyr::mutate(
                    .ontology_path = paste0(op_genus, "|~"),
                    group = paste(gk$group,"spp. n.o.s."))))
            lv3 <- dplyr::bind_rows(lv3,lv4)
          }
          lv2 <- dplyr::bind_rows(lv2,lv3)
        }
        lv1 <- dplyr::bind_rows(lv1,lv2)
      }
      lv0 <- dplyr::bind_rows(lv0,lv1)
    }
    else if (!is.na(di$group)) {
      kd <- x |>
        get_rates(
          group_cols = c("domain","kingdom"),
          use_cache = use_cache) |>
        dplyr::filter(.data$domain == di$group) |>
        dplyr::select("group"="kingdom",tidyselect::all_of(rate_cols)) |>
        dplyr::arrange(dplyr::desc(.data$rate))
      for (j in seq_len(nrow(kd))) {
        kj <- kd[j,]
        op_kingdom <- paste0(di$group, "|", kj$group)
        lv1 <- dplyr::bind_cols(lv = 1L, tl = "kingdom",
          .ontology_path = op_kingdom, kj)
        g <- x |>
          get_rates(
            group_cols = c("kingdom","genus"),
            use_cache = use_cache) |>
          dplyr::filter(.data$kingdom == kj$group) |>
          dplyr::select("group"="genus",tidyselect::all_of(rate_cols)) |>
          dplyr::arrange(dplyr::desc(.data$rate))
        for (k in seq_len(nrow(g))) {
          gk <- g[k,]
          op_genus <- paste0(op_kingdom, "|", gk$group)
          lv2 <- dplyr::bind_cols(lv = 2L, tl = "genus",
            .ontology_path = op_genus,
            gk |> dplyr::mutate(group = paste(.data$group, "spp.")))
          s <- x |>
            get_rates(
              group_cols = c("genus","coagulase","species"),
              use_cache = use_cache) |>
            dplyr::filter(.data$genus == gk$group) |>
            dplyr::select("group"="species",tidyselect::all_of(rate_cols)) |>
            dplyr::arrange(dplyr::desc(.data$rate))
          lv3 <- dplyr::bind_rows(
            dplyr::bind_cols(
              lv = 3L,
              tl = "species",
              s |> dplyr::filter(!is.na(.data$group)) |>
                dplyr::mutate(.ontology_path = paste0(
                  op_genus, "|", .data$group))),
            dplyr::bind_cols(
              lv = 3L,
              tl = "species_nos",
              s |> dplyr::filter(is.na(.data$group)) |>
                dplyr::mutate(
                  .ontology_path = paste0(op_genus, "|~"),
                  group = paste(gk$group,"spp. n.o.s."))))
          lv1 <- dplyr::bind_rows(lv1,lv2,lv3)
        }
      }
      lv0 <- dplyr::bind_rows(lv0,lv1)
    }
  }

  lv0 |>
    (\(d) dplyr::bind_cols(d, poisson_ci_cols(d$n, d$inf_with_pathogen, multiplier = 100)))() |>
    dplyr::select(!"inf_with_pathogen") |>
    dplyr::rename("level"="lv","taxon"="tl","pooled"="rate") |>
    add_class(return_class) |>
    cache(x, cache_key)
}

#' Get the table of infection rates with antibiotic resistant bacteria in a
#'  somewhat meaningful taxonomic structure
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing infection rates with antibiotic resistant
#'  bacteria
#' @export
get_abr_infection_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "abr_infection_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  # Choose the appropriate helper function, the columns to include and the class
  # to return based on include_quartiles
  if(include_quartiles) {
    get_resistance <- function(...) {
      get_resistance_rate_with_department_quartiles(...)
    }
    rate_cols <- c("n","inf_w_ia","rate","q1","q2","q3",
                   "q1_ci_lower","q1_ci_upper","q2_ci_lower","q2_ci_upper",
                   "q3_ci_lower","q3_ci_upper")
    return_class <- c("neoipcr_tbl_abr_ir_ref", "neoipcr_tbl_abr_ir")
  } else {
    get_resistance <- function(...) {
      get_resistance_rate(...) |>
        dplyr::rename("n"="inf_rs","rate"="inf_rs_rate")
    }
    rate_cols <- c("n","inf_w_ia","rate")
    return_class <- "neoipcr_tbl_abr_ir"
  }

  abr_types <- c("3gcr","car","cor")
  tbl <- NULL

  for (abr_type in abr_types) {
    t <- x |>
      get_resistance(
        resistance = abr_type,
        use_cache = use_cache) |>
      dplyr::select(tidyselect::all_of(rate_cols))

    lv0 <- dplyr::bind_cols(
      abr_type = abr_type,
      lv = 0L,
      tl = "none",
      .ontology_path = abr_type,
      group = "Total",
      t)

    if(length(t$n) > 0 && t$n > 0) {
      o <- x |>
        get_resistance(
          resistance = abr_type,
          group_cols = "order",
          use_cache = use_cache) |>
        dplyr::select("group"="order",tidyselect::all_of(rate_cols)) |>
        dplyr::arrange(dplyr::desc(.data$rate))

      for (j in seq_len(nrow(o))) {
        oj <- o[j,]

        if(oj$n > 0) {
          op_order <- paste0(abr_type, "|", oj$group)
          lv1 <- dplyr::bind_cols(
            abr_type = abr_type,
            lv = 2L,
            tl = "order",
            .ontology_path = op_order,
            oj)
          g <- x |>
            get_resistance(
              resistance = abr_type,
              group_cols = c("order","genus"),
              use_cache = use_cache) |>
            dplyr::filter(.data$order == oj$group) |>
            dplyr::select("group"="genus",tidyselect::all_of(rate_cols)) |>
            dplyr::arrange(dplyr::desc(.data$rate))

          for (k in seq_len(nrow(g))) {
            gk <- g[k,]

            if(gk$n > 0) {
              op_genus <- paste0(op_order, "|", gk$group)
              lv2 <- dplyr::bind_cols(
                abr_type = abr_type,
                lv = 3L,
                tl = "genus",
                .ontology_path = op_genus,
                gk |>
                  dplyr::mutate(group = paste(.data$group, "spp.")))
              s <- x |>
                get_resistance(
                  resistance = abr_type,
                  group_cols = c("genus","species"),
                  use_cache = use_cache) |>
                dplyr::filter(.data$genus == gk$group) |>
                dplyr::select("group"="species",tidyselect::all_of(rate_cols)) |>
                dplyr::arrange(dplyr::desc(.data$rate))
              lv3 <- dplyr::bind_rows(
                dplyr::bind_cols(
                  abr_type = abr_type,
                  lv = 4L,
                  tl = "species",
                  s |>
                    dplyr::filter(!is.na(.data$group) & .data$n > 0) |>
                    dplyr::mutate(.ontology_path = paste0(
                      op_genus, "|", .data$group))),
                dplyr::bind_cols(
                  abr_type = abr_type,
                  lv = 4L,
                  tl = "species_nos",
                  s |>
                    dplyr::filter(is.na(.data$group) & .data$n > 0) |>
                    dplyr::mutate(
                      .ontology_path = paste0(op_genus, "|~"),
                      group = paste(gk$group,"spp. n.o.s."))))
              lv2 <- dplyr::bind_rows(lv2,lv3)
              lv1 <- dplyr::bind_rows(lv1,lv2)
            }
          }
          lv0 <- dplyr::bind_rows(lv0,lv1)
          }
        }
      }
    tbl <- dplyr::bind_rows(tbl,lv0)
  }

  # MRSA only has one species
  tbl <-dplyr::bind_rows(
    tbl,
    dplyr::bind_cols(
      abr_type = "mrsa",
      lv = 0L,
      tl = "species",
      .ontology_path = "mrsa",
      x |>
        get_resistance(
          resistance = "mrsa",
          group_cols = "species",
          use_cache = use_cache) |>
        dplyr::select("group"="species",tidyselect::all_of(rate_cols))))

  # VRE is reported only for the Enterococcus genus. Non-Enterococcus or
  # NA-genus rows would otherwise produce duplicate "Enterococcus spp."
  # entries via the hardcoded label below.
  g <- x |>
    get_resistance(
      resistance = "vre",
      group_cols = "genus",
      use_cache = use_cache) |>
    dplyr::filter(.data$genus == "Enterococcus") |>
    dplyr::select("group"="genus",tidyselect::all_of(rate_cols)) |>
    dplyr::mutate(group = "Enterococcus spp.")

  lv0 <- dplyr::bind_cols(
    abr_type = "vre",
    lv = 0L,
    tl = "genus",
    .ontology_path = "vre",
    g)

  if(length(g$n) > 0 && g$n > 0) {
    s <- x |>
      get_resistance(
        resistance = "vre",
        group_cols = c("species"),
        use_cache = use_cache) |>
      dplyr::select("group"="species",tidyselect::all_of(rate_cols)) |>
      dplyr::arrange(dplyr::desc(.data$rate))

    lv1 <- dplyr::bind_rows(
      dplyr::bind_cols(
        abr_type = "vre",
        lv = 1L,
        tl = "species",
        s |>
          dplyr::filter(!is.na(.data$group) & .data$n > 0) |>
          dplyr::mutate(.ontology_path = paste0("vre|", .data$group))),
      dplyr::bind_cols(
        abr_type = "vre",
        lv = 1L,
        tl = "species_nos",
        s |>
          dplyr::filter(is.na(.data$group) & .data$n > 0) |>
          dplyr::mutate(
            .ontology_path = "vre|~",
            group = paste(g$group," n.o.s."))))

    lv0 <- dplyr::bind_rows(lv0,lv1)
  }

  tbl <-dplyr::bind_rows(tbl,lv0)

  tbl |>
    (\(d) dplyr::bind_cols(d, wilson_ci_cols(d$n, d$inf_w_ia, scale = 100)))() |>
    dplyr::select(!"inf_w_ia") |>
    dplyr::rename("abr"="abr_type","level"="lv","taxon"="tl","pooled"="rate") |>
    add_class(return_class) |>
    cache(x, cache_key)
}

#' Get a table with organism-specific resistance rates
#'
#' Unlike the ABR infection rate table which uses infection-level denominators
#' (infections with resistant organism / all infections with a pathogen), this
#' table uses organism-level denominators (resistant detections of organism X /
#' all tested detections of organism X). This answers the question "what
#' fraction of K. pneumoniae is carbapenem-resistant?"
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include the quartile columns
#'
#' @returns A tibble with organism-specific resistance rates grouped by
#'  resistance type
#' @export
get_organism_resistance_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "organism_resistance_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  if(include_quartiles) {
    get_resistance <- function(...) {
      get_organism_resistance_rate_with_department_quartiles(...)
    }
    rate_cols <- c("n","ia_tst_tot","rate","q1","q2","q3",
                   "q1_ci_lower","q1_ci_upper","q2_ci_lower","q2_ci_upper",
                   "q3_ci_lower","q3_ci_upper")
    return_class <- c("neoipcr_tbl_org_rr_ref", "neoipcr_tbl_org_rr")
  } else {
    get_resistance <- function(...) {
      get_resistance_rate(...) |>
        dplyr::rename("n"="ia_rs","rate"="ia_rs_rate")
    }
    rate_cols <- c("n","ia_tst_tot","rate")
    return_class <- "neoipcr_tbl_org_rr"
  }

  abr_types <- c("3gcr","car","cor","mrsa","vre")
  tbl <- NULL

  for (abr_type in abr_types) {
    g <- x |>
      get_resistance(
        resistance = abr_type,
        group_cols = c("genus","species"),
        use_cache = use_cache) |>
      dplyr::filter(.data$ia_tst_tot >= 1)

    if (nrow(g) < 1) next

    for (k in seq_len(nrow(g))) {
      gk <- g[k,]
      genus_name <- gk$genus
      species_name <- gk$species
      op_genus <- paste0(abr_type, "|", genus_name)

      if (!is.na(species_name)) {
        lv <- 1L
        tl <- "species"
        op <- paste0(op_genus, "|", species_name)
        grp <- species_name
      } else {
        lv <- 1L
        tl <- "species_nos"
        op <- paste0(op_genus, "|~")
        grp <- paste(genus_name, "spp. n.o.s.")
      }

      row <- dplyr::bind_cols(
        abr_type = abr_type,
        lv = lv,
        tl = tl,
        .ontology_path = op,
        group = grp,
        gk |> dplyr::select(tidyselect::all_of(rate_cols)))

      tbl <- dplyr::bind_rows(tbl, row)
    }

    # Add a genus summary row for every genus represented in g, so that every
    # species and species_nos row has a visible parent in the rendered table.
    unique_genera <- unique(g$genus)

    if (length(unique_genera) > 0) {
      genus_summary <- x |>
        get_resistance(
          resistance = abr_type,
          group_cols = "genus",
          use_cache = use_cache) |>
        dplyr::filter(.data$genus %in% unique_genera)

      for (j in seq_len(nrow(genus_summary))) {
        gs <- genus_summary[j,]
        genus_row <- dplyr::bind_cols(
          abr_type = abr_type,
          lv = 0L,
          tl = "genus",
          .ontology_path = paste0(abr_type, "|", gs$genus),
          group = paste(gs$genus, "spp."),
          gs |> dplyr::select(tidyselect::all_of(rate_cols)))
        tbl <- dplyr::bind_rows(tbl, genus_row)
      }
    }
  }

  if (is.null(tbl) || nrow(tbl) == 0) {
    tbl <- tibble::tibble(
      abr_type = character(),
      lv = integer(),
      tl = character(),
      .ontology_path = character(),
      group = character(),
      n = numeric(),
      ia_tst_tot = numeric(),
      rate = numeric())
    if (include_quartiles)
      tbl <- tbl |>
        dplyr::mutate(
          q1 = numeric(), q2 = numeric(), q3 = numeric(),
          q1_ci_lower = numeric(), q1_ci_upper = numeric(),
          q2_ci_lower = numeric(), q2_ci_upper = numeric(),
          q3_ci_lower = numeric(), q3_ci_upper = numeric())
  }

  # Sort: within each abr_type, genus summary rows first, then species rows
  # sorted by rate descending
  tbl <- tbl |>
    dplyr::arrange(
      factor(.data$abr_type, levels = c("3gcr","car","cor","mrsa","vre")),
      .data$.ontology_path,
      -.data$rate)

  tbl |>
    (\(d) dplyr::bind_cols(d, wilson_ci_cols(d$n, d$ia_tst_tot, scale = 100)))() |>
    dplyr::rename("abr"="abr_type","level"="lv","taxon"="tl","pooled"="rate") |>
    add_class(return_class) |>
    cache(x, cache_key)
}

#' Get the table with resistance test rates of the recorded resistance
#'  mechanisms
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing resistance test rates
#' @export
get_resistance_test_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "resistance_test_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  if(include_quartiles) {
    test_fn <- get_resistance_test_rate_with_department_quartiles
    return_class <- c("neoipcr_tbl_rtr_ref", "neoipcr_tbl_rtr")
  } else {
    test_fn <- get_resistance_test_rate
    return_class <- "neoipcr_tbl_rtr"
  }

  c("3gcr","car","cor","mrsa","vre") |>
    lapply(\(r) dplyr::bind_cols(res = r, type = "routine", test_fn(x, r))) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(
      x |>
        test_fn(
          resistance = "car",
          group_cols = "3gcr") |>
        dplyr::filter(.data$`3gcr` == "yes") |>
        dplyr::mutate(type = "if_3gcr", res = "car")) |>
    dplyr::bind_rows(
      x |>
        test_fn(
          resistance = "cor",
          group_cols = c("3gcr","car")) |>
        dplyr::filter(.data$`3gcr` == "yes" & .data$car == "yes") |>
        dplyr::mutate(type = "if_3gcr&car", res = "cor")) |>
    dplyr::mutate(
      abr = factor(.data$res, levels = c("3gcr","car","cor","mrsa","vre")),
      cond = factor(.data$type, levels = c("routine","if_3gcr","if_3gcr&car")),
      .keep = "unused"
    ) |>
    dplyr::select("abr","cond","n"="tested","total","pooled"="rate",
                  tidyselect::any_of(c("q1","q2","q3",
                    "q1_ci_lower","q1_ci_upper","q2_ci_lower","q2_ci_upper",
                    "q3_ci_lower","q3_ci_upper"))) |>
    (\(d) dplyr::bind_cols(d, wilson_ci_cols(d$n, d$total, scale = 100)))() |>
    dplyr::select(!"total") |>
    (\(r) {
      expected <- tibble::tibble(
        abr = factor(
          c("3gcr","car","cor","mrsa","vre","car","cor"),
          levels = c("3gcr","car","cor","mrsa","vre")),
        cond = factor(
          c("routine","routine","routine","routine","routine","if_3gcr","if_3gcr&car"),
          levels = c("routine","if_3gcr","if_3gcr&car")))
      missing <- dplyr::anti_join(expected, r, by = c("abr", "cond"))
      if (nrow(missing) > 0) {
        missing_rows <- missing |>
          dplyr::mutate(n = 0L, pooled = NA_real_,
                        ci_lower = NA_real_, ci_upper = NA_real_)
        if ("q1" %in% names(r)) {
          missing_rows <- missing_rows |>
            dplyr::mutate(q1 = NA_real_, q2 = NA_real_, q3 = NA_real_)
        }
        r <- dplyr::bind_rows(r, missing_rows)
      }
      r
    })() |>
    dplyr::arrange(.data$abr, .data$cond) |>
    add_class(return_class) |>
    cache(x, cache_key)
}

#' Get the table secondary BSI rates of the recorded infections
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing secondary BSI rates
#' @export
get_secondary_bsi_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "secondary_bsi_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles)))
    return(r)

  # Calculate pooled rates
  r <- x |>
    get_secondary_bsi_rates(use_cache = use_cache) |>
    dplyr::rename("pooled" = "rate")
  r <- r |>
    dplyr::bind_cols(wilson_ci_cols(r$n, r$followup_n, scale = 100)) |>
    dplyr::select(!"followup_n") |>
    add_class("neoipcr_tbl_sec_bsi")

  # Calculate quartiles if needed
  if(include_quartiles) {
    # Get department-level data for quartile calculation
    dept_rates <- x |>
      get_secondary_bsi_rates(
        group_cols = "department_key",
        use_cache = use_cache)

    # Check if we have enough departments
    n_deps <- dept_rates |>
      dplyr::pull("department_key") |>
      dplyr::n_distinct()

    # Calculate median infections with sec BSI per department
    median_n <- dept_rates |>
      dplyr::group_by(.data$department_key) |>
      dplyr::summarise(total_n = sum(.data$n), .groups = "drop") |>
      dplyr::pull("total_n") |>
      stats::median()

    if (nrow(dept_rates) < 1) {
      r <- r |>
        dplyr::mutate(q1 = NA_real_, q2 = NA_real_, q3 = NA_real_) |>
        add_class("neoipcr_tbl_sec_bsi_ref")
    } else {
      # Calculate quartiles for each infection type
      quartiles <- dept_rates |>
        tidyr::pivot_wider(
          id_cols = "department_key",
          names_from = "event_type_key",
          values_from = "rate") |>
        dplyr::select(!"department_key") |>
        dplyr::reframe(
          dplyr::across(
            tidyselect::everything(),
            ~stats::quantile(.x, prob = c(.25, .5, .75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(Q = c("q1", "q2", "q3"))) |>
        tidyr::pivot_longer(!"Q", names_to = "event_type_key") |>
        tidyr::pivot_wider(names_from = "Q", values_from = "value")

      # Determine if quartiles should be dropped
      r <- r |>
        dplyr::mutate(
          drop_quartiles = tidyr::replace_na(
            n_deps < 5 | round(100 / .data$pooled) >= median_n,
            TRUE)) |>
        dplyr::left_join(quartiles, dplyr::join_by("event_type_key")) |>
        dplyr::mutate(
          dplyr::across(
            c("q1", "q2", "q3"),
            ~dplyr::if_else(.data$drop_quartiles, NA_real_, .x)))

      # Bootstrap CIs for quartiles where gate passes
      if (nrow(r) > 0L) {
        boot_cis <- r |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::group_map(~ {
            if (.x$drop_quartiles[1]) {
              ci <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                             q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                             q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
            } else {
              d <- dept_rates |>
                dplyr::filter(.data$event_type_key == .y$event_type_key)
              ci <- bootstrap_quantile_ci(d$n, d$followup_n,
                                    type = "binomial", multiplier = 100)
            }
            dplyr::bind_cols(.y, ci)
          }) |>
          dplyr::bind_rows()

        r <- r |> dplyr::left_join(boot_cis, by = "event_type_key")
      }

      r <- r |>
        dplyr::select(!"drop_quartiles") |>
        add_class("neoipcr_tbl_sec_bsi_ref")
    }
  }

  # Ensure all expected infection types are present
  expected_types <- c("nec", "hap", "ssi")
  missing <- setdiff(expected_types, r$event_type_key)
  if(length(missing) > 0) {
    missing_rows <- tibble::tibble(
      event_type_key = missing,
      n = 0L,
      pooled = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_)
    if(include_quartiles) {
      missing_rows <- missing_rows |>
        dplyr::mutate(q1 = NA_real_, q2 = NA_real_, q3 = NA_real_)
    }
    r <- r |> dplyr::bind_rows(missing_rows)
  }

  # Convert to factor and sort
  r |>
    dplyr::mutate(
      event_type_key = factor(.data$event_type_key, levels = expected_types)) |>
    dplyr::arrange(.data$event_type_key) |>
    cache(x, cache_key)
}
