get_birthweight_figure_data <- function(x) {
  # Birth weight is an optional attribute (mandatory = false); the
  # distribution figure covers only patients with a recorded value.
  birth_weight <- x$patients$birth_weight
  birth_weight <- birth_weight[!is.na(birth_weight)]

  bw_quartiles <- birth_weight |>
    stats::quantile(names = FALSE) |>
    as.integer()

  bw_mean = birth_weight |>
    mean() |>
    as.integer()

  bw_scale_min <- as.integer(bw_quartiles[1] / 50L) * 50L - 50L
  bw_scale_max <- as.integer(bw_quartiles[5] / 50L) * 50L + 100L

  # density.default needs at least two points
  if (length(birth_weight) > 1) {
    bw_density <- birth_weight |>
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
      birth_weight_cat = birth_weight |>
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
  # Gestational age is an optional attribute (mandatory = false); the
  # distribution figure covers only patients with a recorded value. A
  # program rule guarantees birth weight or gestational age is present,
  # but either one alone can be missing.
  total_gestation_days <- x$patients$total_gestation_days
  total_gestation_days <- total_gestation_days[!is.na(total_gestation_days)]

  ga_quartiles <- total_gestation_days |>
    stats::quantile(names = FALSE) |>
    as.integer()

  ga_mean = total_gestation_days |>
    mean() |>
    as.integer()

  ga_scale_min = as.integer(ga_quartiles[1] / 7L) * 7L - 7L
  ga_scale_max = as.integer(ga_quartiles[5] / 7L) * 7L + 14L

  # density.default needs at least two points
  if (length(total_gestation_days) > 1) {
    ga_density <- total_gestation_days |>
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
      gestational_age_cat = total_gestation_days |>
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

# The departments' quartiles of a rate or share as `.department_quartiles()`
# gives them: the quartiles, their bootstrap intervals, and the number of
# departments that enter them.
department_quartile_cols <- c(
  "q1", "q2", "q3",
  "q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
  "q3_ci_lower", "q3_ci_upper", "quartile_departments")

# The columns a table without quartiles leaves out: the departments'
# quartiles, the secondary-BSI table's quartiles of the departments'
# follow-up shares, and the departments' quartiles of the shares tested in the
# table by infection type.
quartile_cols <- c(
  department_quartile_cols,
  "followup_q1", "followup_q2", "followup_q3",
  paste0("tested_", department_quartile_cols))

# `include_event` is the event tiers the table accepts: the infectious-agent
# tables read the findings' `pathogen_key`, which only the full tier carries.
check_ds_and_try_get_table <- function(
    x, table_name, use_cache, include_quartiles,
    include_event = c("pseudo", "full")) {
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
      include_event      = include_event
    ), reasons = if (identical(include_event, "full"))
      calculation_option_reasons() else list(),
    fn_name = table_name)

  # First try, if it's a report dataset because in that case it already contains
  # the table and we can just return it (potentially after removing the quartile
  # columns)
  if(is_neoipcr_rep_ds(x))
  {
    # NEOIPC-PERMANENT(dataset-format): never drop this refusal. A calculated
    # dataset serialized before one of its tables existed has no slot for it,
    # and a file on disk outlives the code that wrote it; the dataset holds
    # nothing the table could be computed from, so without the refusal the
    # builder fails far from the cause.
    if (is.null(x[[table_name]]))
      rlang::abort(
        c(gettextf("The calculated dataset has no table %s.",
                   paste0("`", table_name, "`")),
          "i" = gettext("It was calculated by a version of neoipcr that did not compute this table; calculate it again from its raw dataset.")),
        class = "neoipcr_table_not_in_dataset")

    if(!include_quartiles && is_neoipcr_ref_ds(x))
      return(
        x[[table_name]] |>
          dplyr::select(!tidyselect::any_of(quartile_cols)))

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
            dplyr::select(!tidyselect::any_of(quartile_cols)))
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

#' Get the antibiotic utilization table with ATC5/substance-level rates
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
get_antibiotic_utilization_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "antibiotic_utilization_table"
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

#' Get the table with the cumulative incidence of infection per admission window
#'
#' The cumulative incidence is the share of the patients (or admissions)
#' admitted to a department within a calendar window who acquired at least one
#' infection of the given types within that same window. It complements the
#' incidence density tables, which relate infections to patient days rather
#' than to the admitted cohort, and is the measure a trial asks for when its
#' study periods are calendar windows per site.
#'
#' The default `event_types`, primary sepsis/BSI plus pneumonia, is the
#' "severe infection" composite (`si`) that
#' [get_incidence_density_rate_table()] reports. The confidence interval is the
#' Wilson score interval, the house method for a proportion of discrete
#' entities.
#'
#' @param x A `neoipcr_ds` object imported with `include_enrollment = "full"`
#'  and `include_event = "full"`, and with `include_patient` and
#'  `include_department` not `"no"`: the admission date, the event date and
#'  the department link are what this computation reads. The admission date
#'  is the enrollment date (`enrolledAt`): in the NeoIPC data model the
#'  enrollment is the admission, and an admission event dated differently is
#'  a validation error (rule 3) that a validation-clean import does not
#'  contain. Under `include_invalid_patients = TRUE` such an admission is
#'  placed by its enrollment date, and an infection dated before its
#'  admission — another validation error — does not count.
#' @param windows A data frame with one row per window: `department_key`
#'  (integer, never `NA`), `window` (a label such as `"baseline"`), and the
#'  inclusive `start` and `end` dates (`Date`). Windows may overlap; an
#'  admission is counted in every window it falls into. Grouping is ignored
#'  and further columns are dropped. An admission without an admission date,
#'  or an infection without an event date (possible under
#'  `include_incomplete`), falls into no window.
#' @param event_types The event types that count as an infection, a subset of
#'  `"bsi"`, `"nec"`, `"hap"` and `"ssi"`.
#' @param unit Whether `n` and `n_infected`, and with them the proportion,
#'  count distinct `"patients"` (the default: a patient admitted twice in one
#'  window counts once) or `"enrollments"` (admissions).
#' @param conf.level Confidence level of the Wilson interval. Default 0.95.
#'
#' @returns A tibble with one row per row of `windows`: `department_key`,
#'  `window`, `start`, `end`, the four counts `n_patients`, `n_enrollments`,
#'  `n_infected_patients` and `n_infected_enrollments`, then `n` and
#'  `n_infected` for the chosen `unit`, `proportion` (per 100; `NA` when `n`
#'  is 0) and its `ci_lower` and `ci_upper`.
#' @export
get_cumulative_incidence_table <- function(
    x, windows, event_types = c("bsi", "hap"),
    unit = c("patients", "enrollments"), conf.level = 0.95)
{
  check_neoipcr_ds(x)
  assert_options_for(x, required = list(
    include_patient    = c("pseudo", "full"),
    include_enrollment = "full",
    include_event      = "full",
    include_department = c("pseudo", "full")
  ), fn_name = "get_cumulative_incidence_table")
  require_cols(x$patients, "patient_key", "patients")
  require_cols(
    x$enrollments,
    c("enrollment_key", "patient_key", "department_key", "enrolledAt"),
    "enrollments")
  require_cols(
    x$events,
    c("event_key", "enrollment_key", "event_type_key", "occurredAt"),
    "events")

  if (!is.data.frame(windows))
    rlang::abort(gettextf("%s must be a data frame.", "`windows`"))
  missing_cols <- setdiff(c("department_key", "window", "start", "end"), names(windows))
  if (length(missing_cols) > 0L)
    rlang::abort(c(
      if (length(missing_cols) == 1L)
        gettextf("%s lacks a required column:", "`windows`")
      else
        gettextf("%s lacks required columns:", "`windows`"),
      "x" = paste(missing_cols, collapse = ", ")))
  if (!inherits(windows$start, "Date") || !inherits(windows$end, "Date"))
    rlang::abort(gettextf(
      "%s and %s must be %s columns.", "`windows$start`", "`windows$end`", "`Date`"))
  if (any(is.na(windows$start) | is.na(windows$end) | windows$start > windows$end))
    rlang::abort(gettextf(
      "Every window needs %s, with neither date missing.", "`start <= end`"))
  if (any(is.na(windows$department_key)))
    rlang::abort(gettextf(
      "Every window must have a non-missing %s.", "`department_key`"))

  event_types <- rlang::arg_match(
    event_types, c("bsi", "nec", "hap", "ssi"), multiple = TRUE)
  unit <- rlang::arg_match(unit)
  check_number_decimal(
    conf.level, min = .Machine$double.eps, max = 1 - .Machine$double.eps)

  r <- x |>
    get_cumulative_incidence(windows, event_types)

  if (unit == "patients")
    r <- r |>
      dplyr::mutate(
        n = .data$n_patients,
        n_infected = .data$n_infected_patients)
  else
    r <- r |>
      dplyr::mutate(
        n = .data$n_enrollments,
        n_infected = .data$n_infected_enrollments)

  r |>
    dplyr::mutate(
      proportion = dplyr::if_else(
        .data$n > 0L, .data$n_infected / .data$n * 100, NA_real_)) |>
    dplyr::bind_cols(
      wilson_ci_cols(r$n_infected, r$n, scale = 100, conf.level = conf.level)) |>
    dplyr::select(
      "department_key", "window", "start", "end",
      "n_patients", "n_enrollments",
      "n_infected_patients", "n_infected_enrollments",
      "n", "n_infected", "proportion", "ci_lower", "ci_upper") |>
    add_class("neoipcr_tbl_cuminc")
}

#' Get the table of microbiological testing by infection type
#'
#' Each row, one for primary sepsis/BSI, pneumonia, and SSI, gives two
#' shares, in percent: how often the infection's site was tested
#' microbiologically, which the treating team decides and the laboratory
#' available to it allows, and how often a test recorded an infectious agent,
#' the yield of the testing. Neither tells a true from a false result.
#'
#' 1. **Tested:** the pneumonias whose microbiological test, and the SSIs
#'    whose culture at the depth their infection type records, is recorded
#'    positive or negative, over every infection of the type. A test recorded
#'    "Not tested" or left blank counts as not tested, and so does an SSI
#'    without an infection type.
#' 2. **Positive:** the tested infections whose test is recorded positive (for
#'    an SSI, at its own depth) with at least one own slot naming an
#'    infectious agent, over the tested infections.
#'
#' Primary sepsis/BSI has only the positive share, over every episode,
#' clinical sepsis included, sampled or not: its form's single item "No
#' positive blood culture" cannot tell a blood culture never taken from a
#' negative one. An episode counts where an own slot names an infectious agent
#' and the form is not recorded culture-negative. A slot names an infectious
#' agent where it holds one, "Not listed", "Unidentifiable", and a code the
#' infectious-agent list lacks included; a slot holding only a source, a
#' resistance category, or a name does not. A secondary-BSI slot counts in
#' neither share, and the follow-up for a secondary BSI moves neither:
#' [get_secondary_bsi_rate_table()] gives both. NEC has no row, since it has
#' no test of its own and its infectious agents are recorded only for a
#' secondary BSI; nor do all types together, whose share would depend on each
#' department's mix of infection types.
#'
#' A department enters a share's quartiles where at least 5 infections are in
#' its denominator of the share, at 0 where none of them is in its numerator.
#' A share shows its quartiles where at least 5 departments enter and their
#' median denominator exceeds the number of infections in which the pooled
#' share gives 1 infection; the bootstrap of their intervals takes the same
#' departments.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include the quartile columns of both shares in the
#'  output. Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table with a row for each of `"bsi"`, `"hap"`, and `"ssi"`
#'  (`inf`), in that order:
#'
#'  * the positive share: `n`, the positive infections; `tested`, its
#'    denominator, the tested infections, every episode for `"bsi"`;
#'    `pooled`, the share, `NA` without a denominator; `ci_lower` and
#'    `ci_upper`, its Wilson interval; and with `include_quartiles` the
#'    departments' quartiles (`q1`, `q2`, `q3`), their bootstrap intervals
#'    (`q1_ci_lower` to `q3_ci_upper`), and `quartile_departments`, the
#'    number of departments that enter them, given whether or not the
#'    quartiles are shown;
#'  * the tested share: `total`, every infection of the type, its
#'    denominator; `tested_pooled`, the share; `tested_ci_lower` and
#'    `tested_ci_upper`, its Wilson interval; and with `include_quartiles` the
#'    departments' quartiles (`tested_q1`, `tested_q2`, `tested_q3`), their
#'    bootstrap intervals (`tested_q1_ci_lower` to `tested_q3_ci_upper`), and
#'    `tested_quartile_departments`. The share and all its columns but
#'    `total` are `NA` for `"bsi"`, whose `total` counts every episode.
#' @export
get_infectious_agent_detection_rate_per_inf_type_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "infectious_agent_detection_rate_per_inf_type_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  types <- testing_infection_types
  counts <- get_infectious_agent_detection_rates_per_inf_type(
    x, use_cache = use_cache)
  # The measures of the share of the infections of `share_types` counted in
  # the column `events` among those counted in `denominator`.
  share <- function(share_types, events, denominator)
    .infectious_agent_rate_columns(
      counts[counts$event_type_key %in% share_types, ] |>
        dplyr::mutate(
          event_type_key = .data$event_type_key,
          department_key = .data$department_key,
          n = .data[[events]],
          total = .data[[denominator]],
          .keep = "none"),
      keys = tibble::tibble(event_type_key = share_types),
      by = "event_type_key",
      scale = 100,
      interval = "wilson",
      denominator = "count",
      include_quartiles = include_quartiles)

  # Primary sepsis/BSI has no tested share, its form not recording whether a
  # blood culture was taken.
  tested <- share(setdiff(types, "bsi"), "tested", "total") |>
    dplyr::select(!c("n", "total")) |>
    dplyr::rename_with(\(name) paste0("tested_", name), !"event_type_key")
  r <- share(types, "positive", "tested") |>
    dplyr::rename("tested" = "total") |>
    dplyr::left_join(
      dplyr::summarise(counts, total = sum(.data$total),
                       .by = "event_type_key"),
      dplyr::join_by("event_type_key")) |>
    dplyr::left_join(tested, dplyr::join_by("event_type_key")) |>
    dplyr::mutate(
      inf = factor(.data$event_type_key, levels = types),
      total = dplyr::coalesce(.data$total, 0L)) |>
    dplyr::select(tidyselect::any_of(c(
      "inf", "n", "tested", "pooled", "ci_lower", "ci_upper",
      department_quartile_cols, "total", "tested_pooled", "tested_ci_lower",
      "tested_ci_upper", paste0("tested_", department_quartile_cols)))) |>
    add_class("neoipcr_tbl_iadrpit")
  if (include_quartiles)
    r <- add_class(r, "neoipcr_tbl_iadrpit_ref")

  cache(r, x, cache_key)
}

#' Get the table of the infectious agents detected in nosocomial infections
#'
#' Counts the infectious agents that the slots of the infection forms record,
#' each once per infection, and only in a slot the form asks for: a sepsis
#' form's slots unless it is recorded culture-negative, a pneumonia's own slots
#' where its test is positive, an SSI's own slots where a depth is tested
#' positive, and the secondary-BSI slots where the follow-up is answered Yes.
#' A slot holding only a source, a resistance category, or a name records
#' none. An infection is microbiologically confirmed where such a slot records
#' an infectious agent; two codes of one concept, a synonym and the code it
#' stands for, count as one infectious agent.
#'
#' The first row, the Total, gives the infectious agents per 100 confirmed
#' infections, "Not listed", "Unidentifiable", and the codes the
#' infectious-agent list lacks included, with an exact Poisson interval;
#' `n_outside_tree` gives how many of them have no other row. Every other row
#' gives the share of the confirmed infections with an infectious agent in the
#' tree in which an infectious agent at or beneath the row's node was found,
#' with a Wilson interval. An infection counts once in each row, so a parent's
#' share need not be the sum of its children's, and the shares can add up to
#' more than 100%. A table has a row for each node at or above an infectious
#' agent found; each row's children follow it, siblings by their share,
#' descending.
#'
#' Over the reporting groups (`rows = "groups"`), the rows are the groups of
#' the reporting classification of the NeoIPC infectious-agent list, nested as
#' it nests them, below a summary block of the bacteria by Gram reaction and by
#' oxygen requirement, a row per value found, keyed `"<attribute>|<value>"`.
#' Over the full ontology (`rows = "ontology"`), the rows are every node of an
#' infectious agent's path through the list, from its top-level row, Bacteria,
#' Fungi, Protozoa, or Viruses, to the node at which it is recorded, a concept
#' beneath a species (a subspecies, a serotype, or a group such as the
#' polioviruses) counting in its species; where infectious agents are recorded
#' at a node that has nodes beneath it in the full ontology (a species has
#' none), a residual row labelled "n.o.s." (not otherwise specified), last
#' among the node's children, counts them, whether or not the table has a row
#' beneath the node, so that a department's rows are those a reference
#' holding it gives the department.
#'
#' Beside the column of all infection types, each row has the same measures
#' among the confirmed infections of each type, without quartiles. A
#' department enters a row's quartiles where at least 5 confirmed infections
#' are in its denominator of the row, at 0 where none of them is the row's; a
#' row shows its quartiles where at least 5 departments enter and their median
#' denominator exceeds the number of infections in which the pooled rate gives
#' 1 event; the bootstrap of their intervals takes the same departments.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#' @param rows The rows of the table: `"groups"`, the default, over the
#'  reporting groups, or `"ontology"`, over the full ontology. A calculated
#'  dataset carries both.
#'
#' @returns A table with one row per row described above: `level`, 0 for the
#'  Total, 1 for a summary row and a top-level node, and one more for each
#'  level below; `rank`, the node's concept type, `NA` for the Total, a summary
#'  row, a reporting group, and a residual row; `node_id`, the row's key, `NA`
#'  for the Total, which stays the same for a node across releases of the list
#'  that keep it; `parent_id`, the `node_id` of the row's parent, `NA` for a
#'  top-level row; `group`, the row's label; `n`, the infectious agents (the
#'  Total) or the infections (every other row) counted; `total`, the
#'  denominator; `pooled`, the rate per 100, `NA` without a denominator;
#'  `ci_lower` and `ci_upper`, its interval; with `include_quartiles`, the
#'  departments' quartiles `q1`, `q2`, and `q3`, their bootstrap intervals
#'  `q1_ci_lower` to `q3_ci_upper`, and `quartile_departments`, the number of
#'  departments that enter them; `n_outside_tree`, on the Total only; and, for
#'  each infection type (`bsi`, `hap`, `nec`, and `ssi`), `n_<type>`,
#'  `total_<type>`, `pooled_<type>`, `ci_lower_<type>`, and `ci_upper_<type>`.
#' @export
get_infectious_agent_detection_rate_per_agent_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE,
    rows = c("groups", "ontology")) {
  rows <- rlang::arg_match(rows)
  cache_key <- infectious_agent_detection_table_names[[rows]]
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  d <- get_infectious_agent_detection_rates(x, rows = rows, use_cache = use_cache)

  total <- .infectious_agent_rate_columns(
    dplyr::mutate(d$total, .row = "total"),
    keys = tibble::tibble(.row = "total"),
    by = ".row",
    scale = 100,
    interval = "poisson",
    denominator = "count",
    include_quartiles = include_quartiles,
    types = infectious_agent_infection_types)
  node_rows <- .infectious_agent_rate_columns(
    d$rows,
    keys = d$layout["node_id"],
    by = "node_id",
    scale = 100,
    interval = "wilson",
    denominator = "count",
    include_quartiles = include_quartiles,
    types = infectious_agent_infection_types)

  r <- dplyr::bind_rows(
    tibble::tibble(
      level = 0L,
      rank = NA_character_,
      node_id = NA_character_,
      parent_id = NA_character_,
      group = "Total",
      n_outside_tree = d$n_outside_tree) |>
      dplyr::bind_cols(dplyr::select(total, !".row")),
    d$layout |>
      dplyr::left_join(node_rows, dplyr::join_by("node_id"))) |>
    dplyr::relocate("n_outside_tree", .before = "n_bsi") |>
    add_class("neoipcr_tbl_iadrpa")
  if (include_quartiles)
    r <- add_class(r, "neoipcr_tbl_iadrpa_ref")

  cache(r, x, cache_key)
}

# The slots of a calculated dataset, and the cache keys, of the two versions of
# the table of the infectious agents detected.
infectious_agent_detection_table_names <- c(
  groups = "infectious_agent_detection_rate_per_agent_table",
  ontology = "infectious_agent_detection_rate_per_agent_ontology_table")

#' Get the table of the nosocomial infections with resistant infectious agents
#'
#' For each resistance category, third-generation cephalosporin resistance
#' (3GCR), carbapenem resistance, colistin resistance, methicillin-resistant
#' *Staphylococcus aureus* (MRSA), and vancomycin-resistant *Enterococcus*
#' (VRE), a block gives the density of the infections with a resistant
#' infectious agent per 1000 patient-days, with an exact Poisson interval,
#' every infant being at risk: first the category's Total, the infections
#' with at least one infectious agent recorded resistant in the category,
#' then the rows on the paths of the infectious agents to which the NeoIPC
#' infectious-agent list applies the category, each with the infections in
#' which an infectious agent at or beneath its node is recorded resistant in
#' it. A block has each such row that lies at or above an infectious agent
#' found to which the list applies the category or that has a result for
#' it, so that whether a row is there depends only on the infectious agents
#' at or beneath it. An infection counts once in each row, so a parent's
#' density need not be the sum of its children's. The patient-days are all
#' those of the dataset, as the incidence densities count them.
#'
#' The infectious agents are those that
#' [get_infectious_agent_detection_rate_per_agent_table()] counts, each once
#' per infection and only in a slot the form asks for, and one is resistant
#' where a slot naming it records it resistant. "Not listed",
#' "Unidentifiable", and a code the list lacks count in a Total only, and
#' `n_outside_tree` gives how many of them are recorded resistant. A value
#' recorded for a category that the list does not apply to its infectious
#' agent counts as any other, in the Total and in the block's rows at or
#' above the infectious agent's node, which it gives the block.
#'
#' Over the reporting groups (`rows = "groups"`), the rows of a block below
#' its Total are a summary block of the bacteria by Gram reaction and by
#' oxygen requirement, a row per value found that an infectious agent the
#' list applies the category to has, keyed `"<attribute>|<value>"`, and then
#' the groups of the reporting classification, nested as it nests them. Over
#' the full ontology (`rows = "ontology"`), they are the nodes of the paths
#' through the list, from the top-level row, Bacteria, Fungi, Protozoa, or
#' Viruses, to the node at which an infectious agent is recorded, a concept
#' beneath a species (a subspecies, a serotype, or a group such as the
#' polioviruses) counting in its species; where infectious agents are
#' recorded at a node that has a node of those paths beneath it in the full
#' ontology (a species has none), a residual row labelled "n.o.s." (not
#' otherwise specified), last among the node's children, counts them, whether
#' or not the block has a row beneath the node. Each row's children follow
#' it, siblings by their density, descending.
#'
#' A department enters a row's quartiles with at least 50 patient-days, at 0
#' where it has none of the row's infections, unless the list applies the
#' category to infectious agents it detected and none of them has a result
#' for it, as [get_resistance_test_rate_table()] counts them: its 0 would
#' dilute the quartiles. A row shows its quartiles where at least 5
#' departments enter and their median patient-days exceed the patient-days in
#' which the pooled density gives 1 infection; the bootstrap of their
#' intervals takes the same departments.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#' @param rows The rows of the table: `"groups"`, the default, over the
#'  reporting groups, or `"ontology"`, over the full ontology. A calculated
#'  dataset carries both.
#'
#' @returns A table with the blocks described above, in the order of `abr`,
#'  the resistance category, a factor with the levels `"3gcr"`, `"car"`,
#'  `"cor"`, `"mrsa"`, and `"vre"`; then `level`, 0 for a Total, 1 for a
#'  summary row and a top-level node, and one more for each level below;
#'  `rank`, the node's concept type, `NA` for a Total, a summary row, a
#'  reporting group, and a residual row; `node_id`, the row's key, `NA` for a
#'  Total; `parent_id`, the `node_id` of the row's parent, `NA` for a
#'  top-level row; `group`, the row's label; `n`, the infections counted;
#'  `total`, the patient-days; `pooled`, the density per 1000 patient-days,
#'  `NA` without patient-days; `ci_lower` and `ci_upper`, its interval; with
#'  `include_quartiles`, the departments' quartiles `q1`, `q2`, and `q3`,
#'  their bootstrap intervals `q1_ci_lower` to `q3_ci_upper`, and
#'  `quartile_departments`, the number of departments that enter them; and
#'  `n_outside_tree`, on a Total only.
#' @export
get_abr_infection_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE,
    rows = c("groups", "ontology")) {
  rows <- rlang::arg_match(rows)
  cache_key <- abr_infection_rate_table_names[[rows]]
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  d <- get_infectious_agent_resistance_rates(
    x, rows = rows, use_cache = use_cache)
  patient_days <- get_risk_time(
    x, group_cols = "department_key", use_cache = use_cache) |>
    dplyr::select("department_key", "patient_days")
  untested <- get_resistance_test_counts(x, use_cache = use_cache) |>
    dplyr::filter(.data$cond == "routine", .data$total > 0L, .data$n == 0L)

  # Each department's patient-days for each row of `keys`, with the row's
  # infections `n` there, 0 where it has none.
  over_patient_days <- function(counts, keys)
    keys |>
    dplyr::cross_join(patient_days) |>
    dplyr::full_join(counts, by = c(names(keys), "department_key")) |>
    dplyr::mutate(
      n = dplyr::coalesce(.data$n, 0L),
      total = dplyr::coalesce(.data$patient_days, 0L)) |>
    dplyr::select(!"patient_days")

  block <- function(category) {
    outside <- untested$department_key[untested$abr == category]
    totals <- d$total[d$total$abr == category, ]
    total_key <- tibble::tibble(.row = "total")
    total <- .infectious_agent_rate_columns(
      over_patient_days(
        tibble::tibble(
          .row = "total",
          department_key = totals$department_key,
          n = totals$resistant),
        total_key),
      keys = total_key,
      by = ".row",
      scale = 1000,
      interval = "poisson",
      denominator = "days",
      include_quartiles = include_quartiles,
      outside_quartiles = outside)

    counts <- d$rows[d$rows$abr == category, ]
    keys <- tibble::tibble(node_id = unique(counts$node_id))
    node_rows <- .infectious_agent_rate_columns(
      over_patient_days(
        tibble::tibble(
          node_id = counts$node_id,
          department_key = counts$department_key,
          n = counts$resistant),
        keys),
      keys = keys,
      by = "node_id",
      scale = 1000,
      interval = "poisson",
      denominator = "days",
      include_quartiles = include_quartiles,
      outside_quartiles = outside)
    layout <- .infectious_agent_layout(
      tibble::tibble(node_id = node_rows$node_id, rate = node_rows$pooled),
      rows)

    dplyr::bind_rows(
      tibble::tibble(
        abr = category,
        level = 0L,
        rank = NA_character_,
        node_id = NA_character_,
        parent_id = NA_character_,
        group = "Total") |>
        dplyr::bind_cols(dplyr::select(total, !".row")) |>
        dplyr::mutate(n_outside_tree = sum(totals$n_outside_tree)),
      layout |>
        dplyr::left_join(node_rows, dplyr::join_by("node_id")) |>
        dplyr::mutate(abr = category, .before = 1))
  }

  r <- purrr::map(infectious_agent_resistance_categories, block) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      abr = factor(.data$abr, levels = infectious_agent_resistance_categories)) |>
    add_class("neoipcr_tbl_abr_ir")
  if (include_quartiles)
    r <- add_class(r, "neoipcr_tbl_abr_ir_ref")

  cache(r, x, cache_key)
}

# The slots of a calculated dataset, and the cache keys, of the two versions of
# the table of the infections with resistant infectious agents.
abr_infection_rate_table_names <- c(
  groups = "abr_infection_rate_table",
  ontology = "abr_infection_rate_ontology_table")

#' Get the table of the resistance rates per infectious agent
#'
#' For each resistance category, third-generation cephalosporin resistance
#' (3GCR), carbapenem resistance, colistin resistance, methicillin-resistant
#' *Staphylococcus aureus* (MRSA), and vancomycin-resistant *Enterococcus*
#' (VRE), a block gives the rows on the paths of the infectious agents to
#' which the NeoIPC infectious-agent list applies the category, each that
#' lies at or above an infectious agent found to which the list applies the
#' category or that has a result for it, so that whether a row is there
#' depends only on the infectious agents at or beneath it. A row's rate is
#' the share, in percent with a Wilson interval, of the infections in which
#' an infectious agent at or beneath the row's node has a result for the
#' category, recorded resistant or not, in which one is recorded resistant.
#' Beside it, the Tested share gives, of the infections
#' with an infectious agent at or beneath the node to which the list applies
#' the category, those in which one has a result, so that an infectious agent
#' the category cannot apply to does not lower a parent row's share; "Not
#' tested" and a blank count as no result.
#'
#' The infectious agents are those that
#' [get_infectious_agent_detection_rate_per_agent_table()] counts, each once
#' per infection and only in a slot the form asks for: one that two slots of
#' an infection name has a result where either has one and is resistant where
#' either records it resistant. An infection counts once in each row,
#' resistant where an infectious agent of the row's node is, so every
#' infection counts, not only an infant's first isolate. A value recorded for
#' a category that the list does not apply to its infectious agent counts in
#' the rate as any other, in the block's rows at or above the infectious
#' agent's node, which it gives the block. "Not listed", "Unidentifiable", and
#' a code the list lacks have no row.
#'
#' Over the reporting groups (`rows = "groups"`), a block's rows are a summary
#' block of the bacteria by Gram reaction and by oxygen requirement, a row per
#' value found that an infectious agent the list applies the category to
#' has, keyed `"<attribute>|<value>"`, and then the groups of the reporting
#' classification, nested as it nests them. Over the full ontology
#' (`rows = "ontology"`), they are the nodes of the paths through the list,
#' from the top-level row, Bacteria, Fungi, Protozoa, or Viruses, to the node
#' at which an infectious agent is recorded, a concept beneath a species (a
#' subspecies, a serotype, or a group such as the polioviruses) counting in
#' its species; where infectious agents are recorded at a node that has a
#' node of those paths beneath it in the full ontology (a species has none),
#' a residual row labelled "n.o.s." (not otherwise specified), last among the
#' node's children, counts them, whether or not the block has a row beneath
#' the node. Each row's children follow it, siblings by their rate,
#' descending.
#'
#' A department enters a row's quartiles where at least 5 infections are in
#' its denominator of the row, at 0 where none of them is resistant; a row
#' shows its quartiles where at least 5 departments enter and their median
#' denominator exceeds the number of infections in which the pooled rate
#' gives 1 resistant one; the bootstrap of their intervals takes the same
#' departments. The Tested share has no quartiles.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#' @param rows The rows of the table: `"groups"`, the default, over the
#'  reporting groups, or `"ontology"`, over the full ontology. A calculated
#'  dataset carries both.
#'
#' @returns A table with the blocks described above, in the order of `abr`,
#'  the resistance category, a factor with the levels `"3gcr"`, `"car"`,
#'  `"cor"`, `"mrsa"`, and `"vre"`; then `level`, 1 for a summary row and a
#'  top-level node, and one more for each level below; `rank`, the node's
#'  concept type, `NA` for a summary row, a reporting group, and a residual
#'  row; `node_id`, the row's key; `parent_id`, the `node_id` of the row's
#'  parent, `NA` for a top-level row; `group`, the row's label; `n`, the
#'  infections with a resistant infectious agent; `total`, those with a
#'  result; `pooled`, the rate in percent, `NA` without a result; `ci_lower`
#'  and `ci_upper`, its interval; with `include_quartiles`, the departments'
#'  quartiles `q1`, `q2`, and `q3`, their bootstrap intervals `q1_ci_lower` to
#'  `q3_ci_upper`, and `quartile_departments`, the number of departments that
#'  enter them; and the Tested share: `tested_n`, the infections with a result
#'  for an infectious agent the category applies to, `tested_total`, those
#'  with such an infectious agent, `tested_pooled`, the share in percent, and
#'  `tested_ci_lower` and `tested_ci_upper`, its interval.
#' @export
get_infectious_agent_resistance_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE,
    rows = c("groups", "ontology")) {
  rows <- rlang::arg_match(rows)
  cache_key <- infectious_agent_resistance_rate_table_names[[rows]]
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  d <- get_infectious_agent_resistance_rates(
    x, rows = rows, use_cache = use_cache)

  block <- function(category) {
    counts <- d$rows[d$rows$abr == category, ]
    keys <- tibble::tibble(node_id = unique(counts$node_id))
    rate <- .infectious_agent_rate_columns(
      tibble::tibble(
        node_id = counts$node_id,
        department_key = counts$department_key,
        n = counts$resistant,
        total = counts$tested),
      keys = keys,
      by = "node_id",
      scale = 100,
      interval = "wilson",
      denominator = "count",
      include_quartiles = include_quartiles)
    tested <- .infectious_agent_rate_columns(
      tibble::tibble(
        node_id = counts$node_id,
        department_key = counts$department_key,
        n = counts$applicable_tested,
        total = counts$applicable),
      keys = keys,
      by = "node_id",
      scale = 100,
      interval = "wilson",
      denominator = "count") |>
      dplyr::rename_with(\(name) paste0("tested_", name), !"node_id")
    layout <- .infectious_agent_layout(
      tibble::tibble(node_id = rate$node_id, rate = rate$pooled), rows)

    layout |>
      dplyr::left_join(rate, dplyr::join_by("node_id")) |>
      dplyr::left_join(tested, dplyr::join_by("node_id")) |>
      dplyr::mutate(abr = category, .before = 1)
  }

  r <- purrr::map(infectious_agent_resistance_categories, block) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      abr = factor(.data$abr, levels = infectious_agent_resistance_categories)) |>
    add_class("neoipcr_tbl_iarr")
  if (include_quartiles)
    r <- add_class(r, "neoipcr_tbl_iarr_ref")

  cache(r, x, cache_key)
}

# The slots of a calculated dataset, and the cache keys, of the two versions of
# the table of the resistance rates per infectious agent.
infectious_agent_resistance_rate_table_names <- c(
  groups = "infectious_agent_resistance_rate_table",
  ontology = "infectious_agent_resistance_rate_ontology_table")

#' Get the table of the completeness of antimicrobial susceptibility testing
#'
#' For each resistance category, third-generation cephalosporin resistance
#' (`"3gcr"`), carbapenem resistance (`"car"`), colistin resistance
#' (`"cor"`), methicillin-resistant *Staphylococcus aureus* (`"mrsa"`), and
#' vancomycin-resistant *Enterococcus* (`"vre"`), a row gives the share, in
#' percent with a Wilson interval, of the detections of an infectious agent to
#' which the NeoIPC infectious-agent list applies the category that have a
#' result for it, recorded resistant or not. A detection is an infectious
#' agent in an infection, counted once per infection and only in a slot the
#' form asks for, with a result where either slot naming it has one; "Not
#' tested" and a blank count as undocumented, in the denominator and not the
#' numerator. "Not listed", to which the list applies every category, counts
#' in every row; "Unidentifiable" and a code the list lacks, to which it
#' applies none, in no row. A value recorded for a category that the list
#' does not apply to its infectious agent counts in no row, since its
#' detection is in no denominator of the category.
#'
#' Two conditional rows hold only the detections recorded resistant to 3GCR:
#' carbapenem resistance in case of 3GCR (`cond = "if_3gcr"`), and colistin
#' resistance in case of 3GCR and carbapenem resistance
#' (`cond = "if_3gcr&car"`), whose detections are also recorded resistant to
#' carbapenems. An infectious agent to which the list does not apply 3GCR,
#' such as *Acinetobacter*, counts in the plain rows (`cond = "routine"`)
#' only.
#'
#' A department enters a row's quartiles where at least 5 detections are in
#' its denominator of the row, at 0 where none of them has a result; a row
#' shows its quartiles where at least 5 departments enter and their median
#' denominator exceeds the number of detections in which the pooled share
#' gives 1 with a result; the bootstrap of their intervals takes the same
#' departments.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table with seven rows: `abr`, the resistance category, a factor
#'  with the levels above; `cond`, the row, a factor with the levels
#'  `"routine"`, `"if_3gcr"`, and `"if_3gcr&car"`; `n`, the detections with a
#'  result; `total`, the detections the row holds; `pooled`, their share in
#'  percent, `NA` without a detection; `ci_lower` and `ci_upper`, its
#'  interval; and with `include_quartiles` the departments' quartiles `q1`,
#'  `q2`, and `q3`, their bootstrap intervals `q1_ci_lower` to `q3_ci_upper`,
#'  and `quartile_departments`, the number of departments that enter them.
#' @export
get_resistance_test_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "resistance_test_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  r <- .infectious_agent_rate_columns(
    get_resistance_test_counts(x, use_cache = use_cache),
    keys = resistance_test_rows,
    by = c("abr", "cond"),
    scale = 100,
    interval = "wilson",
    denominator = "count",
    include_quartiles = include_quartiles) |>
    dplyr::mutate(
      abr = factor(.data$abr, levels = infectious_agent_resistance_categories),
      cond = factor(.data$cond,
                    levels = c("routine", "if_3gcr", "if_3gcr&car"))) |>
    add_class("neoipcr_tbl_rtr")
  if (include_quartiles)
    r <- add_class(r, "neoipcr_tbl_rtr_ref")

  cache(r, x, cache_key)
}

#' Get the table secondary BSI rates of the recorded infections
#'
#' Each row is the share of the NECs, pneumonias, or SSIs followed up for a
#' secondary BSI (their secondary-BSI item answered Yes or No) in which one
#' was found: a follow-up answered Yes with a secondary-BSI slot naming an
#' infectious agent. A slot holding only a source, a resistance category, or a
#' name names none, and an infectious agent left in the slots under another
#' answer, which only a validation exception or an import without the
#' validation pass lets through, does not count, so the rate is a share of its
#' denominator. Beside the rate, each row shows the share of the type's
#' infections that were followed up.
#'
#' @param x The data set which can be either a neoipcr_ds or a neoipcr_rep_ds
#'  object. In case of a neoipcr_rep_ds it has to be a neoipcr_ref_ds if
#'  include_quartiles is TRUE.
#' @param use_cache Use the cache. Ignored if x is a neoipcr_rep_ds object
#' @param include_quartiles Include quartile columns (q1, q2, q3) in the output.
#'  Set to FALSE for department-level reports to simplify output.
#'
#' @returns A table containing secondary BSI rates, a row per infection type
#'  (`event_type_key`): `n`, the infections with a secondary BSI; `pooled`,
#'  their share of the infections followed up, in percent; `ci_lower` and
#'  `ci_upper`, its Wilson interval; with `include_quartiles` the
#'  departments' quartiles of the rate (`q1`, `q2`, `q3`) and their bootstrap
#'  intervals; `followup_n`, the infections followed up; `total_n`, all
#'  infections of the type; `followup_pooled`, the share followed up, in
#'  percent; and with `include_quartiles` the quartiles of the departments'
#'  follow-up shares (`followup_q1`, `followup_q2`, `followup_q3`) over the
#'  departments with at least 5 infections of the type, `NA` with fewer than
#'  5 such departments.
#' @export
get_secondary_bsi_rate_table <- function(
    x, use_cache = TRUE, include_quartiles = TRUE) {
  cache_key <- "secondary_bsi_rate_table"
  if(!is.null(r <- x |> check_ds_and_try_get_table(
    cache_key, use_cache, include_quartiles, include_event = "full")))
    return(r)

  # Calculate pooled rates
  rates <- x |>
    get_secondary_bsi_rates(use_cache = use_cache)
  r <- rates |>
    dplyr::rename("pooled" = "rate")
  r <- r |>
    dplyr::bind_cols(wilson_ci_cols(r$n, r$followup_n, scale = 100)) |>
    dplyr::select("event_type_key", "n", "pooled", "ci_lower", "ci_upper") |>
    add_class("neoipcr_tbl_sec_bsi")

  # Calculate quartiles if needed
  if(include_quartiles) {
    dept_all <- x |>
      get_secondary_bsi_rates(
        group_cols = "department_key",
        use_cache = use_cache)
    # The rate's quartiles take the departments that followed up infections
    # of the type; the follow-up shares' take those with at least 5
    # infections of the type, below.
    dept_rates <- dept_all |>
      dplyr::filter(.data$followup_n > 0L)

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

  # The share of each type's infections followed up for a secondary BSI,
  # with its counts, and in the reference table the quartiles of the
  # departments' shares.
  r <- r |>
    dplyr::left_join(
      rates |>
        dplyr::select(
          "event_type_key", "followup_n", "total_n",
          "followup_pooled" = "followup_rate"),
      dplyr::join_by("event_type_key"))
  if(include_quartiles)
    r <- r |>
    dplyr::left_join(
      followup_share_quartiles(dept_all, r$event_type_key),
      dplyr::join_by("event_type_key"))

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

# The quartiles of the departments' shares of infections followed up for a
# secondary BSI (`followup_q1`, `followup_q2`, `followup_q3`) for each of
# `event_type_keys`, from `dept_rates`, the secondary-BSI rates by
# `department_key`. A department enters a type's quartiles with at least 5
# infections of the type, and the quartiles need at least 5 such departments;
# otherwise they are `NA`.
followup_share_quartiles <- function(dept_rates, event_type_keys)
  purrr::map(as.character(event_type_keys), \(type) {
    shares <- dept_rates$followup_rate[
      dept_rates$event_type_key == type & dept_rates$total_n >= 5L]
    q <- if (length(shares) < 5L) rep(NA_real_, 3L) else
      stats::quantile(shares, probs = quartile_probs, names = FALSE)
    tibble::tibble(
      event_type_key = type,
      followup_q1 = q[1],
      followup_q2 = q[2],
      followup_q3 = q[3])
  }) |>
  purrr::list_rbind(
    ptype = tibble::tibble(
      event_type_key = character(),
      followup_q1 = numeric(),
      followup_q2 = numeric(),
      followup_q3 = numeric()))
