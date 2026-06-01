quartile_probs <- c(0.25,0.5,0.75)

ensure_quartile_cols <- function(df) {
  for (col in c("q1", "q2", "q3")) {
    if (!col %in% names(df))
      df[[col]] <- NA_real_
  }
  df
}

# Recompute Poisson CIs for zero-event rows created by full_join or
# upstream missing-level backfill.  A rate of 0 with a known denominator
# has a valid CI (lower = 0, upper > 0).
fix_zero_event_ci <- function(tbl, suffixes, denominator_tbl,
                              denom_prefix = "total",
                              n_prefix = "n",
                              multiplier = 100) {
  for (sfx in suffixes[suffixes != ""]) {
    n_col  <- paste0(n_prefix, sfx)
    ci_lo  <- paste0("ci_lower", sfx)
    ci_hi  <- paste0("ci_upper", sfx)
    d_col  <- paste0(denom_prefix, sfx)
    if (all(c(n_col, ci_lo) %in% names(tbl)) &&
        d_col %in% names(denominator_tbl)) {
      zero <- !is.na(tbl[[n_col]]) & tbl[[n_col]] == 0 & is.na(tbl[[ci_lo]])
      if (any(zero) && denominator_tbl[[d_col]] > 0) {
        ci <- neoipc_poisson_ci(0, denominator_tbl[[d_col]],
                                 multiplier = multiplier)
        tbl[[ci_lo]][zero] <- ci$lower
        tbl[[ci_hi]][zero] <- ci$upper
      }
    }
  }
  tbl
}

#' Calculate a NeoIPC reference data set
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#' @param redact Redact potentially sensitive information
#'
#' @returns A NeoIPC reference data set
#' @export
calculate_reference_data <- function(x, use_cache = TRUE, redact = TRUE) {
  check_neoipcr_ds(x)

  if(is.null(x$enrollments$department_key))
    rlang::abort("Cannot calculate reference data without department information. You need to include at least pseudonymised department information.")

  if(is.null(x$metadata$countries))
    rlang::warn("The data is missing country metadata. The resulting dataset connot be used to create reference reports")

  pd <- get_risk_time(x, use_cache = use_cache)$patient_days
  pd_dept <- get_risk_time(
    x,
    group_cols = "department_key",
    use_cache = use_cache)$patient_days
  pd_q <- pd_dept |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  rp <- x |>
    get_risk_population(use_cache = use_cache)
  rp_dept <- x |>
    get_risk_population(group_cols = "department_key", use_cache = use_cache)
  pat_q <- rp_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  enr_q <- rp_dept$n_enrollments |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  sr_dept <- x |>
    get_surgery_risk(
      group_cols = "department_key",
      use_cache = use_cache) |>
    dplyr::full_join(
      x$enrollments |>
        dplyr::select("department_key") |>
        dplyr::distinct(),
      dplyr::join_by("department_key")) |>
    dplyr::mutate(
      n_patients = tidyr::replace_na(.data$n_patients, 0L),
      n_procedures = tidyr::replace_na(.data$n_procedures, 0L))
  sur_pat_q <- sr_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  sur_proc_q <- sr_dept$n_procedures |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  ds_opts <- x$metadata$dataset_options
  if(redact && typeof(ds_opts$include_invalid_patients) != "logical")
    ds_opts$include_invalid_patients <- "redacted"

  n_infections <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(event_type_key = "overall"),
      x |>
        get_infection_counts() |>
        dplyr::bind_cols(
          x |>
            get_infection_counts(group_cols = "department_key") |>
            dplyr::summarise(
              pooled_mean = as.integer(round(mean(.data$n))),
              q = list(
                stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
            tidyr::unnest_wider(q, names_sep = "", transform = as.integer) |>
            ensure_quartile_cols())),
    x |>
      get_infection_counts(group_cols = c("event_type_key")) |>
      dplyr::inner_join(
        x |>
          get_infection_counts(
            group_cols = c("department_key", "event_type_key")) |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::summarise(
            pooled_mean = as.integer(round(mean(.data$n))),
            q = list(
              stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
          tidyr::unnest_wider(q, names_sep = "", transform = as.integer) |>
          ensure_quartile_cols(),
        dplyr::join_by("event_type_key"))) |>
    dplyr::rename(inf_type = "event_type_key", total = "n")

  structure(
    list(
      metadata = list(
        calculated = lubridate::now("UTC"),
        dataset_options = ds_opts,
        data_up_to = x$metadata$system$date,
        effective_analysis_period = get_effective_analysis_period(x),
        countries = get_countries_with_wb_class(x)
      ),
      birth_weight_figure = x|> get_birthweight_figure_data(),
      gestational_age_figure = x|> get_gestational_age_figure_data(),
      n_departments = x$enrollments$department_key |> unique() |> length(),
      n_patients = tibble::tibble(
        total = rp$n_patients,
        pooled_mean = rp_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pat_q[1],
        q2 = pat_q[2],
        q3 = pat_q[3]),
      n_enrollments = tibble::tibble(
        total = rp$n_enrollments,
        pooled_mean = rp_dept$n_enrollments |>
          mean() |>
          round() |>
          as.integer(),
        q1 = enr_q[1],
        q2 = enr_q[2],
        q3 = enr_q[3]),
      n_patient_days = tibble::tibble(
        total = pd,
        pooled_mean = pd_dept |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pd_q[1],
        q2 = pd_q[2],
        q3 = pd_q[3]),
      n_surgical_departments = sr$n_departments,
      n_surgical_patients = tibble::tibble(
        total = sr$n_patients,
        pooled_mean = sr_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_pat_q[1],
        q2 = sur_pat_q[2],
        q3 = sur_pat_q[3]),
      n_surgical_procedures = tibble::tibble(
        total = sr$n_procedures,
        pooled_mean = sr_dept$n_procedures |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_proc_q[1],
        q2 = sur_proc_q[2],
        q3 = sur_proc_q[3]),
      n_infections = n_infections,
      usage_density_rate_table =
        get_usage_density_rate_table(x, use_cache),
      antibiotic_utilisation_table =
        get_antibiotic_utilisation_table(x, use_cache),
      surgery_rate_table =
        get_ref_surgery_rate_table(x, use_cache),
      incidence_density_rate_table =
        get_incidence_density_rate_table(x, use_cache),
      dev_ass_incidence_density_rate_table =
        get_dev_ass_incidence_density_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_agent_table =
        get_infectious_agent_detection_rate_per_agent_table(x, use_cache),
      abr_infection_rate_table =
        get_abr_infection_rate_table(x, use_cache),
      organism_resistance_rate_table =
        get_organism_resistance_rate_table(x, use_cache),
      secondary_bsi_rate_table =
        get_secondary_bsi_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_inf_type_table =
        get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache),
      resistance_test_rate_table =
        get_resistance_test_rate_table(x, use_cache)
    ),
    class = c("neoipcr_ref_ds", "neoipcr_rep_ds", "list"))
}

#' Calculate a NeoIPC department report data set
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#'
#' @returns A NeoIPC department report data set
#' @export
calculate_department_data <- function(x, use_cache = TRUE) {
  check_neoipcr_ds(x)

  rt <- x |>
    get_risk_time(use_cache = use_cache)
  rp <- x |>
    get_risk_population(use_cache = use_cache)
  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  usage_density_rate_table <- rt |>
    dplyr::select(!"patient_days") |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("factor","name"),
      names_pattern = "^(.+)_([^_]+)$") |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      factor = factor(.data$factor,
        levels = c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                   "kangaroo_care","ab","a","w","r"))
    ) |>
    (\(d) dplyr::bind_cols(d, poisson_ci_cols(d$days, rt$patient_days, multiplier = 100)))() |>
    (\(r) {
      expected_levels <- c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                           "kangaroo_care","ab","a","w","r")
      missing <- setdiff(expected_levels, as.character(r$factor))
      if (length(missing) > 0)
        r <- r |>
          dplyr::bind_rows(
            tibble::tibble(
              factor = factor(missing, levels = levels(r$factor)),
              days = 0L,
              rate = NA_real_,
              ci_lower = NA_real_,
              ci_upper = NA_real_))
      r
    })() |>
    dplyr::arrange(.data$factor)

  # Extract infection counts for metadata
  n_infections <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(inf_type = "overall"),
      x |>
        get_infection_counts(use_cache = use_cache) |>
        dplyr::rename(total = "n")),
    x |>
      get_infection_counts(group_cols = c("event_type_key"), use_cache = use_cache) |>
      dplyr::rename(inf_type = "event_type_key", total = "n"))

  structure(
    list(
      metadata = list(
        calculated = lubridate::now("UTC"),
        dataset_options = x$metadata$dataset_options,
        data_up_to = x$metadata$system$date,
        effective_analysis_period = get_effective_analysis_period(x),
        hospitals = x$metadata$hospitals,
        departments = x$metadata$departments,
        countries = get_countries_with_wb_class(x)
      ),
      birth_weight_figure = get_birthweight_figure_data(x),
      gestational_age_figure = get_gestational_age_figure_data(x),
      n_departments = if (!is.null(x$enrollments$department_key)) {
        x$enrollments$department_key |> unique() |> length()
      } else {
        1L
      },
      n_patients = list(total = rp$n_patients),
      n_enrollments = list(total = rp$n_enrollments),
      n_patient_days = list(total = rt$patient_days),
      n_infections = n_infections,
      n_surgical_departments = sr$n_departments,
      n_surgical_procedures = list(total = sr$n_procedures),
      n_surgical_patients = list(total = sr$n_patients),
      usage_density_rate_table = usage_density_rate_table,
      antibiotic_utilisation_table =
        get_antibiotic_utilisation_table(x, use_cache, include_quartiles = FALSE),
      surgery_rate_table =
        get_surgery_rate_table(
          x,
          use_cache),
      incidence_density_rate_table =
        get_incidence_density_rate_table(
          x,
          use_cache,
          include_quartiles = FALSE),
      dev_ass_incidence_density_rate_table =
        get_dev_ass_incidence_density_rate_table(x, use_cache, include_quartiles = FALSE),
      infectious_agent_detection_rate_per_agent_table =
        get_infectious_agent_detection_rate_per_agent_table(x, use_cache, include_quartiles = FALSE),
      abr_infection_rate_table =
        get_abr_infection_rate_table(x, use_cache, include_quartiles = FALSE),
      organism_resistance_rate_table =
        get_organism_resistance_rate_table(x, use_cache, include_quartiles = FALSE),
      secondary_bsi_rate_table =
        get_secondary_bsi_rate_table(x, use_cache, include_quartiles = FALSE),
      infectious_agent_detection_rate_per_inf_type_table =
        get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache, include_quartiles = FALSE),
      resistance_test_rate_table =
        get_resistance_test_rate_table(x, use_cache, include_quartiles = FALSE)
    ),
    class = c("neoipcr_rep_ds", "list"))
}

get_countries_with_wb_class <- function(x) {
  if (!is.null(x$metadata$countries)) {
    countries_data <- x$enrollments |>
      dplyr::inner_join(
        x$metadata$countries |>
          dplyr::select("country_key", "displayName"),
        dplyr::join_by("country_key")) |>
        dplyr::select(name = "displayName", tidyselect::any_of("world_bank_class_key")) |>
        dplyr::distinct()

    # Join with worldBankClasses to get stable class and displayName
    if (!is.null(x$metadata$worldBankClasses) && "world_bank_class_key" %in% names(countries_data)) {
      countries_data |>
        dplyr::left_join(
          x$metadata$worldBankClasses |>
            dplyr::select("world_bank_class_key", wb_class = "class"),
          dplyr::join_by("world_bank_class_key")) |>
        dplyr::select("name", "wb_class") |>
        dplyr::distinct() |>
        dplyr::arrange(.data$name)
    } else {
      countries_data |>
      dplyr::select("name") |>
      dplyr::arrange(.data$name)
    }
  } else {
  NULL
  }
}

# Calculates effective analysis period from actual surveillance end dates
get_effective_analysis_period <- function(x) {
  surveillance_end_dates <- x$events |>
    dplyr::filter(.data$event_type_key == "end") |>
    dplyr::pull(.data$occurredAt)

  if (length(surveillance_end_dates) > 0) {
    return(
      list(
        from = min(surveillance_end_dates, na.rm = TRUE),
        to = max(surveillance_end_dates, na.rm = TRUE)))
  }

  return(NULL)
}


#' Creates a NeoIPC benchmark data set from department report datasets and a
#'  reference data set
#'
#' @param ... A set of name-value pairs. Each of them should be either a
#'  neoipcr_rep_ds or a neoipcr_ref_ds (typically it's exactly one
#'  neoipcr_rep_ds and one neoipcr_ref_ds to benchmark department data against
#'  reference data but it can also be multiple neoipcr_rep_ds or multiple
#'  neoipcr_ref_ds to benchmark them against each other). The names are used as
#'  prefixes in the resulting tables
#'
#' @returns A neoipcr_bnch_ds
#' @export
get_benchmark_data <- function(...) {
  x <- list(...)
  n_ds <- length(x)
  ds_names = rlang::names2(x)
  output <- list(
    dataset_names = ds_names,
    metadata = list())
  suffixes = ds_names |>
    sapply(\(x)ifelse(x=="",x,paste0("_",x)), USE.NAMES = FALSE)

  for (i in 1:n_ds) {
    ds <- x[[i]]
    suffix <- suffixes[i]
    ds_name <- ds_names[i]
    elements <- names(ds)

    # Extract metadata if present
    if ("metadata" %in% elements) {
      output$metadata[[ds_name]] <- ds$metadata
    }

    if ("n_departments" %in% elements) {
      tbl <- tibble::tibble(n = ds$n_departments) |>
        dplyr::rename_with(~ paste0(.x, suffix))
      output$n_departments <- dplyr::bind_cols(output$n_departments, tbl)
    }
    if ("n_surgical_departments" %in% elements) {
      tbl <- tibble::tibble(n = ds$n_surgical_departments) |>
        dplyr::rename_with(~ paste0(.x, suffix))
      output$n_surgical_departments <- dplyr::bind_cols(output$n_surgical_departments, tbl)
    }
    if ("n_patients" %in% elements) {
      tbl <- ds$n_patients
      if (!is.data.frame(tbl)) {
        tbl <- tibble::as_tibble(tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_patients <- dplyr::bind_cols(output$n_patients, tbl)
    }
    if ("n_enrollments" %in% elements) {
      tbl <- ds$n_enrollments
      if (!is.data.frame(tbl)) {
        tbl <- tibble::as_tibble(tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_enrollments <- dplyr::bind_cols(output$n_enrollments, tbl)
    }
    if ("n_patient_days" %in% elements) {
      tbl <- ds$n_patient_days
      if (!is.data.frame(tbl)) {
        tbl <- tibble::as_tibble(tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_patient_days <- dplyr::bind_cols(output$n_patient_days, tbl)
    }
    if ("n_surgical_patients" %in% elements) {
      tbl <- ds$n_surgical_patients
      if (!is.data.frame(tbl)) {
        tbl <- tibble::as_tibble(tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_surgical_patients <- dplyr::bind_cols(
        output$n_surgical_patients, tbl)
    }
    if ("n_surgical_procedures" %in% elements) {
      tbl <- ds$n_surgical_procedures
      if (!is.data.frame(tbl)) {
        tbl <- tibble::as_tibble(tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_surgical_procedures <- dplyr::bind_cols(
        output$n_surgical_procedures, tbl)
    }
    if ("birth_weight_figure" %in% elements) {
      n_tbl <- length(ds$birth_weight_figure)
      tbl_names <- names(ds$birth_weight_figure)
      for (j in 1:n_tbl) {
        tbl_name <- tbl_names[j]
        tbl <- tibble::tibble(dataset = ds_name) |>
          dplyr::bind_cols(ds$birth_weight_figure[[j]])

        if (is.null(output$birth_weight_figure)) {
          output$birth_weight_figure <- list()
        }
        if (is.null(output$birth_weight_figure[[tbl_name]])) {
          output$birth_weight_figure[[tbl_name]] <- tbl
        } else {
          output$birth_weight_figure[[tbl_name]] <-
            output$birth_weight_figure[[tbl_name]] |>
            dplyr::bind_rows(tbl)
        }
      }
    }
    if ("gestational_age_figure" %in% elements) {
      n_tbl <- length(ds$gestational_age_figure)
      tbl_names <- names(ds$gestational_age_figure)
      for (j in 1:n_tbl) {
        tbl_name <- tbl_names[j]
        tbl <- tibble::tibble(dataset = ds_name) |>
          dplyr::bind_cols(ds$gestational_age_figure[[j]])

        if (is.null(output$gestational_age_figure)) {
          output$gestational_age_figure <- list()
        }
        if (is.null(output$gestational_age_figure[[tbl_name]])) {
          output$gestational_age_figure[[tbl_name]] <- tbl
        } else {
          output$gestational_age_figure[[tbl_name]] <-
            output$gestational_age_figure[[tbl_name]] |>
            dplyr::bind_rows(tbl)
        }
      }
    }
    if ("usage_density_rate_table" %in% elements) {
      tbl <- ds$usage_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"factor")

      if (is.null(output$usage_density_rate_table)) {
        output$usage_density_rate_table <- tbl
      } else {
        output$usage_density_rate_table <- output$usage_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("factor")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("days_"),
              ~tidyr::replace_na(.x, 0L)),
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0L)),
            dplyr::across(
              c(dplyr::starts_with("rate_"), dplyr::starts_with("pooled_"),
                dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )

        output$usage_density_rate_table <- fix_zero_event_ci(
          output$usage_density_rate_table, suffixes,
          denominator_tbl = output$n_patient_days,
          multiplier = 100)
      }
    }
    if ("antibiotic_utilisation_table" %in% elements) {
      structural <- c("row_id", "atc5_group", "row_type", "display_name", "aware")
      tbl <- ds$antibiotic_utilisation_table |>
        dplyr::rename_with(~ paste0(.x, suffix),
                           !tidyselect::any_of(structural))

      if (is.null(output$antibiotic_utilisation_table)) {
        output$antibiotic_utilisation_table <- tbl
      } else {
        output$antibiotic_utilisation_table <-
          output$antibiotic_utilisation_table |>
          dplyr::full_join(tbl, dplyr::join_by("row_id"),
                           suffix = c("", ".y")) |>
          dplyr::mutate(
            atc5_group = dplyr::coalesce(
              .data$atc5_group, .data$atc5_group.y),
            row_type = dplyr::coalesce(
              .data$row_type, .data$row_type.y),
            display_name = dplyr::coalesce(
              .data$display_name, .data$display_name.y),
            aware = dplyr::coalesce(
              .data$aware, .data$aware.y)) |>
          dplyr::select(!tidyselect::ends_with(".y")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~ tidyr::replace_na(.x, 0L)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"),
                dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"),
                dplyr::starts_with("ci_upper_")),
              ~ tidyr::replace_na(.x, NA_real_)))

        output$antibiotic_utilisation_table <- fix_zero_event_ci(
          output$antibiotic_utilisation_table, suffixes,
          denominator_tbl = output$n_patient_days,
          multiplier = 100)

        # Re-sort after merge: ATC5 group, then header before substances
        output$antibiotic_utilisation_table <-
          output$antibiotic_utilisation_table |>
          dplyr::arrange(.data$atc5_group, .data$row_type, .data$row_id)
      }
    }
    if ("surgery_rate_table" %in% elements) {
      tbl <- ds$surgery_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"pro_cat")

      if (is.null(output$surgery_rate_table)) {
        output$surgery_rate_table <- tbl
      } else {
        output$surgery_rate_table <- output$surgery_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("pro_cat")) |>
          dplyr::mutate(
            dplyr::across(
              !tidyselect::matches("^q|^ci_", ignore.case = F),
              ~ tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_)))

        output$surgery_rate_table <- fix_zero_event_ci(
          output$surgery_rate_table, suffixes,
          denominator_tbl = output$n_patients,
          multiplier = 100)
      }
    }
    if ("incidence_density_rate_table" %in% elements) {
      tbl <- ds$incidence_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"inf")

      if (is.null(output$incidence_density_rate_table)) {
        output$incidence_density_rate_table <- tbl
      } else {
        output$incidence_density_rate_table <-
          output$incidence_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("inf")) |>
          dplyr::mutate(
            dplyr::across(dplyr::starts_with("n_"), ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )

        output$incidence_density_rate_table <- fix_zero_event_ci(
          output$incidence_density_rate_table, suffixes,
          denominator_tbl = output$n_patient_days,
          multiplier = 1000)
      }
    }
    if ("dev_ass_incidence_density_rate_table" %in% elements) {
      tbl <- ds$dev_ass_incidence_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"dev")

      if (is.null(output$dev_ass_incidence_density_rate_table)) {
        output$dev_ass_incidence_density_rate_table <- tbl
      } else {
        output$dev_ass_incidence_density_rate_table <-
          output$dev_ass_incidence_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("dev")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )
      }
    }
    if ("n_infections" %in% elements) {
      tbl <- ds$n_infections
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"inf_type")

      if (is.null(output$n_infections)) {
        output$n_infections <- tbl
      } else {
        output$n_infections <- output$n_infections |>
          dplyr::full_join(tbl, dplyr::join_by("inf_type"))
      }
    }
    if ("infectious_agent_detection_rate_per_agent_table" %in% elements) {
      tbl <- ds$infectious_agent_detection_rate_per_agent_table

      # Skip if table is NULL or empty
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Detect key columns - could be lv/tl (from dept) or
        # level/taxon (from ref)
        key_cols <- intersect(
          names(tbl),
          c("lv", "tl", "level", "taxon", "group", ".ontology_path"))
        tbl <- tbl |>
          dplyr::rename_with(
            ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

        if (is.null(output$infectious_agent_detection_rate_per_agent_table)) {
          output$infectious_agent_detection_rate_per_agent_table <- tbl
        } else {
          join_cols <- intersect(
            names(output$infectious_agent_detection_rate_per_agent_table),
            c("level", "taxon", "lv", "tl", "group", ".ontology_path"))
          output$infectious_agent_detection_rate_per_agent_table <-
            output$infectious_agent_detection_rate_per_agent_table |>
            dplyr::full_join(tbl, dplyr::join_by(!!!join_cols)) |>
            dplyr::arrange(.data$.ontology_path) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("n_"),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("rate_"),
                  dplyr::starts_with("pooled_"),
                  dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
    if ("abr_infection_rate_table" %in% elements) {
      tbl <- ds$abr_infection_rate_table

      # Skip if table is NULL or empty
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Detect key columns - could be abr_type/lv/tl (from dept) or
        # abr/level/taxon (from ref)
        key_cols <- intersect(
          names(tbl),
          c("abr_type", "abr", "lv", "tl", "level", "taxon", "group",
            ".ontology_path"))
        tbl <- tbl |>
          dplyr::rename_with(
            ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

        if (is.null(output$abr_infection_rate_table)) {
          output$abr_infection_rate_table <- tbl
        } else {
          join_cols <- intersect(
            names(output$abr_infection_rate_table),
            c("abr", "abr_type", "level", "taxon", "lv", "tl", "group",
              ".ontology_path"))
          output$abr_infection_rate_table <- output$abr_infection_rate_table |>
            dplyr::full_join(tbl, dplyr::join_by(!!!join_cols)) |>
            dplyr::arrange(.data$.ontology_path) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("n_"),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("rate_"),
                  dplyr::starts_with("pooled_"),
                  dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
    if ("organism_resistance_rate_table" %in% elements) {
      tbl <- ds$organism_resistance_rate_table

      if (!is.null(tbl) && nrow(tbl) > 0) {
        key_cols <- intersect(
          names(tbl),
          c("abr_type", "abr", "lv", "tl", "level", "taxon", "group",
            ".ontology_path"))
        tbl <- tbl |>
          dplyr::rename_with(
            ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

        if (is.null(output$organism_resistance_rate_table)) {
          output$organism_resistance_rate_table <- tbl
        } else {
          join_cols <- intersect(
            names(output$organism_resistance_rate_table),
            c("abr", "abr_type", "level", "taxon", "lv", "tl", "group",
              ".ontology_path"))
          output$organism_resistance_rate_table <- output$organism_resistance_rate_table |>
            dplyr::full_join(tbl, dplyr::join_by(!!!join_cols)) |>
            dplyr::arrange(.data$.ontology_path) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("n_"),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("rate_"),
                  dplyr::starts_with("pooled_"),
                  dplyr::starts_with("ia_tst_tot_"),
                  dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
    if ("secondary_bsi_rate_table" %in% elements) {
      tbl <- ds$secondary_bsi_rate_table

      # Skip if table is NULL or empty
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Key column is event_type_key
        tbl <- tbl |>
          dplyr::rename_with(~ paste0(.x, suffix), !"event_type_key")

        if (is.null(output$secondary_bsi_rate_table)) {
          output$secondary_bsi_rate_table <- tbl
        } else {
          output$secondary_bsi_rate_table <- output$secondary_bsi_rate_table |>
            dplyr::full_join(tbl, dplyr::join_by("event_type_key")) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("n_"),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
    if ("infectious_agent_detection_rate_per_inf_type_table" %in% elements) {
      tbl <- ds$infectious_agent_detection_rate_per_inf_type_table

      # Skip if table is NULL or empty
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Use any_of to handle tables with different structures
        # Note: function returns "inf" as key column (renamed from
        # event_type_key)
        # ToDo: Check why event_type_key even appears here. It's definitely
        # an internal name that should not appear here
        key_cols <- intersect(names(tbl), c("inf", "event_type_key"))
        tbl <- tbl |>
          dplyr::rename_with(
            ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

        if (is.null(
          output$infectious_agent_detection_rate_per_inf_type_table)) {
          output$infectious_agent_detection_rate_per_inf_type_table <- tbl
        } else {
          # Use the actual key column that exists in the data
          join_col <- intersect(
            names(output$infectious_agent_detection_rate_per_inf_type_table),
            c("inf", "event_type_key"))[1]
          output$infectious_agent_detection_rate_per_inf_type_table <-
            output$infectious_agent_detection_rate_per_inf_type_table |>
            dplyr::full_join(tbl, dplyr::join_by(!!join_col)) |>
            dplyr::mutate(
              dplyr::across(
                c(dplyr::starts_with("inf_with_pathogen_"),
                  dplyr::starts_with("n_")),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
    if ("resistance_test_rate_table" %in% elements) {
      tbl <- ds$resistance_test_rate_table

      # Skip if table is NULL or empty
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Use any_of to handle tables with different structures
        key_cols <- intersect(names(tbl), c("abr", "cond"))
        tbl <- tbl |>
          dplyr::rename_with(
            ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

        if (is.null(output$resistance_test_rate_table)) {
          output$resistance_test_rate_table <- tbl
        } else {
          output$resistance_test_rate_table <-
            output$resistance_test_rate_table |>
            dplyr::full_join(tbl, dplyr::join_by("abr", "cond")) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("n_"),
                ~tidyr::replace_na(.x, 0)),
              dplyr::across(
                c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                  dplyr::starts_with("ci_lower_"),
                  dplyr::starts_with("ci_upper_")),
                ~tidyr::replace_na(.x, NA_real_))
            )
        }
      }
    }
  }
  structure(output, class = c("neoipcr_bnch_ds", "list"))
}

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
        drop_quartiles = n_deps < 5 | round(100 / .data$pooled) >= median_patient_days,
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

#' Get the table with rates of surgical procedures
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing the rates of surgical procedures
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
      drop_quartiles = n_deps < 5 | round(100 / .data$pooled) >= median_patients,
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
        drop_quartiles = n_deps < 5 | round(1000 / .data$pooled) >= median_patient_days,
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

    r <- r |>
      dplyr::left_join(boot_cis, by = "inf") |>
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
          FALSE),
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
          tidyr::unnest_wider(col = .data$q, names_sep = ""),
        dplyr::join_by("event_type_key")) |>
      dplyr::inner_join(
        dep_stats,
        dplyr::join_by("event_type_key")) |>
      ensure_quartile_cols() |>
      dplyr::mutate(
        drop_quartiles = .data$n < 5 | round(100 / .data$pooled) >= .data$median,
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

    r <- r |>
      dplyr::left_join(boot_cis, by = "event_type_key") |>
      dplyr::select(!c("drop_quartiles","n","median")) |>
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

      for (j in 1:nrow(o)) {
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

          for (k in 1:nrow(g)) {
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

  # VRE only has one genus
  g <- x |>
    get_resistance(
      resistance = "vre",
      group_cols = "genus",
      use_cache = use_cache) |>
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
            ~quantile(.x, prob = c(.25, .5, .75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(Q = c("q1", "q2", "q3"))) |>
        tidyr::pivot_longer(!"Q", names_to = "event_type_key") |>
        tidyr::pivot_wider(names_from = "Q", values_from = "value")

      # Determine if quartiles should be dropped
      r <- r |>
        dplyr::mutate(
          drop_quartiles = n_deps < 5 | round(100 / .data$pooled) >= median_n) |>
        dplyr::left_join(quartiles, dplyr::join_by("event_type_key")) |>
        dplyr::mutate(
          dplyr::across(
            c("q1", "q2", "q3"),
            ~dplyr::if_else(.data$drop_quartiles, NA_real_, .x)))

      # Bootstrap CIs for quartiles where gate passes
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

      r <- r |>
        dplyr::left_join(boot_cis, by = "event_type_key") |>
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

#' Prettify the names of a neoipcr object
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns the same object as x but with pretty and potentially translated
#'  names
#' @export
pretty_names <- function(x, ...) {
  UseMethod("pretty_names")
}

#' @export
pretty_names.default <- function(x, ...) x

#' @export
pretty_names.neoipcr_tbl_sr_ref <- function(x, ...) {
  col_names <- stats::setNames(
    gettext("Procedure category","N","Pooled","Q1","Q2","Q3"),
    c("pro_cat","n","pooled","q1","q2","q3"))

  pairs <- x |>
    dplyr::select("pro_cat") |>
    dplyr::mutate(
      pretty_name = get_procedure_category_pretty(.data$pro_cat))

  row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

  attr(x, "names.pretty") <- col_names
  attr(x, "row.names.pretty") <- row_names

  x |>
    dplyr::inner_join(pairs, dplyr::join_by("pro_cat")) |>
    dplyr::mutate(pro_cat = .data$pretty_name, .keep = "unused") |>
    dplyr::rename_with(
      ~ dplyr::case_match(
        .x,
        "pro_cat"~col_names[["pro_cat"]],
        "n"~col_names[["n"]],
        "pooled"~col_names[["pooled"]],
        "q1"~col_names[["q1"]],
        "q2"~col_names[["q2"]],
        "q3"~col_names[["q3"]],
        .default = .x))
}

get_dev_ass_incidence_density_rates <- function(
    x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "dev_ass_incidence_density_rates"
  else
    cache_key <- paste0("dev_ass_incidence_density_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  # Extract device-associated events, yielding event_key + dev columns.
  # dev_ass may be absent when there are no events of that type in the dataset.
  extract_dev_ass <- function(data, dev_map) {
    if (!("dev_ass" %in% names(data)))
      return(tibble::tibble(event_key = integer(), dev = character()))
    data |>
      dplyr::select("event_key", "dev_ass") |>
      dplyr::filter(.data$dev_ass != 0) |>
      dplyr::mutate(
        dev = dplyr::case_match(
          as.integer(as.character(.data$dev_ass)),
          !!!dev_map),
        .keep = "unused")
  }

  x$events |>
    dplyr::inner_join(
      extract_dev_ass(x$sepsisData, list(1 ~ "cvc", 2 ~ "pvc")),
      dplyr::join_by("event_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(
      c(group_cols,"dev")))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::right_join(
      get_risk_time(x, group_cols, use_cache) |>
        dplyr::select(
          tidyselect::all_of(c(group_cols,"cvc_days","pvc_days"))) |>
        tidyr::pivot_longer(
          !tidyselect::all_of(group_cols),
          names_pattern = "^([^_]+)",
          names_to = "dev",
          values_to = "days"),
      by = c(group_cols,"dev")) |>
    dplyr::bind_rows(
      x$events |>
        dplyr::inner_join(
          extract_dev_ass(x$pneumoniaData, list(1 ~ "niv", 2 ~ "inv")),
          dplyr::join_by("event_key")) |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(
          c(group_cols,"dev")))) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = "dev",
          values_from = "n",
          values_fill = 0) |>
        dplyr::rowwise() |>
        dplyr::mutate(vs = sum(dplyr::c_across(tidyselect::any_of(c("niv","inv"))))) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          !tidyselect::all_of(c(group_cols)),
          names_to = "dev",
          values_to = "n") |>
        dplyr::right_join(
          get_risk_time(x, group_cols, use_cache) |>
            dplyr::select(
              tidyselect::all_of(c(group_cols,"inv_days","niv_days","vs_days"))) |>
            tidyr::pivot_longer(
              !tidyselect::all_of(group_cols),
              names_pattern = "^([^_]+)",
              names_to = "dev",
              values_to = "days"),
          by = c(group_cols,"dev"))) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      rate = .data$n / .data$days * 1000) |>
    cache(x, cache_key)
}

get_incidence_density_rates <- function(
    x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "incidence_density_rates"
  else
    cache_key <- paste0("incidence_density_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- x$events |>
    dplyr::filter(.data$event_type_key %in% c("bsi","nec","hap")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(
      c("event_type_key",group_cols)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "event_type_key",
      values_from = "n",
      values_fill = 0)  |>
    dplyr::rowwise() |>
    dplyr::mutate(si = sum(dplyr::c_across(tidyselect::any_of(c("bsi","hap"))))) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(!tidyselect::all_of(group_cols), names_to = "inf", values_to = "n")

  if(is.null(group_cols))
    r <- r |>
    dplyr::bind_cols(
      get_risk_time(x, use_cache = use_cache) |>
        dplyr::select("patient_days"))
  else
    r <- r |>
    dplyr::right_join(
      get_risk_time(x, group_cols, use_cache) |>
        dplyr::select(tidyselect::all_of(c(group_cols,"patient_days"))),
      by = group_cols)

  r |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      rate = .data$n / .data$patient_days * 1000) |>
    cache(x, cache_key)
}

get_infectious_agent_detection_rates_with_department_quartiles <- function(
    x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "infectious_agent_detection_rates_with_department_quartiles"
  else
    cache_key <- paste0("infectious_agent_detection_rates_with_department_quartiles_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_with_pathogen <- x |>
    get_infection_counts(
      group_cols = c("department_key","with_pathogen"),
      use_cache = use_cache) |>
    dplyr::filter(.data$with_pathogen) |>
    dplyr::pull("n")

  n_deps <- length(inf_with_pathogen)
  median_inf_with_pathogen <- stats::median(inf_with_pathogen)

  r1 <- x |>
    get_infectious_agent_detection_rates(
      group_cols = group_cols,
      use_cache = use_cache) |>
    dplyr::select(c(group_cols,"n","inf_with_pathogen","rate"="n_per_iwp")) |>
    dplyr::mutate(
      drop_quartiles = n_deps < 5 | round(100 / .data$rate) >= median_inf_with_pathogen)

  if(nrow(r1) < 1)
  {
    gc <- stats::setNames(
      as.list(rep(NA_character_, length(group_cols))),
      group_cols)
    return(
      tibble::tibble(
        n = 0,
        rate = NA_real_,
        drop_quartiles = TRUE,
        q1 = NA_real_,
        q2 = NA_real_,
        q3 = NA_real_
        ) |>
        dplyr::bind_cols(gc)
      )
  }

  dept_data <- x |>
    get_infectious_agent_detection_rates(
      group_cols = c("department_key", group_cols),
      use_cache = use_cache)
  r2 <- dept_data |>
    dplyr::select(c("department_key", group_cols,"n_per_iwp"))

  if (nrow(r2) < 1) {
    return(
      r1 |>
        dplyr::mutate(q1 = NA_real_, q2 = NA_real_, q3 = NA_real_) |>
        cache(x, cache_key))
  }

  if (!is.null(group_cols))
  {
    r2 <- r2 |>
    tidyr::pivot_wider(
      names_from = group_cols,
      values_from = "n_per_iwp",
      values_fill = 0)

    glue_spec <- "{.value}_{name}"
  }
  else glue_spec <- NULL

  r2 <- r2 |>
    dplyr::select(!"department_key") |>
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
    dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
    tidyr::pivot_wider(values_from = !"name", names_glue = glue_spec) |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_pattern = paste0(
        c("^", rep("(.+)_", length(group_cols)), "(q(?:1|2|3))$"),
        collapse = ""),
      names_to = c(group_cols,".value")) |>
    dplyr::mutate(
      dplyr::across(tidyselect::any_of(group_cols), ~ dplyr::na_if(.x,"NA")))

  if (is.null(group_cols))
    r <- r1 |> dplyr::bind_cols(r2)
  else
    r <- r1 |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(group_cols), as.character)) |>
    dplyr::inner_join(r2 , by = group_cols)

  # Bootstrap CIs for quartiles where gate passes
  na_boot <- tibble::tibble(q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
                            q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
                            q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
  if (nrow(r) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map_dfr(seq_len(nrow(r)), function(i) {
      if (r$drop_quartiles[i]) return(na_boot)
      d <- dept_data
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        d <- d |> dplyr::semi_join(r[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$inf_with_pathogen > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$n, d$inf_with_pathogen,
                            type = "poisson", multiplier = 100)
    })
  }

  r |> dplyr::mutate(
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
      .data$q3)) |>
    dplyr::bind_cols(boot_cis) |>
    cache(x, cache_key)
}

get_infectious_agent_detection_rates <- function(
    x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "infectious_agent_detection_rates"
  else
    cache_key <- paste0("infectious_agent_detection_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- x$events |>
    dplyr::inner_join(
      x$infectiousAgentFindings |>
        dplyr::inner_join(
          get_pathogen_taxonomy(
            x$infectiousAgentFindings$pathogen_key |> unique()),
          dplyr::join_by("pathogen_key" == "input_id")),
      dplyr::join_by("event_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # get_infection_counts cannot access specific pathogen related information
  # since this would lead to counting infections with multiple pathogens
  # multiple times
  inf_groups <- setdiff(group_cols, c(names(x$infectiousAgentFindings),names(get_pathogen_taxonomy(-1))))

  inf_counts <- x |>
    get_infection_counts(
      group_cols = c(inf_groups,"with_pathogen"),
      use_cache = use_cache) |>
    tidyr::pivot_wider(names_from = "with_pathogen", values_from = "n",
                       values_fill = 0L)
  if (!"TRUE" %in% names(inf_counts)) inf_counts[["TRUE"]] <- 0L
  if (!"FALSE" %in% names(inf_counts)) inf_counts[["FALSE"]] <- 0L
  inf_counts <- inf_counts |>
    dplyr::mutate(
      inf_with_pathogen = .data$`TRUE`,
      total_inf = .data$`TRUE` + .data$`FALSE`,
      .keep = "unused")

  if(is.null(group_cols))
    r <- r |> dplyr::bind_cols(inf_counts)
  else
    r <- r |>
    dplyr::right_join(inf_counts, by = inf_groups) |>
    dplyr::mutate(n = tidyr::replace_na(.data$n, 0))

  r |>
    dplyr::mutate(
      n_per_iwp = .data$n / .data$inf_with_pathogen * 100,
      n_per_t = .data$n / .data$total_inf * 100,
      iwp_per_t = .data$inf_with_pathogen / .data$total_inf * 100
    ) |>
    cache(x, cache_key)
}

get_infection_counts <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "infection_counts"
  else
    cache_key <- paste0("infection_counts_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_events <- c("bsi","nec","hap","ssi")

  event_base <- x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::filter(.data$event_type_key %in% inf_events) |>
    dplyr::mutate(
      event_type_key = factor(
        as.character(.data$event_type_key),
        levels = inf_events
      ),
      with_pathogen = .data$event_key %in% x$infectiousAgentFindings$event_key)

  counts <- event_base |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  if(is.null(group_cols))
    return(counts |>
             cache(x, cache_key))

  ee_intersect <- intersect(
    intersect(names(event_base), group_cols),
    names(x$enrollments))

  expanded <- x$enrollments |>
    dplyr::left_join(
      event_base |>
        dplyr::select(
          tidyselect::all_of(
            setdiff(c("enrollment_key","event_key",group_cols), ee_intersect))),
      dplyr::join_by("enrollment_key")) |>
    # Make sure both values for with_pathogen are expanded if necessary
    dplyr::bind_rows(list(with_pathogen = c(TRUE,FALSE))) |>
    tidyr::expand(!!! dplyr::syms(group_cols)) |>
    tidyr::drop_na()

  expanded |>
    dplyr::left_join(counts, by = group_cols) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0)) |>
    cache(x, cache_key)
}

get_resistance_test_rate_with_department_quartiles <- function(
    x, resistance, group_cols = NULL, use_cache = TRUE) {
  rate <- x |>
    get_resistance_test_rate(
      resistance = resistance,
      group_cols = group_cols,
      use_cache = use_cache)

  deps <- x |>
    get_resistance_test_rate(
      resistance = resistance,
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))

  if (nrow(deps) < 1) {
    return(
      rate |>
        dplyr::mutate(q1 = NA_real_, q2 = NA_real_, q3 = NA_real_))
  }

  dep_stats <- deps |>
    dplyr::summarise(
      n_deps = dplyr::n(),
      median = stats::median(.data$total),
      .groups = "drop")

  quartiles <- deps |>
    dplyr::reframe(
      value = stats::quantile(
        .data$rate,
        prob = c(.25,.5,.75),
        na.rm = TRUE)) |>
    dplyr::mutate(
      name=names(.data$value),
      name=dplyr::case_match(
        .data$name,
        "25%"~"q1",
        "50%"~"q2",
        "75%"~"q3")) |>
    tidyr::pivot_wider()

  if(is.null(group_cols))
    rate <- rate |>
    dplyr::bind_cols(dep_stats) |>
    dplyr::bind_cols(quartiles)
  else
    rate <- rate |>
    dplyr::inner_join(dep_stats, by = group_cols) |>
    dplyr::inner_join(quartiles, by = group_cols)

  rate <- rate |>
    dplyr::mutate(
      drop_quartiles = .data$n_deps < 5 | round(100 / .data$rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map_dfr(seq_len(nrow(rate)), function(i) {
      if (rate$drop_quartiles[i]) return(na_boot)
      d <- dplyr::ungroup(deps)
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        # Extract row i as a 1-row data frame; semi_join keeps only deps
        # rows whose group columns match, avoiding manual column extraction
        d <- d |> dplyr::semi_join(rate[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$total > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$tested, d$total,
                            type = "binomial", multiplier = 100)
    })
  }

  rate |>
    dplyr::bind_cols(boot_cis) |>
    dplyr::select(!c("n_deps","median", "drop_quartiles"))
}

get_resistance_test_rate <- function(
    x, resistance, group_cols = NULL, use_cache = TRUE) {
  res_names <- c("3gcr","car","cor","mrsa","vre")
  resistance <- rlang::arg_match(
    arg = resistance,
    res_names)

  check_character(group_cols, allow_na = FALSE, allow_null = TRUE)

  check_bool(use_cache)

  if(is.null(group_cols))
    cache_key <- paste0("resistance_test_rate_", resistance)
  else
    cache_key <- paste0("resistance_test_rate_", resistance, "_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  infectiousAgentFindings <- x$infectiousAgentFindings
  for(col in union(resistance, intersect(group_cols, res_names))) {
    if(!(col %in% names(infectiousAgentFindings)))
      infectiousAgentFindings <- infectiousAgentFindings |>
        dplyr::mutate(!!col := NA_character_)
  }

  r <- x$events |>
    dplyr::inner_join(infectiousAgentFindings, dplyr::join_by("event_key")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(resistance),
        ~ factor(
          dplyr::case_match(
            as.character(.x),
            "yes" ~ "tested",
            "no" ~ "tested",
            "not_tested" ~ "not_tested"),
          levels = c("tested","not_tested"))),
      # For the grouping columns that are resistances, we assume NA to be yes
      # because if we don't ask for resistance that's typically because of a
      # primary resistance
      dplyr::across(
        tidyselect::all_of(intersect(group_cols, res_names)),
        ~ tidyr::replace_na(.x, "yes")
      )) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols, resistance)))) |>
    dplyr::summarise(
      value=dplyr::n(),
      .groups = "drop") |>
    dplyr::filter(!is.na(.data[[resistance]])) |>
    tidyr::pivot_wider(
      names_from = resistance,
      values_fill = 0L,
      names_expand = TRUE)

  if(nrow(r) == 0)
    return(
      r |>
        dplyr::select(tidyselect::any_of(group_cols)) |>
        dplyr::mutate(
          tested = integer(), not_tested = integer(),
          total = integer(), rate = numeric()) |>
        cache(x, cache_key))

  r |>
    dplyr::mutate(
      tested = .data$tested,
      not_tested = .data$not_tested,
      total = .data$tested + .data$not_tested,
      rate = .data$tested / .data$total * 100,
      .keep = "unused") |>
    cache(x, cache_key)
}

get_resistance_rate_with_department_quartiles <- function(
    x, resistance, group_cols = NULL, use_cache = TRUE) {
  rate <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = group_cols,
      use_cache = use_cache)

  deps <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))

  if (nrow(deps) < 1) {
    r <- tibble::tibble(
      n = 0L,
      inf_w_ia = 0L,
      rate = NA_real_,
      q1 = NA_real_,
      q2 = NA_real_,
      q3 = NA_real_,
      q1_ci_lower = NA_real_,
      q1_ci_upper = NA_real_,
      q2_ci_lower = NA_real_,
      q2_ci_upper = NA_real_,
      q3_ci_lower = NA_real_,
      q3_ci_upper = NA_real_)
    if (!is.null(group_cols))
      r <- dplyr::bind_cols(
        tibble::tibble(!!!group_cols, .rows = 1, .name_repair = ~ group_cols),
        r)
    return(r)
  } else {
    dep_stats <- deps |>
      dplyr::summarise(
        n_deps = dplyr::n(),
        median = stats::median(.data$inf_w_ia),
        .groups = "drop")

    quartiles <- deps |>
      dplyr::reframe(
        value = stats::quantile(
          .data$inf_rs_rate,
          prob = c(.25,.5,.75),
          na.rm = TRUE)) |>
      dplyr::mutate(
        name=names(.data$value),
        name=dplyr::case_match(
          .data$name,
          "25%"~"q1",
          "50%"~"q2",
          "75%"~"q3")) |>
      tidyr::pivot_wider()
  }

  if (is.null(group_cols))
    rate <- rate |>
    dplyr::bind_cols(dep_stats) |>
    dplyr::bind_cols(quartiles)
  else
    rate <- rate |>
    dplyr::inner_join(dep_stats, by = group_cols) |>
    dplyr::inner_join(quartiles, by = group_cols)

  rate <- rate |>
    dplyr::mutate(
      drop_quartiles = .data$n_deps < 5 | round(100 / .data$inf_rs_rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map_dfr(seq_len(nrow(rate)), function(i) {
      if (rate$drop_quartiles[i]) return(na_boot)
      d <- dplyr::ungroup(deps)
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        # Extract row i as a 1-row data frame; semi_join keeps only deps
        # rows whose group columns match, avoiding manual column extraction
        d <- d |> dplyr::semi_join(rate[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$inf_w_ia > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$inf_rs, d$inf_w_ia,
                            type = "binomial", multiplier = 100)
    })
  }

  rate |>
    dplyr::bind_cols(boot_cis) |>
    dplyr::select(!c("inf_nrs", "inf_tst_tot", "ia_rs", "ia_nrs",
                     "ia_tst_tot", "ia_rs_rate","n_deps","median",
                     "drop_quartiles")) |>
    dplyr::rename("n"="inf_rs","rate"="inf_rs_rate")
}

get_organism_resistance_rate_with_department_quartiles <- function(
    x, resistance, group_cols = NULL, use_cache = TRUE) {
  rate <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = group_cols,
      use_cache = use_cache)

  deps <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))

  if (nrow(deps) < 1) {
    r <- tibble::tibble(
      n = 0L,
      ia_tst_tot = 0L,
      rate = NA_real_,
      q1 = NA_real_,
      q2 = NA_real_,
      q3 = NA_real_,
      q1_ci_lower = NA_real_,
      q1_ci_upper = NA_real_,
      q2_ci_lower = NA_real_,
      q2_ci_upper = NA_real_,
      q3_ci_lower = NA_real_,
      q3_ci_upper = NA_real_)
    if (!is.null(group_cols))
      r <- dplyr::bind_cols(
        tibble::tibble(!!!group_cols, .rows = 1, .name_repair = ~ group_cols),
        r)
    return(r)
  } else {
    dep_stats <- deps |>
      dplyr::summarise(
        n_deps = dplyr::n(),
        median = stats::median(.data$ia_tst_tot),
        .groups = "drop")

    quartiles <- deps |>
      dplyr::reframe(
        value = stats::quantile(
          .data$ia_rs_rate,
          prob = c(.25,.5,.75),
          na.rm = TRUE)) |>
      dplyr::mutate(
        name=names(.data$value),
        name=dplyr::case_match(
          .data$name,
          "25%"~"q1",
          "50%"~"q2",
          "75%"~"q3")) |>
      tidyr::pivot_wider()
  }

  if (is.null(group_cols))
    rate <- rate |>
    dplyr::bind_cols(dep_stats) |>
    dplyr::bind_cols(quartiles)
  else
    rate <- rate |>
    dplyr::inner_join(dep_stats, by = group_cols) |>
    dplyr::inner_join(quartiles, by = group_cols)

  rate <- rate |>
    dplyr::mutate(
      drop_quartiles = .data$n_deps < 5 | round(100 / .data$ia_rs_rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map_dfr(seq_len(nrow(rate)), function(i) {
      if (rate$drop_quartiles[i]) return(na_boot)
      d <- dplyr::ungroup(deps)
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        d <- d |> dplyr::semi_join(rate[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$ia_tst_tot > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$ia_rs, d$ia_tst_tot,
                            type = "binomial", multiplier = 100)
    })
  }

  rate |>
    dplyr::bind_cols(boot_cis) |>
    dplyr::select(!c("inf_rs", "inf_nrs", "inf_tst_tot", "inf_w_ia",
                     "ia_nrs", "inf_rs_rate", "n_deps", "median",
                     "drop_quartiles")) |>
    dplyr::rename("n"="ia_rs","rate"="ia_rs_rate")
}

get_resistance_rate <- function(
    x, resistance, group_cols = NULL, use_cache = TRUE) {
  res_names <- c("3gcr","car","cor","mrsa","vre")
  resistance <- rlang::arg_match(
    arg = resistance,
    res_names)

  check_character(group_cols, allow_na = FALSE, allow_null = TRUE)

  check_bool(use_cache)

  if(is.null(group_cols))
    cache_key <- paste0("resistance_rate_", resistance)
  else
    cache_key <- paste0("resistance_rate_", resistance, "_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_group_cols <- c(
    names(x$patients),
    names(x$enrollments),
    names(x$events)) |>
    unique() |>
    intersect(group_cols)

  inf_w_ia <- x |>
    get_infection_counts(
      group_cols = c("with_pathogen", inf_group_cols),
      use_cache = use_cache) |>
    dplyr::filter(.data$with_pathogen) |>
    dplyr::select(!"with_pathogen")

  infectiousAgentFindings <- x$infectiousAgentFindings
  for(col in union(resistance, intersect(group_cols, res_names))) {
    if(!(col %in% names(infectiousAgentFindings)))
      infectiousAgentFindings <- infectiousAgentFindings |>
        dplyr::mutate(!!col := NA_character_)
  }

  r <- x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              infectiousAgentFindings |>
                dplyr::select(
                  c(
                    "event_key","secondary_bsi","pathogen_key","index",
                    "source",tidyselect::all_of(resistance))
                ) |>
                dplyr::inner_join(
                  get_pathogen_taxonomy(),
                  dplyr::join_by("pathogen_key" == "input_id")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols, resistance)))) |>
    dplyr::summarise(
      inf = dplyr::n_distinct(.data$event_key),
      ia_dtct = dplyr::n(),
      .groups = "drop") |>
    dplyr::filter(!is.na(.data[[resistance]]) & .data[[resistance]] != "not_tested") |>
    tidyr::pivot_wider(
      names_from = resistance,
      values_from = c("inf","ia_dtct"),
      values_fill = 0L,
      names_expand = TRUE) |>
    dplyr::select(!tidyselect::ends_with("not_tested"))

  if(nrow(r) == 0)
    return(
      r |>
        dplyr::select(tidyselect::any_of(group_cols)) |>
        dplyr::mutate(
          inf_rs = numeric(), inf_nrs = numeric(),
          inf_tst_tot = numeric(), inf_w_ia = numeric(),
          ia_rs = numeric(), ia_nrs = numeric(),
          ia_tst_tot = numeric(), ia_rs_rate = numeric(),
          inf_rs_rate = numeric()) |>
        cache(x, cache_key))

  if(length(inf_group_cols) < 1)
    r <- r |>
    dplyr::bind_cols(inf_w_ia)
  else
    r <- r |>
    dplyr::inner_join(inf_w_ia, by = inf_group_cols)

  r |>
    dplyr::mutate(
      inf_rs = .data$inf_yes,
      inf_nrs = .data$inf_no,
      inf_tst_tot = .data$inf_rs + .data$inf_nrs,
      inf_w_ia = .data$n,
      ia_rs = .data$ia_dtct_yes,
      ia_nrs = .data$ia_dtct_no,
      ia_tst_tot = .data$ia_rs + .data$ia_nrs,
      ia_rs_rate = .data$ia_rs / .data$ia_tst_tot * 100,
      inf_rs_rate = .data$inf_rs / .data$inf_w_ia * 100,
      .keep = "unused") |>
    cache(x, cache_key)
}

get_secondary_bsi_rates <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "secondary_bsi_rates"
  else
    cache_key <- paste0("secondary_bsi_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  # Count infections with secondary BSI for each type
  infections_with_sec_bsi <- x$events |>
    dplyr::filter(.data$event_type_key %in% c("nec","ssi","hap")) |>
    dplyr::semi_join(
      x$infectiousAgentFindings |>
        dplyr::filter(.data$secondary_bsi),
      dplyr::join_by("event_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(
      c("event_type_key", group_cols)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # Count infections with follow-up for each type
  filter_sec_bsi <- function(d) {
    if(!("sec_bsi" %in% names(d))) return(d[0,])
    d |> dplyr::filter(.data$sec_bsi != -1)
  }
  infections_with_followup <- dplyr::bind_rows(
    x$events |>
      dplyr::semi_join(
        filter_sec_bsi(x$necData),
        dplyr::join_by("event_key")) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::summarise(
        followup_n = dplyr::n(),
        event_type_key = "nec",
        .groups = "drop"),
    x$events |>
      dplyr::semi_join(
        filter_sec_bsi(x$pneumoniaData),
        dplyr::join_by("event_key")) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::summarise(
        followup_n = dplyr::n(),
        event_type_key = "hap",
        .groups = "drop"),
    x$events |>
      dplyr::semi_join(
        filter_sec_bsi(x$ssiData),
        dplyr::join_by("event_key")) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::summarise(
        followup_n = dplyr::n(),
        event_type_key = "ssi",
        .groups = "drop")
  )

  # Join and calculate rates
  join_cols <- c("event_type_key", group_cols)
  r <- infections_with_followup |>
    dplyr::left_join(
      infections_with_sec_bsi,
      by = join_cols) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      rate = .data$n / .data$followup_n * 100) |>
    cache(x, cache_key)

  r
}

get_risk_population <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "risk_population"
  else
    cache_key <- paste0("risk_population_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$patients |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(tidyselect::all_of(c("patient_key",setdiff(names(x$enrollments),names(x$patients))))),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      n_enrollments = dplyr::n(),
      n_patients = dplyr::n_distinct(.data$patient_key),
      .groups = "drop") |>
    cache(x, cache_key)
}

get_surgery_risk <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "surgery_risk"
  else
    cache_key <- paste0("surgery_risk_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$surveillanceEndData |>
    dplyr::left_join(aware_days, dplyr::join_by("event_key")) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("a_days", "w_days", "r_days")),
      ~ tidyr::replace_na(.x, 0L))) |>
    dplyr::inner_join(
      x$events |>
        dplyr::select("event_key","enrollment_key"),
      dplyr::join_by("event_key")) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              x$surveillanceEndData |>
                dplyr::inner_join(
                  get_aware_days(x, use_cache),
                  dplyr::join_by("event_key")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(c("enrollment_key","event_key")) |>
            dplyr::inner_join(
              surgeryData |>
                dplyr::mutate(
                  main_procedure_category = get_procedure_category(.data$main_procedure_code, not_surgery_na = TRUE),
                  dplyr::across(
                    tidyselect::any_of(
                      "side_procedure_code_1"),
                    ~ get_procedure_category(.x, not_surgery_na = TRUE),
                    .names = "side_procedure_1_category"),
                  dplyr::across(
                    tidyselect::any_of(
                      "side_procedure_code_2"),
                    ~ get_procedure_category(.x, not_surgery_na = TRUE),
                    .names = "side_procedure_2_category")) |>
                dplyr::filter(dplyr::if_any(tidyselect::matches("procedure_(\\d_)?category$"), ~ !is.na(.x))),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(tidyselect::any_of("department_key"), ~ dplyr::n_distinct(.x), .names = "n_departments"),
      dplyr::across(tidyselect::any_of("patient_key"), ~ dplyr::n_distinct(.x), .names = "n_patients"),
      n_procedures = dplyr::n(),
      .groups = "drop") |>
    cache(x, cache_key)
}

get_risk_time <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "risk_time"
  else
    cache_key <- paste0("risk_time_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
      return(r)

  x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              x$surveillanceEndData |>
                dplyr::inner_join(
                  get_aware_days(x, use_cache),
                  dplyr::join_by("event_key")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(dplyr::across(!"total_gestation_days" & tidyselect::ends_with("_days"), sum), .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(
        !tidyselect::all_of(c(group_cols,"patient_days")),
        ~ .x / .data$patient_days * 100,
        .names = "{.col}_rate")) |>
    dplyr::rename_with(~ stringr::str_remove_all(.x, "days_")) |>
    cache(x, cache_key)
}

get_procedures <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "procedures"
  else
    cache_key <- paste0("procedures_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  surgeryData <- x$surgeryData
  if(!("main_procedure_code" %in% names(surgeryData)))
    surgeryData <- surgeryData |>
      dplyr::mutate(main_procedure_code = character())

  x$events |>
    dplyr::inner_join(
      surgeryData |>
        dplyr::inner_join(
          get_procedure_categories(x, use_cache = use_cache),
          dplyr::join_by("main_procedure_code" == "procedure_code")),
      dplyr::join_by("event_key")) |>
    dplyr::filter(.data$pro_cat != "not_surgery") |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    cache(x, cache_key)
}

get_substance_days <- function(
    x, level = c("substance", "atc5", "aware"), use_cache = TRUE) {
  level <- match.arg(level)
  cache_key <- paste0("substance_days.", level)
  if (use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  # Per-event, per-substance base data
  base <- x$substanceDays |>
    dplyr::group_by(.data$event_key, .data$substance_code) |>
    dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
    dplyr::inner_join(
      x$metadata$antimicrobialSubstances |>
        dplyr::select(tidyselect::all_of(c("code", "displayName", "WHO_AWARE", "ATC5"))),
      dplyr::join_by("substance_code" == "code"))

  r <- switch(level,
    substance = base |>
      dplyr::transmute(
        .data$event_key,
        code = .data$substance_code,
        display_name = as.character(.data$displayName),
        .data$days),

    atc5 = {
      atc5_names <- x$metadata$atc5Categories |>
        dplyr::transmute(
          atc5_code = as.character(.data$code),
          atc5_name = as.character(.data$displayName))

      base |>
        dplyr::mutate(atc5_code = as.character(.data$ATC5)) |>
        dplyr::left_join(atc5_names, dplyr::join_by("atc5_code")) |>
        dplyr::group_by(.data$event_key, .data$atc5_code, .data$atc5_name) |>
        dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
        dplyr::transmute(
          .data$event_key,
          code = .data$atc5_code,
          display_name = .data$atc5_name,
          .data$days)
    },

    aware = base |>
      dplyr::mutate(
        AWaRe = factor(
          tolower(
            stringr::str_extract(
              .data$WHO_AWARE,
              "^WHO_AWARE_(A|W|R).+$",
              group = 1)),
          levels = c("a", "w", "r"))) |>
      dplyr::group_by(.data$event_key, .data$AWaRe) |>
      dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
      dplyr::transmute(
        .data$event_key,
        code = .data$AWaRe,
        display_name = NA_character_,
        .data$days)
  )

  cache(r, x, cache_key)
}

summarise_substance_days <- function(
    x, level = c("substance", "atc5", "aware"),
    group_cols = NULL, use_cache = TRUE) {
  level <- match.arg(level)
  cache_key <- if (is.null(group_cols))
    paste0("substance_days_sum.", level)
  else
    paste0("substance_days_sum.", level, ".", paste0(group_cols, collapse = "."))
  if (use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  # Per-event, per-substance base data
  base <- x$substanceDays |>
    dplyr::group_by(.data$event_key, .data$substance_code) |>
    dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
    dplyr::inner_join(
      x$metadata$antimicrobialSubstances |>
        dplyr::select(tidyselect::all_of(c("code", "displayName", "WHO_AWARE", "ATC5"))),
      dplyr::join_by("substance_code" == "code"))

  # Join group_cols from events if needed
  if (!is.null(group_cols)) {
    base <- base |>
      dplyr::inner_join(
        x$events |>
          dplyr::select(tidyselect::all_of(c("event_key", group_cols))),
        dplyr::join_by("event_key"))
  }

  r <- switch(level,
    substance = base |>
      dplyr::group_by(
        dplyr::across(tidyselect::all_of(group_cols)),
        code = .data$substance_code,
        display_name = as.character(.data$displayName)) |>
      dplyr::summarise(days = sum(.data$days), .groups = "drop"),

    atc5 = {
      atc5_names <- x$metadata$atc5Categories |>
        dplyr::transmute(
          atc5_code = as.character(.data$code),
          atc5_name = as.character(.data$displayName))

      base |>
        dplyr::mutate(atc5_code = as.character(.data$ATC5)) |>
        dplyr::left_join(atc5_names, dplyr::join_by("atc5_code")) |>
        dplyr::group_by(
          dplyr::across(tidyselect::all_of(group_cols)),
          code = .data$atc5_code,
          display_name = .data$atc5_name) |>
        dplyr::summarise(days = sum(.data$days), .groups = "drop")
    },

    aware = base |>
      dplyr::mutate(
        AWaRe = factor(
          tolower(
            stringr::str_extract(
              .data$WHO_AWARE,
              "^WHO_AWARE_(A|W|R).+$",
              group = 1)),
          levels = c("a", "w", "r"))) |>
      dplyr::group_by(
        dplyr::across(tidyselect::all_of(group_cols)),
        code = .data$AWaRe) |>
      dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
      dplyr::mutate(display_name = NA_character_)
  )

  cache(r, x, cache_key)
}

get_aware_days <- function(x, use_cache = TRUE) {
  if (use_cache && !is.null(r <- get_cached(x, "aware_days")))
    return(r)

  get_substance_days(x, level = "aware", use_cache = use_cache) |>
    dplyr::select("event_key", AWaRe = "code", "days") |>
    tidyr::pivot_wider(
      names_from = "AWaRe",
      values_from = "days",
      names_glue = "{AWaRe}_{.value}",
      values_fill = 0L) |>
    cache(x, "aware_days")
}

get_procedure_categories <- function(
    x, pretty = FALSE, include_iche = FALSE, use_cache = TRUE) {
  cache_key <- "procedure_categories"

  # ToDo: Clarify licensing and inclusion criteria for ICHE information with WHO
  # and add ICHE table
  # if(include_iche)
  #   cache_key <- paste0(cache_key, ".iche")

  if(pretty)
  {
    l <- Sys.getenv("LANGUAGE")
    if(l == "")
      l <- Sys.getlocale("LC_MESSAGES")
    if(l == "")
      pretty <- FALSE # just in case
    else
      cache_key <- paste0(cache_key, ".", l)
  }

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- tibble::tibble(
    procedure_code = c(
      x$surgeryData$main_procedure_code,
      x$surgeryData$side_procedure_code_1,
      x$surgeryData$side_procedure_code_2) |>
      unique() |>
      sort() |>
      as.character()) |>
    dplyr::mutate(pro_cat = get_procedure_category(.data$procedure_code))

  if(pretty)
  {

    with_pretty <- r |>
      dplyr::select("pro_cat") |>
      dplyr::mutate(
        pretty_name = get_procedure_category_pretty(.data$pro_cat))

    pairs <- with_pretty |> dplyr::distinct()

    col_names <- stats::setNames(
      gettext("Procedure code","Procedure category"),
      c("procedure_code","pro_cat"))
    row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

    attr(r, "names.pretty") <- col_names
    attr(r, "row.names.pretty") <- row_names

    r <- r |>
    dplyr::mutate(
      pro_cat = with_pretty$pretty_name) |>
    dplyr::rename(
      !!col_names[["procedure_code"]] := .data$procedure_code,
      !!col_names[["pro_cat"]] := .data$pro_cat)
  }

  # if(include_iche)
  #   r <- r |>
  #   dplyr::inner_join(
  #     ichi_health_interventions,join_by(main_procedure_code == code))

  r |>
    cache(x, cache_key)
}

get_procedure_category <- function(x, not_surgery_na = FALSE) {
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)
  if (not_surgery_na) {
    not_surgery <- NA_character_
  } else {
    not_surgery <- "not_surgery"
  }

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    # Neurosurgery
    ############################################################################
    target %in% c(
      "AAE",# Interventions on ventricles of brain
      "AAG",# Interventions on intracranial space
      "ABA",# Interventions on spinal cord
      "ABG",# Interventions on spinal canal
      "MAA" # Interventions on skull
      ) &
      means %in% c("AA","AB","AE") ~ "neurosurgery",

    # Cardiac/large vessel surgery
    ############################################################################
    target == "HIJ" & action == "LA" ~ "cardiac_and_large_vessel_surgery",

    target == "HIK" & means == "AA" ~ "cardiac_and_large_vessel_surgery",

    # Lung/pleural space/thoracic surgery
    ############################################################################
    target %in% c(
      "MCX",# Interventions on diaphragm
      "JBF",# Interventions on lung parenchyma
      "JCA",# Interventions on pleura
      "JCB",# Interventions on pleura
      "JCH" # Interventions on thoracic cavity
    ) &
      means %in% c("AA","AB") ~ "lung_pleural_space_thoracic_surgery",

    # Oesophageal surgery
    ############################################################################
    target == "KBA" & means %in% c("AA","AB") ~ "oesophageal_surgery",

    # Abdominal surgery
    ############################################################################
    target %in% c(
      "KBF",# Interventions on stomach
      "KBI",# Interventions on duodenum
      "KBK",# Interventions on small intestine, not elsewhere classified
      "KBO",# Interventions on appendix
      "KBP",# Interventions on colon
      "KBZ",# Interventions on large intestine, not elsewhere classified
      "KMA",# Interventions on peritoneum
      "PAK",# Interventions on abdomen, not otherwise specified
      "PAL",# Interventions on abdominal wall, not otherwise specified
      "PAO" # Interventions on abdominal wall, umbilical
      ) &
      means %in% c("AA","AB") ~ "abdominal_surgery",

    target %in% c(
      "PTA",
      "PTB"
      ) &
      action == "LA" &
      means == "AC" ~ "abdominal_surgery",

    x == "KMA.JB.AE" ~ "abdominal_surgery",# Percutaneous drainage of peritoneal cavity
    x == "KZZ.MK.AA" ~ "abdominal_surgery",# Repair of intestine, not elsewhere classified
    x == "PAK.JB.AE" ~ "abdominal_surgery",# Percutaneous abdominal drainage

    # Inguinal hernia surgery
    ############################################################################
    x %in% c(
      "PAM.MK.AA",# Repair of inguinal hernia
      "PAM.MK.AB" # Laparoscopic repair of inguinal hernia
    ) ~ "inguinal_hernia_surgery",

    # Other
    ############################################################################
    x %in% c(
      "BCC.GA.AA",# Destruction of retina
      "BCD.DB.AE",# Injection into vitreous body
      "HDG.LG.AF",# Percutaneous transluminal balloon dilatation of pulmonary valve
      "HIB.DL.AF",# Percutaneous transluminal insertion of device into superior vena cava
      "IBD.DL.AF",# Percutaneous transluminal insertion of device into vein of head and neck
      "IZD.DL.AF",# Insertion of a device into a vein, not elsewhere classified
      "JAN.AE.AC",# Laryngoscopy
      "JAN.MK.AD",# Endoscopic repair of larynx
      "JAM.ML.AD",# Endoscopic reconstruction of nasopharynx
      "JBA.AE.AB",# Tracheoscopy through artificial stoma
      "JBA.KA.AC",# Replacement of tracheal device
      "JBA.LI.AA",# Tracheostomy
      "JBA.MK.AA",# Repair of trachea
      "KAA.AD.AA",# Biopsy of lip
      "KAB.FB.AC",# Lingual fraenotomy
      "LAB.JG.AH",# Debridement of skin and subcutaneous cell tissue of trunk, without incision
      "LAB.LL.AA",# Reduction of skin and subcutaneous cell tissue of trunk
      "LCA.JG.AA",# Debridement of breast with incision
      "NAM.MK.AA",# Repair of urethra
      "NGL.LC.AA",# Orchiopexy
      "NMR.MK.AB",# Endoscopic repair of fetal or embryonic structure
      "NZZ.ZZ.ZZ",# Interventions on the genitourinary system, unspecified
      "PAW.JB.AA" # Drainage of perineum
      ) ~ "other",

    # Not considered as surgery (remove)
    ############################################################################
    x %in% c(
      "ABA.BA.BH",# Magnetic resonance imaging of spinal cord
      "JBB.AE.AD",# Bronchoscopy
      "KBA.LG.AD",# Endoscopic dilatation of oesophagus
      "KBF.DL.AC",# Insertion of device into stomach
      "KBF.KA.AC",# Replacement of gastric device
      "KBK.LD.AH",# Manual reduction of ileostomy prolapse
      "LZZ.DK.AH",# Application of dressing to skin or subcutaneous cell tissue, not elsewhere classified
      "MBO.BA.BC",# Computerised tomography of lumbosacral spine, not elsewhere classified
      "PAB.BA.BH",# Magnetic resonance imaging of head or neck
      "PAE.BA.BH",# Magnetic resonance imaging of thorax
      "PAK.BA.BH",# Magnetic resonance imaging of abdomen
      "PTB.SN.AC",# Management of enterostomy
      "PTA.PM.ZZ",# Gastrostomy education
      "PTC.PM.ZZ",# Tracheostomy education
      "PZA.BA.BH" # Magnetic resonance imaging of whole body
      ) ~ not_surgery,

    # To be categorised (default)
    ############################################################################
    .default = "to_be_categorised"
  ) |>
    factor(
      levels = c(
        "abdominal_surgery",
        "neurosurgery",
        "inguinal_hernia_surgery",
        "cardiac_and_large_vessel_surgery",
        "lung_pleural_space_thoracic_surgery",
        "oesophageal_surgery",
        "other",
        not_surgery,
        "to_be_categorised"))
}

get_procedure_category_pretty <- function(x) {
  dplyr::case_match(
    as.character(x),
    "overall" ~ gettext("Overall"),
    "abdominal_surgery" ~ gettext("Abdominal surgery"),
    "neurosurgery" ~ gettext("Neurosurgery"),
    "inguinal_hernia_surgery" ~ gettext("Inguinal hernia surgery"),
    "cardiac_and_large_vessel_surgery" ~ gettext("Cardiac- / large vessel surgery"),
    "lung_pleural_space_thoracic_surgery" ~ gettext("Lung- / pleural space- / thoracic surgery"),
    "oesophageal_surgery" ~ gettext("Oesophageal surgery"),
    "other" ~ gettext("Other"),
    "not_surgery" ~ gettext("Not a surgical procedure"),
    "to_be_categorised" ~ gettext("Not yet categorised"),
    .default = x
  )
}

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

# --- Antibiotic utilisation table ---

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
        drop_quartiles = n_deps < 5 |
          round(100 / .data$pooled) >= median_patient_days,
        q1 = dplyr::if_else(.data$drop_quartiles, NA, .data$q1),
        q2 = dplyr::if_else(.data$drop_quartiles, NA, .data$q2),
        q3 = dplyr::if_else(.data$drop_quartiles, NA, .data$q3))

    # Bootstrap CIs for quartiles where gate passes
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
      dplyr::left_join(boot_cis, by = "row_id") |>
      dplyr::select(!"drop_quartiles")
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

# --- Reference surgery-rate table ---

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
