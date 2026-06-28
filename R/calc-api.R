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
  # Three-valued gates all need to be non-"no" for the reference
  # pipeline: department grouping, per-patient / per-enrollment / per-
  # event denominators, and hierarchy metadata for the country-level
  # joins all feed into the computed reference data.
  assert_options_for(x, required = list(
    include_department = c("pseudo", "full"),
    include_patient    = c("pseudo", "full"),
    include_enrollment = c("pseudo", "full"),
    include_event      = c("pseudo", "full")
  ), fn_name = "calculate_reference_data")

  # `metadata$countries` is always a tibble under the three-mode schema
  # contract; gate on the key column instead of null-ness. Under "no" the
  # tibble is 0×0 so `country_key` is absent.
  if(!("country_key" %in% names(x$metadata$countries)))
    logger::log_info(
      "Data is missing country metadata; the resulting dataset cannot be used to create reference reports.",
      namespace = "neoipcr")

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
  # Three-valued gates all need to be non-"no" for the department
  # pipeline: department grouping, per-patient / per-enrollment / per-
  # event denominators.
  assert_options_for(x, required = list(
    include_department = c("pseudo", "full"),
    include_patient    = c("pseudo", "full"),
    include_enrollment = c("pseudo", "full"),
    include_event      = c("pseudo", "full")
  ), fn_name = "calculate_department_data")

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
      ~ dplyr::recode_values(
        .x,
        "pro_cat"~col_names[["pro_cat"]],
        "n"~col_names[["n"]],
        "pooled"~col_names[["pooled"]],
        "q1"~col_names[["q1"]],
        "q2"~col_names[["q2"]],
        "q3"~col_names[["q3"]],
        default = .x))
}

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

get_countries_with_wb_class <- function(x) {
  # `metadata$countries` is always a tibble under the three-mode schema
  # contract. Gate on `displayName` presence — it only appears under
  # `include_country == "full"`. The helper's name-based output only
  # makes sense under "full" where labels exist.
  if ("displayName" %in% names(x$metadata$countries)) {
    countries_data <- x$enrollments |>
      dplyr::inner_join(
        x$metadata$countries |>
          dplyr::select("country_key", "displayName"),
        dplyr::join_by("country_key")) |>
        dplyr::select(name = "displayName", tidyselect::any_of("world_bank_class_key")) |>
        dplyr::distinct()

    # Join with worldBankClasses to get stable `class` label. Gate on
    # column presence rather than null-ness: under the three-mode schema
    # contract `worldBankClasses` is always a tibble, but `class` only
    # appears under `include_world_bank_class == "full"`.
    if ("class" %in% names(x$metadata$worldBankClasses) &&
        "world_bank_class_key" %in% names(countries_data)) {
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
