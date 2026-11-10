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
        dev = dplyr::recode_values(
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

# Cumulative incidence of infection among the admissions of calendar windows.
#
# `windows` is caller data — one row per (department_key, window) with an
# inclusive [start, end] date range — so the result is not cached: the cache
# keys are option-derived strings and cannot describe an arbitrary window set.
#
# Cohort: an enrollment (admission) belongs to a window when its department
# matches and `enrolledAt` falls inside the range. Infected: the enrollment has
# at least one event of an `event_types` type whose `occurredAt` falls inside
# the same window and not before the admission. Counting through the
# enrollment ties every infection to the admission — and so to the department
# — it belongs to, so an infection outside the window, before its admission
# (a validation error, reachable only under `include_invalid_patients = TRUE`),
# or on another department's admission of the same patient, never counts.
# Windows may overlap; an admission is counted in each window it falls into,
# which is why both joins are declared many-to-many.
#
# Returns the four window columns with `n_patients`, `n_enrollments`,
# `n_infected_patients` and `n_infected_enrollments` appended, as an
# ungrouped tibble; a window with no admissions keeps its row with zeros
# rather than dropping out.
get_cumulative_incidence <- function(x, windows, event_types)
{
  # Number the windows on the ungrouped rows: a grouped `windows` (the
  # `group_by() |> summarise()` idiom leaves one) would restart the numbering
  # per group and pool the cohorts of different departments. Dropping the
  # caller's other columns keeps them from colliding with the counts.
  windows <- windows |>
    dplyr::ungroup() |>
    tibble::as_tibble() |>
    dplyr::select("department_key", "window", "start", "end") |>
    dplyr::mutate(.window = seq_len(dplyr::n()))

  cohort <- x$enrollments |>
    dplyr::select(
      "enrollment_key", "patient_key", "department_key", "enrolledAt") |>
    dplyr::semi_join(x$patients, dplyr::join_by("patient_key")) |>
    dplyr::inner_join(
      windows |>
        dplyr::select(".window", "department_key", "start", "end"),
      dplyr::join_by("department_key"),
      relationship = "many-to-many") |>
    dplyr::filter(
      .data$enrolledAt >= .data$start,
      .data$enrolledAt <= .data$end)

  infected <- x$events |>
    dplyr::filter(.data$event_type_key %in% event_types) |>
    dplyr::select("enrollment_key", "occurredAt") |>
    dplyr::inner_join(
      cohort |>
        dplyr::select(
          ".window", "enrollment_key", "patient_key", "enrolledAt",
          "start", "end"),
      dplyr::join_by("enrollment_key"),
      relationship = "many-to-many") |>
    dplyr::filter(
      .data$occurredAt >= .data$start,
      .data$occurredAt <= .data$end,
      .data$occurredAt >= .data$enrolledAt) |>
    dplyr::select(".window", "enrollment_key", "patient_key") |>
    dplyr::distinct()

  cohort_counts <- cohort |>
    dplyr::group_by(.data$.window) |>
    dplyr::summarise(
      n_patients = dplyr::n_distinct(.data$patient_key),
      n_enrollments = dplyr::n_distinct(.data$enrollment_key),
      .groups = "drop")

  infected_counts <- infected |>
    dplyr::group_by(.data$.window) |>
    dplyr::summarise(
      n_infected_patients = dplyr::n_distinct(.data$patient_key),
      n_infected_enrollments = dplyr::n_distinct(.data$enrollment_key),
      .groups = "drop")

  windows |>
    dplyr::left_join(cohort_counts, dplyr::join_by(".window")) |>
    dplyr::left_join(infected_counts, dplyr::join_by(".window")) |>
    dplyr::mutate(
      dplyr::across(
        c("n_patients", "n_enrollments",
          "n_infected_patients", "n_infected_enrollments"),
        \(v) tidyr::replace_na(v, 0L))) |>
    dplyr::select(!".window")
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
    dplyr::select(tidyselect::all_of(c(group_cols, "n", "inf_with_pathogen")), rate = "n_per_iwp") |>
    dplyr::mutate(
      drop_quartiles = tidyr::replace_na(
        n_deps < 5 | round(100 / .data$rate) >= median_inf_with_pathogen,
        TRUE))

  if(nrow(r1) < 1)
  {
    gc <- stats::setNames(
      as.list(rep(NA_character_, length(group_cols))),
      group_cols)
    return(
      tibble::tibble(
        n = 0L,
        inf_with_pathogen = 0L,
        rate = NA_real_,
        drop_quartiles = TRUE,
        q1 = NA_real_,
        q2 = NA_real_,
        q3 = NA_real_,
        q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
        q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
        q3_ci_lower = NA_real_, q3_ci_upper = NA_real_
        ) |>
        dplyr::bind_cols(gc)
      )
  }

  dept_data <- x |>
    get_infectious_agent_detection_rates(
      group_cols = c("department_key", group_cols),
      use_cache = use_cache)
  r2 <- dept_data |>
    dplyr::select(tidyselect::all_of(c("department_key", group_cols, "n_per_iwp")))

  if (nrow(r2) < 1) {
    return(
      r1 |>
        dplyr::mutate(
          q1 = NA_real_, q2 = NA_real_, q3 = NA_real_,
          q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
          q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
          q3_ci_lower = NA_real_, q3_ci_upper = NA_real_) |>
        cache(x, cache_key))
  }

  if (!is.null(group_cols))
  {
    r2 <- r2 |>
    tidyr::pivot_wider(
      names_from = tidyselect::all_of(group_cols),
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
    boot_cis <- purrr::map(seq_len(nrow(r)), function(i) {
      if (r$drop_quartiles[i]) return(na_boot)
      d <- dept_data
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        d <- d |> dplyr::semi_join(r[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$inf_with_pathogen > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$n, d$inf_with_pathogen,
                            type = "poisson", multiplier = 100)
    }) |> purrr::list_rbind()
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
  else if(length(inf_groups) == 0L)
    r <- r |>
    dplyr::cross_join(inf_counts) |>
    dplyr::mutate(n = tidyr::replace_na(.data$n, 0))
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
      with_pathogen = .data$event_key %in%
        .findings_naming_infectious_agent(x$infectiousAgentFindings)$event_key)

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

# The microbiological testing of the infections per infection type and its
# yield: for primary sepsis/BSI, pneumonia, and SSI (`event_type_key`), by
# `group_cols` (columns of `x$events`), every infection of the type
# (`total_inf`), the infections whose site was tested (`tested_inf`), those
# tested positive with an infectious agent in an own slot (`positive_inf`),
# the share tested (`tested_per_t`) and the share positive among the tested
# (`positive_per_tested`), both in percent; see
# `.infectious_agent_recording()` for what counts as tested and positive. A
# sepsis/BSI form does not record whether a blood culture was taken, so every
# episode is in its positive share's denominator (`tested_inf`) and it has no
# share tested (`NA`). NEC has no row: it has no test of its own, its
# infectious agents being recorded only for a secondary BSI. Without
# `group_cols` every type has a row, with none of its infections at 0; with
# them, a group has a row for each type it has infections of.
get_infectious_agent_detection_rates_per_inf_type <- function(
    x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "infectious_agent_detection_rates_per_inf_type"
  else
    cache_key <- paste0("infectious_agent_detection_rates_per_inf_type_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_events <- c("bsi","hap","ssi")

  r <- x$events |>
    dplyr::filter(.data$event_type_key %in% inf_events) |>
    dplyr::select(
      "event_key", "event_type_key", tidyselect::all_of(group_cols)) |>
    dplyr::inner_join(.infectious_agent_recording(x), dplyr::join_by("event_key")) |>
    dplyr::mutate(
      event_type_key = factor(
        as.character(.data$event_type_key),
        levels = inf_events)) |>
    dplyr::summarise(
      total_inf = dplyr::n(),
      tested_inf = sum(dplyr::coalesce(.data$tested, TRUE)),
      positive_inf = sum(.data$positive),
      .by = tidyselect::all_of(c(group_cols, "event_type_key")))

  if(is.null(group_cols))
    r <- tibble::tibble(event_type_key = factor(inf_events, levels = inf_events)) |>
    dplyr::left_join(r, dplyr::join_by("event_type_key")) |>
    dplyr::mutate(
      dplyr::across(
        c("total_inf", "tested_inf", "positive_inf"),
        \(v) dplyr::coalesce(v, 0L)))

  r |>
    dplyr::mutate(
      tested_per_t = dplyr::if_else(
        .data$event_type_key == "bsi",
        NA_real_,
        .data$tested_inf / .data$total_inf * 100),
      positive_per_tested = .data$positive_inf / .data$tested_inf * 100) |>
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
      name=dplyr::recode_values(
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
      drop_quartiles = tidyr::replace_na(
        .data$n_deps < 5 | round(100 / .data$rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map(seq_len(nrow(rate)), function(i) {
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
    }) |> purrr::list_rbind()
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

  infectiousAgentFindings <- .findings_naming_infectious_agent(
    x$infectiousAgentFindings)
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
          dplyr::recode_values(
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
      names_from = tidyselect::all_of(resistance),
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
        name=dplyr::recode_values(
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
      drop_quartiles = tidyr::replace_na(
        .data$n_deps < 5 | round(100 / .data$inf_rs_rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map(seq_len(nrow(rate)), function(i) {
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
    }) |> purrr::list_rbind()
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
        name=dplyr::recode_values(
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
      drop_quartiles = tidyr::replace_na(
        .data$n_deps < 5 | round(100 / .data$ia_rs_rate) >= .data$median,
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
  if (nrow(rate) == 0) {
    boot_cis <- na_boot[0, ]
  } else {
    boot_cis <- purrr::map(seq_len(nrow(rate)), function(i) {
      if (rate$drop_quartiles[i]) return(na_boot)
      d <- dplyr::ungroup(deps)
      if (!is.null(group_cols) && all(group_cols %in% names(d))) {
        d <- d |> dplyr::semi_join(rate[i, , drop = FALSE], by = group_cols)
      }
      d <- d |> dplyr::filter(.data$ia_tst_tot > 0)
      if (nrow(d) < 2) return(na_boot)
      bootstrap_quantile_ci(d$ia_rs, d$ia_tst_tot,
                            type = "binomial", multiplier = 100)
    }) |> purrr::list_rbind()
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
      names_from = tidyselect::all_of(resistance),
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

  sec_bsi_events <- c("nec","hap","ssi")

  # A secondary BSI counts where the follow-up is answered Yes and a
  # secondary-BSI slot names an infectious agent, so that every infection
  # the numerator counts is in the denominator, whatever a validation
  # exception keeps in the slots under another answer.
  named <- .findings_naming_infectious_agent(x$infectiousAgentFindings)
  named <- named$event_key[named$secondary_bsi %in% TRUE]

  r <- x$events |>
    dplyr::filter(.data$event_type_key %in% sec_bsi_events) |>
    dplyr::select(
      "event_key", "event_type_key", tidyselect::all_of(group_cols)) |>
    dplyr::left_join(.secondary_bsi_answers(x), dplyr::join_by("event_key")) |>
    dplyr::mutate(
      event_type_key = factor(
        as.character(.data$event_type_key),
        levels = sec_bsi_events),
      followed_up = .data$sec_bsi %in% c("1", "0"),
      with_sec_bsi = .data$sec_bsi %in% "1" & .data$event_key %in% named) |>
    dplyr::summarise(
      total_n = dplyr::n(),
      followup_n = sum(.data$followed_up),
      n = sum(.data$with_sec_bsi),
      .by = tidyselect::all_of(c(group_cols, "event_type_key")))

  if(is.null(group_cols))
    r <- tibble::tibble(
      event_type_key = factor(sec_bsi_events, levels = sec_bsi_events)) |>
    dplyr::left_join(r, dplyr::join_by("event_type_key")) |>
    dplyr::mutate(
      dplyr::across(
        c("total_n", "followup_n", "n"),
        \(v) dplyr::coalesce(v, 0L)))

  r |>
    dplyr::mutate(
      event_type_key = as.character(.data$event_type_key),
      rate = .data$n / .data$followup_n * 100,
      followup_rate = .data$followup_n / .data$total_n * 100) |>
    cache(x, cache_key)
}

# ---- Internal helpers -----------------------------------------------------

# The findings that name an infectious agent. "Not listed" (`Id` 0),
# "Unidentifiable" (`Id` 3561), and a code the infectious-agent list lacks
# name one; a slot that holds only a companion value (the source, the
# "recovered multiple times" item, a resistance category, or a free-text name,
# which the import moves to `unknownPathogenNames`) yields a finding without a
# `pathogen_key`, which names none and enters no figure.
.findings_naming_infectious_agent <- function(findings)
  dplyr::filter(findings, !is.na(.data$pathogen_key))

# The secondary-BSI item of the NEC, pneumonia, and SSI forms by event
# (`event_key`, `sec_bsi`), as the stored option code: "1" Yes, "0" No, "-1"
# No follow-up.
.secondary_bsi_answers <- function(x)
  dplyr::bind_rows(
    tibble::tibble(event_key = integer(), sec_bsi = character()),
    purrr::map(
      list(x$necData, x$pneumoniaData, x$ssiData),
      \(d) if ("sec_bsi" %in% names(d))
        tibble::tibble(
          event_key = d$event_key,
          sec_bsi = as.character(d$sec_bsi))))

# Whether the site of each primary sepsis/BSI, pneumonia, and SSI of `x` was
# tested microbiologically (`tested`), and whether its test was positive with
# an infectious agent named in an own slot, one with `secondary_bsi` FALSE
# (`positive`), by `event_key`. Each form item is read as its stored option
# code ("1" Yes, positive; "0" No, negative; "-1" Not tested); a blank item
# counts as not tested, and an event without a form row has every item blank.
# The pneumonia stage makes its test compulsory, and the program rules make the
# culture at the SSI's depth mandatory, so a blank occurs only on a form not
# completed or one stored without the client.
#
#   1. A sepsis/BSI's `tested` is `NA`: the form's single item "No positive
#      blood culture" (`no_pos_culture`) cannot tell a culture never taken
#      from a negative one. It is positive where an own slot names an
#      infectious agent and the form is not recorded culture-negative, the
#      form asking for the slots only then (NEOIPC_BSI_IF_NO_POS_CULTURE).
#   2. A pneumonia is tested where its `microbiological_test_result` is
#      positive or negative, and positive where the test is positive and an
#      own slot names an infectious agent, the form asking for the slots only
#      then (NEOIPC_HAP_MIBI_TEST_RESULT_VAL_NO_VAL_OR_0).
#   3. An SSI is tested where the culture at the depth its `infection_type`
#      records is positive or negative, and positive where that culture is
#      positive and an own slot names an infectious agent; an SSI without an
#      infection type is not tested. The form asks for the slots where the
#      culture of any depth is positive (NEOIPC_SSI_NO_MIBI_RESULT_AVAILABLE,
#      on the value NEOIPC_SSI_SET_AGENTS_IDENTIFIED_FROM_ANY_SITE assigns),
#      so an infectious agent there makes the SSI positive only with its own
#      depth's culture positive.
# The test item, not the slots alone, decides `positive`, since an
# infectious agent can stay in an own slot the form no longer asks for:
# Tracker Capture keeps the values of a section it hides, and only
# reconciliation 6 removes them, from a sepsis form recorded culture-negative
# that it repairs.
# A secondary-BSI slot makes no infection positive; the secondary-BSI rate
# counts it, and the follow-up for a secondary BSI decides neither column.
.infectious_agent_recording <- function(x) {
  named <- .findings_naming_infectious_agent(x$infectiousAgentFindings)
  own <- named$event_key[named$secondary_bsi %in% FALSE]

  form_items <- function(d, items)
    tibble::as_tibble(d)[c("event_key", intersect(items, names(d)))] |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))

  forms <- dplyr::bind_rows(
    tibble::tibble(
      event_key = integer(),
      no_pos_culture = logical(),
      microbiological_test_result = character(),
      infection_type = character(),
      organisms_superf = character(),
      organisms_deep = character(),
      organisms_organ = character()),
    form_items(x$sepsisData, "no_pos_culture"),
    form_items(x$pneumoniaData, "microbiological_test_result"),
    form_items(
      x$ssiData,
      c("infection_type", "organisms_superf", "organisms_deep",
        "organisms_organ")))

  x$events |>
    dplyr::filter(.data$event_type_key %in% c("bsi","hap","ssi")) |>
    dplyr::select("event_key", "event_type_key") |>
    dplyr::left_join(forms, dplyr::join_by("event_key")) |>
    dplyr::mutate(
      type = as.character(.data$event_type_key),
      test_result = dplyr::case_when(
        .data$type == "hap" ~ .data$microbiological_test_result,
        .data$type == "ssi" & .data$infection_type %in% "1" ~
          .data$organisms_superf,
        .data$type == "ssi" & .data$infection_type %in% "2" ~
          .data$organisms_deep,
        .data$type == "ssi" & .data$infection_type %in% "3" ~
          .data$organisms_organ),
      tested = dplyr::if_else(
        .data$type == "bsi",
        NA,
        .data$test_result %in% c("1", "0")),
      positive = .data$event_key %in% own & dplyr::if_else(
        .data$type == "bsi",
        !dplyr::coalesce(.data$no_pos_culture, FALSE),
        .data$test_result %in% "1")) |>
    dplyr::select("event_key", "tested", "positive")
}
