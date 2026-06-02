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
    dplyr::select(tidyselect::all_of(c(group_cols, "n", "inf_with_pathogen")), rate = "n_per_iwp") |>
    dplyr::mutate(
      drop_quartiles = n_deps < 5 | round(100 / .data$rate) >= median_inf_with_pathogen)

  if(nrow(r1) < 1)
  {
    gc <- setNames(
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
    dplyr::select(tidyselect::all_of(c("department_key", group_cols, "n_per_iwp")))

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
