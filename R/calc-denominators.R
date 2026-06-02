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

  surgeryData <- x$surgeryData
  if(!("main_procedure_code" %in% names(surgeryData)))
    surgeryData <- surgeryData |>
      dplyr::mutate(main_procedure_code = character())

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
