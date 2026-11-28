# The enrolments under the stay-leg names: a denominator joins enrolments
# onto patients excluding the shared names, where the patient record's own
# hierarchy would otherwise win. Renaming first lets the stay's chain
# survive that exclusion and gives the grouping column the same name,
# `enrollment_department_key`, that the events table carries.
.enrollment_hierarchy_cols <- function(enrollments) {
  # `recycle0` keeps the renaming empty when the selection is: `paste0`
  # without it returns one name for zero columns, which `rename_with`
  # rejects.
  dplyr::rename_with(
    enrollments, ~ paste0("enrollment_", .x, recycle0 = TRUE),
    tidyselect::any_of(.hierarchy_key_names))
}

get_risk_population <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "risk_population"
  else
    cache_key <- paste0("risk_population_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  enrollments <- .enrollment_hierarchy_cols(x$enrollments)
  x$patients |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select(tidyselect::all_of(c("patient_key",setdiff(names(enrollments),names(x$patients))))),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      n_enrollments = dplyr::n(),
      n_patients = dplyr::n_distinct(.data$patient_key),
      .groups = "drop") |>
    cache(x, cache_key)
}

# The surgery form's three code columns, an absent one standing in as a
# missing value in every row: a dataset taken from a form version without
# the side codes counts like one whose procedures hold no side code.
.surgery_code_columns <- function(surgeryData) {
  for (col in c("main_procedure_code", "side_procedure_code_1",
                "side_procedure_code_2"))
    if (!(col %in% names(surgeryData)))
      surgeryData <- dplyr::mutate(surgeryData, !!col := NA_character_)
  surgeryData
}

get_surgery_risk <- function(x, group_cols = NULL, use_cache = TRUE) {
  v <- paste0(".v", .surgical_definitions_version())
  if(is.null(group_cols))
    cache_key <- paste0("surgery_risk", v)
  else
    cache_key <- paste0("surgery_risk_by.", paste0(group_cols, collapse = "."), v)

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
      return(r)

  surgeryData <- .surgery_code_columns(x$surgeryData)

  enrollments <- .enrollment_hierarchy_cols(x$enrollments)
  x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select("enrollment_key", "event_key") |>
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
                # The count holds a procedure only where the classifier
                # places it among the groups: its verdict over the main and
                # the side codes together, which holds out a pending code
                # that holds the whole procedure and lets in a procedure
                # whose side code places.
                dplyr::filter(
                  as.character(get_procedure_group(
                    .data$main_procedure_code,
                    .data$side_procedure_code_1,
                    .data$side_procedure_code_2)) %in%
                    .procedure_group_levels),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(tidyselect::any_of("enrollment_department_key"), ~ dplyr::n_distinct(.x), .names = "n_departments"),
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

  enrollments <- .enrollment_hierarchy_cols(x$enrollments)
  x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select("enrollment_key", "event_key") |>
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
  v <- paste0(".v", .surgical_definitions_version())
  if(is.null(group_cols))
    cache_key <- paste0("procedures", v)
  else
    cache_key <- paste0("procedures_by.", paste0(group_cols, collapse = "."), v)

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  surgeryData <- .surgery_code_columns(x$surgeryData)
  # The classifier's verdict over the main and the side codes together, as
  # in get_surgery_risk(), decides both whether a procedure counts and in
  # whose row: a procedure a side code places is that group's procedure,
  # and one a held pending code keeps unknown stays out of both paths.
  surgeryData <- dplyr::mutate(
    surgeryData,
    pro_cat = get_procedure_group(
      .data$main_procedure_code, .data$side_procedure_code_1,
      .data$side_procedure_code_2))
  surgeryData <- dplyr::filter(
    surgeryData, as.character(.data$pro_cat) %in% .procedure_group_levels)

  x$events |>
    dplyr::inner_join(surgeryData, dplyr::join_by("event_key")) |>
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
      dplyr::mutate(
        event_key = .data$event_key,
        code = .data$substance_code,
        display_name = as.character(.data$displayName),
        days = .data$days,
        .keep = "none"),

    atc5 = {
      atc5_names <- x$metadata$atc5Categories |>
        dplyr::mutate(
          atc5_code = as.character(.data$code),
          atc5_name = as.character(.data$displayName),
          .keep = "none")

      base |>
        dplyr::mutate(atc5_code = as.character(.data$ATC5)) |>
        dplyr::left_join(atc5_names, dplyr::join_by("atc5_code")) |>
        dplyr::group_by(.data$event_key, .data$atc5_code, .data$atc5_name) |>
        dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
        dplyr::mutate(
          event_key = .data$event_key,
          code = .data$atc5_code,
          display_name = .data$atc5_name,
          days = .data$days,
          .keep = "none")
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
      dplyr::mutate(
        event_key = .data$event_key,
        code = .data$AWaRe,
        display_name = NA_character_,
        days = .data$days,
        .keep = "none")
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
        dplyr::mutate(
          atc5_code = as.character(.data$code),
          atc5_name = as.character(.data$displayName),
          .keep = "none")

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
