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

# The version of the layout of the infectious-agent tables' rows: their order,
# their levels, the residual and summary rows, and the keys and labels each
# row carries. It changes with any change to these, so that tables laid out
# otherwise are not compared.
infectious_agent_table_layout <- 1L

# The infection types of the infectious-agent tables, in the order of their
# rows and columns.
infectious_agent_infection_types <- c("bsi", "hap", "nec", "ssi")

# The counts behind the detection rates per infectious agent, over the
# reporting groups (`rows = "groups"`) or over the full ontology
# (`rows = "ontology"`): every infectious agent counted once per infection,
# in the slots its form asks for (see `.counted_infectious_agent_findings()`).
# Returns a list:
#   * `total`: by `department_key` and `event_type_key`, the infectious agents
#     (`n`) of the microbiologically confirmed infections (`total`), those
#     outside the tree included;
#   * `rows`: by `node_id`, `department_key`, and `event_type_key`, the
#     confirmed infections with an infectious agent at or beneath the row's
#     node (`n`), over those with an infectious agent in the tree (`total`);
#     every row has an entry for each department and type with such an
#     infection, at 0 where none of them is the row's;
#   * `layout`: the rows, in their order, as `.infectious_agent_layout()`
#     gives them;
#   * `n_total_only`: the infectious agents counted in the Total only, no row
#     counting them: "Not listed", "Unidentifiable", and the codes the list
#     lacks, each once per infection.
get_infectious_agent_detection_rates <- function(
    x, rows = c("groups", "ontology"), use_cache = TRUE) {
  rows <- rlang::arg_match(rows)
  cache_key <- paste0("infectious_agent_detection_rates_", rows)

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  agents <- x$events |>
    dplyr::filter(.data$event_type_key %in% infectious_agent_infection_types) |>
    # An infection counts for its stay's department; the internal column
    # keeps the plain name these tables and their joins carry.
    dplyr::select(
      "event_key", "event_type_key",
      department_key = "enrollment_department_key") |>
    dplyr::mutate(event_type_key = as.character(.data$event_type_key)) |>
    dplyr::inner_join(
      .counted_infectious_agent_findings(x) |>
        dplyr::distinct(
          dplyr::across(c("event_key", "concept_key")), .keep_all = TRUE),
      dplyr::join_by("event_key"))

  infections <- agents |>
    dplyr::summarise(
      n = dplyr::n(),
      n_total_only = sum(!.data$in_tree),
      in_tree = any(.data$in_tree),
      .by = c("event_key", "event_type_key", "department_key"))

  denominators <- infections |>
    dplyr::summarise(
      total = sum(.data$in_tree),
      .by = c("department_key", "event_type_key")) |>
    dplyr::filter(.data$total > 0L)

  counts <- agents |>
    dplyr::filter(.data$in_tree) |>
    .infectious_agent_memberships(rows) |>
    dplyr::distinct(
      dplyr::across(c("event_key", "node_id")), .keep_all = TRUE) |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = c("node_id", "department_key", "event_type_key"))
  counts <- denominators |>
    dplyr::cross_join(tibble::tibble(node_id = unique(counts$node_id))) |>
    dplyr::left_join(
      counts,
      dplyr::join_by("node_id", "department_key", "event_type_key")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) |>
    dplyr::select("node_id", "department_key", "event_type_key", "n", "total")

  pooled_total <- sum(denominators$total)
  layout <- counts |>
    dplyr::summarise(n = sum(.data$n), .by = "node_id") |>
    dplyr::mutate(rate = .data$n / pooled_total) |>
    .infectious_agent_layout(rows)

  list(
    total = infections |>
      dplyr::summarise(
        n = sum(.data$n),
        total = dplyr::n(),
        .by = c("department_key", "event_type_key")),
    rows = counts,
    layout = layout,
    n_total_only = sum(infections$n_total_only)) |>
    cache(x, cache_key)
}

# The infection counts, by `group_cols`, where each group column's name
# says which record answers for it. An `event_` hierarchy leg counts and
# frames on the recording side: its counts come from the events, and its
# units are the ones the events show, so a department that records other
# events but no infection counts at 0. A plain hierarchy key or an
# `enrollment_` leg counts on the side of the stay — the value rides on
# the event as its stay leg — and frames on the units the enrolments
# show, so a department with stays but no infection counts at 0. A
# column naming neither leg holds a value of the infection's own
# records: event columns ride on the events, patient and enrolment
# columns join through the event's foreign keys, and only the values the
# infections show get a row. The infection-type filter narrows the
# counts, never a frame; an `event_` leg is therefore a diagnostic
# grouping, since the denominators take only the stay side.
get_infection_counts <- function(x, group_cols = NULL, use_cache = TRUE) {
  if(is.null(group_cols))
    cache_key <- "infection_counts"
  else
    cache_key <- paste0("infection_counts_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_events <- c("bsi","nec","hap","ssi")
  hierarchy_legs <- paste0(
    rep(c("event_", "enrollment_"), each = length(.hierarchy_key_names)),
    rep(.hierarchy_key_names, 2))

  inf <- x$events |>
    dplyr::filter(.data$event_type_key %in% inf_events) |>
    dplyr::mutate(
      event_type_key = factor(
        as.character(.data$event_type_key),
        levels = inf_events
      ),
      with_pathogen = .data$event_key %in%
        .findings_naming_infectious_agent(x$infectiousAgentFindings)$event_key)

  # A plain key counts for the unit of the stay, whose value the event
  # carries under the stay-leg name.
  for(col in intersect(group_cols, .hierarchy_key_names))
    inf <- dplyr::mutate(inf, !!col := .data[[paste0("enrollment_", col)]])

  # A column naming neither leg names a value of one of the infection's
  # records; the left joins keep the infection in its count whatever the
  # joined record shows or whether it is there at all.
  if(!is.null(group_cols)) {
    no_leg <- setdiff(group_cols, c(.hierarchy_key_names, hierarchy_legs))
    bins <- c("bw50", "bw125", "bw250", "bw500", "comp_gw")
    pat_want <- intersect(no_leg, names(x$patients))
    bin_want <- intersect(no_leg, bins)
    if(length(pat_want) || length(bin_want)) {
      patients_group <- x$patients
      if(length(bin_want))
        patients_group <- patients_group |>
          dplyr::mutate(
            bw50 = bw50(.data$birth_weight),
            bw125 = bw125(.data$birth_weight),
            bw250 = bw250(.data$birth_weight),
            bw500 = bw500(.data$birth_weight),
            comp_gw = as.integer(.data$total_gestation_days / 7))
      inf <- dplyr::left_join(
        inf,
        dplyr::select(
          patients_group, "patient_key",
          tidyselect::all_of(setdiff(c(pat_want, bin_want), names(inf)))),
        dplyr::join_by("patient_key"))
    }
    enr_want <- setdiff(
      intersect(no_leg, names(x$enrollments)), names(inf))
    if(length(enr_want))
      inf <- dplyr::left_join(
        inf,
        dplyr::select(
          x$enrollments, "enrollment_key",
          tidyselect::all_of(enr_want)),
        dplyr::join_by("enrollment_key"))
  }

  counts <- inf |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  if(is.null(group_cols))
    return(counts |>
             cache(x, cache_key))

  # The frame holds every unit each side's records show, whether or not
  # it has infections: the recording units from the events, the stay
  # units from the enrolments under whichever name the group column
  # uses, and — for columns naming no side — the values the infections
  # show, a factor's declared levels among them even where no infection
  # shows one, both `with_pathogen` levels among them by the frame's own
  # right. A frame naming no column of its own is one row: a dataset
  # without infections must still count each department at zero, and a
  # frame of no values would multiply the other frames away.
  stay_keys <- intersect(.hierarchy_key_names,
                         sub("^enrollment_", "", group_cols))
  stay_plain <- intersect(group_cols, stay_keys)
  stay_legs <- intersect(group_cols, paste0("enrollment_", stay_keys))
  event_legs <- intersect(group_cols, paste0("event_", .hierarchy_key_names))
  frames <- list(
    if(length(event_legs))
      dplyr::distinct(
        x$events |> dplyr::select(tidyselect::all_of(event_legs)))
    else
      tibble::tibble(.rows = 1L)
  )
  if(length(stay_plain) || length(stay_legs)) {
    stay_side <- x$enrollments |>
      dplyr::select(tidyselect::all_of(stay_keys))
    frames <- c(frames, list(dplyr::distinct(dplyr::bind_cols(
      dplyr::select(stay_side, tidyselect::all_of(stay_plain)),
      dplyr::select(.enrollment_hierarchy_cols(stay_side),
                    tidyselect::all_of(stay_legs))))))
  }
  inf_values <- dplyr::distinct(
    inf |> dplyr::select(tidyselect::all_of(setdiff(
      group_cols,
      c(paste0("event_", .hierarchy_key_names), stay_keys,
        paste0("enrollment_", .hierarchy_key_names))))))
  if(!nrow(inf_values))
    inf_values <- if(ncol(inf_values))
      tidyr::expand_grid(!!!lapply(inf_values, function(col)
        if(is.factor(col)) factor(levels(col), levels = levels(col))
        else unique(col)))
    else
      tibble::tibble(.rows = 1L)
  if("with_pathogen" %in% group_cols)
    inf_values <- dplyr::bind_rows(
      inf_values, tibble::tibble(with_pathogen = c(TRUE, FALSE)))
  frames <- c(frames, list(inf_values))

  Reduce(dplyr::cross_join, frames) |>
    tidyr::expand(!!! dplyr::syms(group_cols)) |>
    tidyr::drop_na() |>
    dplyr::left_join(counts, by = group_cols) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0)) |>
    cache(x, cache_key)
}

# The infection types of the table by infection type, in the order of its
# rows. NEC has none: it has no test of its own, its infectious agents being
# recorded only for a secondary BSI.
testing_infection_types <- c("bsi", "hap", "ssi")

# The counts behind the table by infection type, by `department_key` and
# `event_type_key` (see `testing_infection_types`): every infection of the
# type (`total`), the infections whose site was tested (`tested`), and those
# tested positive with an infectious agent in an own slot (`positive`); see
# `.infectious_agent_recording()` for what counts as tested and positive. A
# sepsis/BSI form does not record whether a blood culture was taken, so every
# episode counts as tested, the denominator of its positive share. A
# department has an entry for each type it has infections of.
get_infectious_agent_detection_rates_per_inf_type <- function(
    x, use_cache = TRUE) {
  cache_key <- "infectious_agent_testing_counts"
  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$events |>
    dplyr::filter(.data$event_type_key %in% testing_infection_types) |>
    dplyr::select(
      "event_key", "event_type_key",
      department_key = "enrollment_department_key") |>
    dplyr::inner_join(.infectious_agent_recording(x), dplyr::join_by("event_key")) |>
    dplyr::mutate(event_type_key = as.character(.data$event_type_key)) |>
    dplyr::summarise(
      total = dplyr::n(),
      tested = sum(dplyr::coalesce(.data$tested, TRUE)),
      positive = sum(.data$positive),
      .by = c("department_key", "event_type_key")) |>
    cache(x, cache_key)
}

# The resistance categories of the infectious-agent slots, in the order of the
# resistance tables' blocks: third-generation cephalosporin resistance (3GCR),
# carbapenem resistance, colistin resistance, methicillin-resistant
# Staphylococcus aureus (MRSA), and vancomycin-resistant Enterococcus (VRE).
infectious_agent_resistance_categories <- c("3gcr", "car", "cor", "mrsa", "vre")

# The rows of the completeness of antimicrobial susceptibility testing, in
# their order: each resistance category (`abr`) with its plain row
# (`cond = "routine"`), and the conditional rows of carbapenem resistance in
# case of 3GCR (`"if_3gcr"`) and of colistin resistance in case of 3GCR and
# carbapenem resistance (`"if_3gcr&car"`).
resistance_test_rows <- tibble::tibble(
  abr = c("3gcr", "car", "car", "cor", "cor", "mrsa", "vre"),
  cond = c("routine", "routine", "if_3gcr", "routine", "if_3gcr&car",
           "routine", "routine"))

# The detections of `x`: each infectious agent the infectious-agent tables
# count (see `.counted_infectious_agent_findings()`), once per infection, by
# `event_key` and `concept_key`, with the infection's `department_key`;
# `in_tree`, `gram_reaction`, and `oxygen_requirement` as
# `classify_infectious_agent_findings()` gives them; and for each resistance
# category `<c>`: `applies_<c>`, whether the list applies the category to the
# infectious agent; `tested_<c>`, whether a slot naming it in the infection
# holds a result, recorded resistant ("yes") or not ("no"), where "Not tested"
# and a blank hold none; and `resistant_<c>`, whether a slot records it
# resistant. Two slots of one infection that name one concept, a primary and a
# secondary-BSI slot or a synonym and its concept, are one detection, tested
# where either has a result and resistant where either records resistance.
get_infectious_agent_detections <- function(x, use_cache = TRUE) {
  cache_key <- "infectious_agent_detections"
  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  categories <- infectious_agent_resistance_categories
  slots <- .counted_infectious_agent_findings(x) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(categories),
        \(value) as.character(value) %in% c("yes", "no"),
        .names = "tested_{.col}"),
      dplyr::across(
        tidyselect::all_of(categories),
        \(value) as.character(value) %in% "yes",
        .names = "resistant_{.col}"))

  x$events |>
    dplyr::filter(.data$event_type_key %in% infectious_agent_infection_types) |>
    dplyr::select("event_key", department_key = "enrollment_department_key") |>
    dplyr::inner_join(slots, dplyr::join_by("event_key")) |>
    dplyr::summarise(
      in_tree = dplyr::first(.data$in_tree),
      gram_reaction = dplyr::first(.data$gram_reaction),
      oxygen_requirement = dplyr::first(.data$oxygen_requirement),
      dplyr::across(
        tidyselect::all_of(paste0("applies_", categories)), dplyr::first),
      dplyr::across(
        tidyselect::all_of(c(paste0("tested_", categories),
                             paste0("resistant_", categories))),
        any),
      .by = c("event_key", "department_key", "concept_key")) |>
    cache(x, cache_key)
}

# The counts behind the two resistance tables whose rows are infectious
# agents, over the reporting groups (`rows = "groups"`) or over the full
# ontology (`rows = "ontology"`), for each resistance category (`abr`).
# Returns a list:
#   * `rows`: by `abr`, `node_id`, and `department_key`, the infections with
#     a detection (see `get_infectious_agent_detections()`) at or beneath the
#     row's node that is recorded resistant in the category (`resistant`), that
#     has a result for it (`tested`), to which the list applies it
#     (`applicable`), and to which the list applies it and that has a result
#     (`applicable_tested`), each infection once per row. A category's rows
#     are those the block can have (`.infectious_agent_block_nodes()`) that
#     lie at or above a detection in the tree to which the list applies the
#     category or that has a result for it, residual and summary rows
#     included (see `.infectious_agent_memberships()`), so that whether a
#     dataset has a row depends only on its detections at or beneath it; a
#     detection that is neither adds to no count. `resistant` and `tested`
#     count a detection to which the category does not apply as any other,
#     since no filter of applicability drops a value recorded for it. A
#     department has an entry for each row with such a detection, and
#     `untested` where it has detections in the row to which the list applies
#     the category and none of them has a result, the row's Tested share then
#     being 0, which takes the department out of the row's figures in the
#     density of infections with resistant infectious agents;
#   * `total`: by `abr` and `department_key`, the infections with any
#     detection recorded resistant in the category (`resistant`), those
#     outside the tree included, and those of them that no row of the block
#     counts in that density (`n_total_only`): an infection whose resistant
#     detections in the category all lie outside the tree ("Not listed",
#     "Unidentifiable", and the codes the list lacks), beneath no row of the
#     block, such as a fungus recorded resistant in a category the list
#     applies to bacteria alone, or only in rows its department is untested
#     in.
get_infectious_agent_resistance_rates <- function(
    x, rows = c("groups", "ontology"), use_cache = TRUE) {
  rows <- rlang::arg_match(rows)
  cache_key <- paste0("infectious_agent_resistance_rates_", rows)

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  detections <- get_infectious_agent_detections(x, use_cache = use_cache)
  in_tree <- detections[detections$in_tree, ]

  block <- function(category) {
    applies <- paste0("applies_", category)
    tested <- paste0("tested_", category)
    resistant <- paste0("resistant_", category)
    members <- in_tree[in_tree[[applies]] | in_tree[[tested]], ] |>
      .infectious_agent_memberships(
        rows, .infectious_agent_block_nodes(category, rows)) |>
      dplyr::summarise(
        resistant = any(.data[[resistant]]),
        tested = any(.data[[tested]]),
        applicable = any(.data[[applies]]),
        applicable_tested = any(.data[[applies]] & .data[[tested]]),
        .by = c("node_id", "event_key", "department_key"))
    counts <- members |>
      dplyr::summarise(
        resistant = sum(.data$resistant),
        tested = sum(.data$tested),
        applicable = sum(.data$applicable),
        applicable_tested = sum(.data$applicable_tested),
        .by = c("node_id", "department_key")) |>
      dplyr::mutate(
        untested = .data$applicable > 0L & .data$applicable_tested == 0L)
    in_a_row <- members |>
      dplyr::filter(.data$resistant) |>
      dplyr::semi_join(
        counts[!counts$untested, ],
        dplyr::join_by("node_id", "department_key")) |>
      dplyr::pull("event_key") |>
      unique()

    list(
      rows = dplyr::mutate(counts, abr = category, .before = 1),
      total = detections |>
        dplyr::summarise(
          resistant = any(.data[[resistant]]),
          .by = c("event_key", "department_key")) |>
        dplyr::mutate(
          total_only = .data$resistant & !.data$event_key %in% in_a_row) |>
        dplyr::summarise(
          resistant = sum(.data$resistant),
          n_total_only = sum(.data$total_only),
          .by = "department_key") |>
        dplyr::mutate(abr = category, .before = 1))
  }

  blocks <- purrr::map(infectious_agent_resistance_categories, block)
  list(
    rows = purrr::list_rbind(purrr::map(blocks, "rows")),
    total = purrr::list_rbind(purrr::map(blocks, "total"))) |>
    cache(x, cache_key)
}

# The counts behind the completeness of antimicrobial susceptibility testing,
# by row (`abr` and `cond`, see `resistance_test_rows`) and `department_key`:
# the detections (see `get_infectious_agent_detections()`) a row holds
# (`total`), and those among them with a result for its category (`n`). A
# category's plain row holds every detection to which the list applies it,
# "Not listed" included and "Unidentifiable" and a code the list lacks
# excluded, whatever its slots hold; a conditional row only those of them
# recorded resistant to 3GCR, and for colistin also to carbapenems, to which
# the list applies these categories too, so that a value recorded for a
# category that cannot apply counts in no row. A department has an entry for
# each row that holds one of its detections.
get_resistance_test_counts <- function(x, use_cache = TRUE) {
  cache_key <- "resistance_test_counts"
  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  d <- get_infectious_agent_detections(x, use_cache = use_cache)
  holds <- function(abr, cond) {
    applicable <- d[[paste0("applies_", abr)]]
    resistant_3gcr <- d$applies_3gcr & d$resistant_3gcr
    switch(
      cond,
      routine = applicable,
      if_3gcr = applicable & resistant_3gcr,
      `if_3gcr&car` = applicable & resistant_3gcr &
        d$applies_car & d$resistant_car)
  }

  purrr::pmap(resistance_test_rows, \(abr, cond) {
    held <- holds(abr, cond)
    tested <- d[[paste0("tested_", abr)]]
    tibble::tibble(
      abr = abr,
      cond = cond,
      department_key = d$department_key[held],
      tested = tested[held]) |>
      dplyr::summarise(
        n = sum(.data$tested),
        total = dplyr::n(),
        .by = c("abr", "cond", "department_key"))
  }) |>
    purrr::list_rbind() |>
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

# How each infection of `x` records its infectious agents, by `event_key`,
# from one reading of its form: whether the form asks for its own slots
# (`primary_counts`) and for its secondary-BSI slots (`secondary_counts`),
# which every infectious-agent table counts by; and, for the table by
# infection type, whether the site of a primary sepsis/BSI, pneumonia, or SSI
# was tested microbiologically (`tested`) and whether its test was positive
# with an infectious agent named in an own slot, one with `secondary_bsi`
# FALSE (`positive`). Each form item is read as its stored option code ("1"
# Yes, positive; "0" No, negative; "-1" Not tested or No follow-up); a blank
# item counts as not tested, and an event without a form row has every item
# blank. The pneumonia stage makes its test compulsory, and the program rules
# make the culture at the SSI's depth mandatory, so a blank occurs only on a
# form not completed or one stored without the client.
#
# The slots the form asks for, cases 1 to 4, those the program rules of the
# NEOIPC_CORE program show, whatever else the form records:
#   1. a sepsis form's slots unless it is recorded culture-negative
#      (NEOIPC_BSI_IF_NO_POS_CULTURE), a blank item counting as not;
#   2. a pneumonia's own slots where its test is positive
#      (NEOIPC_HAP_MIBI_TEST_RESULT_VAL_NO_VAL_OR_0);
#   3. an SSI's own slots where the culture of any depth is positive
#      (NEOIPC_SSI_NO_MIBI_RESULT_AVAILABLE, on the value
#      NEOIPC_SSI_SET_AGENTS_IDENTIFIED_FROM_ANY_SITE assigns), whichever
#      depth the infection type records, and whether or not the SSI was
#      followed up for a secondary BSI;
#   4. the secondary-BSI slots of a NEC, pneumonia, or SSI where the
#      follow-up is answered Yes, the answer that records a secondary BSI
#      (NEOIPC_NEC_SEC_BSI_VAL_NO_VAL_OR_0, NEOIPC_HAP_SEC_BSI_VAL_NO_VAL_OR_0,
#      NEOIPC_SSI_NO_SEC_BSI).
# Tracker Capture hides a section the rules hide whatever its fields hold,
# keeping their values. A field the rules hide it blanks, with a notice, on a
# form the user can edit, whatever section holds the field, and saves the
# blank without running the rules again; on a form it does not let the user
# edit, such as a completed one, it keeps the value and shows the field,
# unless a hidden section holds it. The Capture app assigns a field the rules
# hide the empty value. The rules hide a sepsis form's slots (case 1) as a
# section alone, so a sepsis form recorded culture-negative keeps its
# infectious agents. They hide a pneumonia's own slots (case 2) as a section,
# and by the same rule the field of its slot 1's infectious agent,
# NEOIPC_HAP_PATHOGEN_1; they hide slots 2 and 3 field by field once slot 1
# is blank (NEOIPC_HAP_AGENT_1_IF_EMPTY), which Tracker Capture therefore
# blanks only when it next runs the rules on the form. They hide an SSI's own
# slots (case 3) and its secondary-BSI slots both as sections and field by
# field, and a pneumonia's or NEC's secondary-BSI slots field by field. An
# infectious agent in a pneumonia's or NEC's secondary-BSI slot under an
# answer other than Yes was therefore stored where no client blanked it, as
# by an import; it contradicts the answer, and validation rule 55 reports it
# as an error, so only an exception or `include_invalid_patients = TRUE`
# brings it here. An infectious agent in a slot the form does not ask for,
# which reconciliations 5 and 6 and validation rule 55 remove only in part,
# counts for nothing, so that every infection a numerator counts is in its
# denominator.
#
# Tested and positive:
#   1. A sepsis/BSI's `tested` is `NA`: the form's single item "No positive
#      blood culture" (`no_pos_culture`) cannot tell a culture never taken
#      from a negative one. It is positive where an own slot the form asks for
#      (case 1) names an infectious agent.
#   2. A pneumonia is tested where its `microbiological_test_result` is
#      positive or negative, and positive where an own slot the form asks for
#      (case 2, the test positive) names an infectious agent.
#   3. An SSI is tested where the culture at the depth its `infection_type`
#      records is positive or negative, and positive where that culture is
#      positive and an own slot names an infectious agent; an SSI without an
#      infection type is not tested. The form asks for the own slots where
#      the culture of any depth is positive (case 3), so an infectious agent
#      there makes the SSI positive only with its own depth's culture
#      positive.
# The test item, not the slots alone, decides `positive`, since an
# infectious agent can stay in an own slot the form no longer asks for: in a
# sepsis form's slots, in a pneumonia's slots 2 and 3 until Tracker Capture
# next runs the rules on the form, or in a field no client blanked; only
# reconciliation 6 removes such infectious agents, from a sepsis form recorded
# culture-negative that it repairs. A secondary-BSI slot makes no infection
# positive; the secondary-BSI rate counts it, and the follow-up for a
# secondary BSI decides neither `tested` nor `positive`.
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
      sec_bsi = character(),
      microbiological_test_result = character(),
      infection_type = character(),
      organisms_superf = character(),
      organisms_deep = character(),
      organisms_organ = character()),
    form_items(x$sepsisData, "no_pos_culture"),
    form_items(x$necData, "sec_bsi"),
    form_items(x$pneumoniaData, c("sec_bsi", "microbiological_test_result")),
    form_items(
      x$ssiData,
      c("sec_bsi", "infection_type", "organisms_superf", "organisms_deep",
        "organisms_organ")))

  x$events |>
    dplyr::filter(.data$event_type_key %in% c("bsi","nec","hap","ssi")) |>
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
      primary_counts = dplyr::case_when(
        .data$type == "bsi" ~ !dplyr::coalesce(.data$no_pos_culture, FALSE),
        .data$type == "hap" ~ .data$microbiological_test_result %in% "1",
        .data$type == "ssi" ~
          .data$organisms_superf %in% "1" |
          .data$organisms_deep %in% "1" |
          .data$organisms_organ %in% "1",
        .default = FALSE),
      secondary_counts = .data$type != "bsi" & .data$sec_bsi %in% "1",
      tested = dplyr::if_else(
        .data$type == "bsi",
        NA,
        .data$test_result %in% c("1", "0")),
      positive = .data$primary_counts & .data$event_key %in% own &
        (.data$type != "ssi" | .data$test_result %in% "1")) |>
    dplyr::select(
      "event_key", "primary_counts", "secondary_counts", "tested", "positive")
}

# The findings of `x` that the infectious-agent tables count: those that name
# an infectious agent in a slot the form asks for (see
# `.infectious_agent_recording()`), classified by
# `classify_infectious_agent_findings()`, one row per slot. Two slots of one
# infection may name one infectious agent; the tables count it once per
# infection, by its `concept_key`.
.counted_infectious_agent_findings <- function(x)
  classify_infectious_agent_findings(x$infectiousAgentFindings) |>
  dplyr::inner_join(
    .infectious_agent_recording(x) |>
      dplyr::select("event_key", "primary_counts", "secondary_counts"),
    dplyr::join_by("event_key")) |>
  dplyr::filter(
    (.data$secondary_bsi %in% FALSE & .data$primary_counts) |
      (.data$secondary_bsi %in% TRUE & .data$secondary_counts)) |>
  dplyr::select(!c("primary_counts", "secondary_counts"))

# The findings that name an infectious agent (see
# `.findings_naming_infectious_agent()`), each with what the bundled
# infectious-agent list says of its code: `concept_key`, the code of the
# concept the code names, a synonym standing for its concept and a code the
# list lacks for itself; `in_tree`, whether the code has a place among the
# tables' rows, which "Not listed", "Unidentifiable", and a code the list lacks
# have not, so that they count in a table's Total only; `common_commensal` and
# the five `applies_*` flags, `FALSE` for a code the list lacks, which counts
# as a recognized pathogen and as an infectious agent to which no resistance
# category applies; and `gram_reaction` and `oxygen_requirement`, `NA` outside
# the bacteria. The findings' own columns stay as they are.
classify_infectious_agent_findings <- function(findings) {
  classification <- internal_infectious_agent_classification
  flags <- c("common_commensal", "applies_3gcr", "applies_car", "applies_cor",
             "applies_mrsa", "applies_vre")
  .findings_naming_infectious_agent(findings) |>
    dplyr::left_join(
      classification[c("input_id", "output_id", flags, "gram_reaction",
                       "oxygen_requirement")],
      dplyr::join_by("pathogen_key" == "input_id")) |>
    dplyr::mutate(
      concept_key = dplyr::coalesce(.data$output_id, .data$pathogen_key),
      in_tree = .data$pathogen_key %in% internal_infectious_agent_closure$input_id,
      dplyr::across(tidyselect::all_of(flags), \(v) dplyr::coalesce(v, FALSE))) |>
    dplyr::select(!"output_id")
}

# The suffix of the `node_id` of a residual row, which counts the infectious
# agents recorded at its parent's node where that node has nodes beneath it.
infectious_agent_residual_suffix <- "~n.o.s."

# The rows of the version `rows` in which each row of `agents` (classified
# findings in the tree, with `concept_key`, `gram_reaction`, and
# `oxygen_requirement`) counts, one row per agent and row, with the row's
# `node_id` beside the agents' columns, among the rows `nodes` (`node_id`s,
# every row of the version where `NULL`): every node on its concept's path in
# the bundled closure; over the full ontology also the residual row of the
# node it is recorded at, its deepest, where that node has a node beneath it
# among `nodes` in the bundled tree; and over the reporting groups also a
# summary row for the value of each attribute the agent has, keyed
# `<attribute>|<value>`. An agent's rows, its residual row included, thus
# depend on the bundle and on where the agent is recorded, not on the other
# agents, so that a department's dataset and a reference that holds it give
# an agent the same rows.
.infectious_agent_memberships <- function(agents, rows, nodes = NULL) {
  closure <- internal_infectious_agent_closure
  closure <- closure[closure$version == rows, c("input_id", "node_id", "depth")] |>
    dplyr::mutate(
      recorded = .data$depth == max(.data$depth),
      .by = "input_id")
  tree_nodes <- internal_infectious_agent_nodes
  tree_nodes <- tree_nodes[tree_nodes$version == rows, ]
  if (!is.null(nodes)) {
    closure <- closure[closure$node_id %in% nodes, ]
    tree_nodes <- tree_nodes[tree_nodes$node_id %in% nodes, ]
  }
  tree <- agents |>
    dplyr::inner_join(
      closure,
      dplyr::join_by("concept_key" == "input_id"),
      relationship = "many-to-many")

  if (rows == "ontology") {
    with_nodes_beneath <- unique(tree_nodes$parent_id)
    tree <- dplyr::bind_rows(
      tree,
      tree |>
        dplyr::filter(.data$recorded, .data$node_id %in% with_nodes_beneath) |>
        dplyr::mutate(
          node_id = paste0(.data$node_id, infectious_agent_residual_suffix)))
    return(dplyr::select(tree, !c("depth", "recorded")))
  }

  attributes <- unique(internal_infectious_agent_attribute_values$attribute)
  summary <- purrr::map(attributes, \(attribute)
    agents |>
      dplyr::filter(!is.na(.data[[attribute]])) |>
      dplyr::mutate(node_id = paste0(attribute, "|", .data[[attribute]]))) |>
    purrr::list_rbind()
  if (!is.null(nodes))
    summary <- summary[summary$node_id %in% nodes, ]
  dplyr::bind_rows(dplyr::select(tree, !c("depth", "recorded")), summary)
}

# The rows a block of the resistance category `category` can have in the
# version `rows`, as `node_id`s: the nodes on the bundled paths of the `Id`s
# to which the list applies the category, and over the reporting groups the
# summary rows of the attribute values those `Id`s have. They come from the
# bundle, not from a dataset, so that a node is a row of a department's block
# and of a reference's alike.
.infectious_agent_block_nodes <- function(category, rows) {
  classification <- internal_infectious_agent_classification
  applicable <- classification$input_id[
    classification[[paste0("applies_", category)]]]
  closure <- internal_infectious_agent_closure
  closure <- closure[closure$version == rows &
                       closure$input_id %in% applicable, ]
  nodes <- unique(closure$node_id)
  if (rows == "ontology")
    return(nodes)

  applicable <- classification[
    classification$input_id %in% closure$input_id, ]
  attributes <- unique(internal_infectious_agent_attribute_values$attribute)
  c(nodes,
    unlist(lapply(attributes, \(attribute) {
      values <- unique(applicable[[attribute]])
      paste0(attribute, "|", values[!is.na(values)], recycle0 = TRUE)
    })))
}

# The order and labels of the rows `rates` holds (`node_id`, and `rate`, by
# which siblings are ordered) in the version `rows`: over the reporting
# groups, first the summary block, a row per attribute value in the
# classification's order of the attributes and their values; then the tree
# in pre-order, siblings by `rate`, descending, ties by label and `node_id`,
# and a residual row last among its siblings. Returns `level` (1 for a summary
# row or a top-level node, one more for each node below), `rank` (the node's
# concept type, `NA` for a reporting group, a residual row, and a summary
# row), `node_id`, `parent_id` (`NA` for a top-level node and a summary row),
# and `group`, the row's label: the node's name, the parent's name followed by
# "n.o.s." for a residual row, and the value's name for a summary row.
.infectious_agent_layout <- function(rates, rows) {
  values <- internal_infectious_agent_attribute_values
  summary <- tibble::tibble(
    level = 1L,
    rank = NA_character_,
    node_id = paste0(values$attribute, "|", values$key),
    parent_id = NA_character_,
    group = values$name)
  summary <- summary[summary$node_id %in% rates$node_id, ]

  nodes <- internal_infectious_agent_nodes
  nodes <- nodes[nodes$version == rows, ]
  tree <- rates[!rates$node_id %in% summary$node_id, ]
  residual <- endsWith(tree$node_id, infectious_agent_residual_suffix)
  node <- match(
    ifelse(residual,
           substr(tree$node_id, 1L,
                  nchar(tree$node_id) - nchar(infectious_agent_residual_suffix)),
           tree$node_id),
    nodes$node_id)
  tree <- tibble::tibble(
    level = nodes$depth[node] + 1L + residual,
    rank = dplyr::if_else(residual, NA_character_, nodes$rank[node]),
    node_id = tree$node_id,
    parent_id = dplyr::if_else(residual, nodes$node_id[node],
                               nodes$parent_id[node]),
    group = dplyr::if_else(residual, paste(nodes$name[node], "n.o.s."),
                           nodes$name[node]),
    residual = residual,
    rate = tree$rate)
  tree <- tree[order(tree$residual, -tree$rate, tree$group, tree$node_id,
                     method = "radix"), ]
  if (nrow(tree) == 0L)
    return(dplyr::bind_rows(summary, tree[names(summary)]))

  top <- is.na(tree$parent_id)
  children <- split(
    which(!top),
    factor(tree$parent_id[!top], levels = unique(tree$parent_id[!top])))
  visited <- integer()
  visit <- function(i) {
    visited <<- c(visited, i)
    for (child in children[[tree$node_id[i]]])
      visit(child)
  }
  for (i in which(top))
    visit(i)

  dplyr::bind_rows(
    summary,
    tree[visited, c("level", "rank", "node_id", "parent_id", "group")])
}

# The measures of a table's rows from department-level counts: `counts` holds
# one row per row key (the columns `by`), `department_key`, and, where
# `types` names infection types, `event_type_key`, with the row's events `n`
# and its denominator `total` there; a department or type without the row's
# denominator may be left out. `keys` holds the rows to return, in their
# order, a row without counts at 0 over 0. Returns `keys` with `n`, `total`,
# `pooled` (`n / total * scale`, `NA` without a denominator), and its interval
# `ci_lower` and `ci_upper` (Wilson for a share, exact Poisson for a rate over
# days or a ratio); with `include_quartiles`, the departments' quartiles of
# `.department_quartiles()`, with `denominator` the kind of `total`; and for
# each type, `n_<type>`, `total_<type>`, `pooled_<type>`, `ci_lower_<type>`,
# and `ci_upper_<type>` over the type's counts. `outside` names, by the row
# key and `department_key`, the departments that leave a row's quartiles and
# its pooled figures alike, so that both describe one population.
.infectious_agent_rate_columns <- function(
    counts, keys, by, scale, interval = c("wilson", "poisson"),
    denominator = c("count", "days"), include_quartiles = FALSE,
    types = NULL, outside = NULL) {
  interval <- rlang::arg_match(interval)
  denominator <- rlang::arg_match(denominator)
  if (!is.null(outside))
    counts <- dplyr::anti_join(counts, outside,
                               by = c(by, "department_key"))

  measures <- function(d) {
    r <- keys |>
      dplyr::left_join(
        d |>
          dplyr::summarise(
            n = sum(.data$n),
            total = sum(.data$total),
            .by = tidyselect::all_of(by)),
        by = by) |>
      dplyr::mutate(
        n = dplyr::coalesce(.data$n, 0L),
        total = dplyr::coalesce(.data$total, 0L),
        pooled = dplyr::if_else(
          .data$total > 0, .data$n / .data$total * scale, NA_real_))
    dplyr::bind_cols(
      r,
      if (interval == "wilson")
        wilson_ci_cols(r$n, r$total, scale = scale)
      else
        poisson_ci_cols(r$n, r$total, multiplier = scale))
  }

  r <- measures(counts)

  if (include_quartiles)
    r <- r |>
      dplyr::left_join(
        .department_quartiles(
          counts |>
            dplyr::summarise(
              events = sum(.data$n),
              denominator = sum(.data$total),
              .by = tidyselect::all_of(c(by, "department_key"))) |>
            dplyr::arrange(.data$department_key),
          r,
          by = by,
          scale = scale,
          type = if (interval == "wilson") "binomial" else "poisson",
          denominator = denominator),
        by = by)

  for (type in types)
    r <- dplyr::bind_cols(
      r,
      measures(counts[counts$event_type_key %in% type, ]) |>
        dplyr::select(!tidyselect::all_of(by)) |>
        dplyr::rename_with(\(name) paste0(name, "_", type)))
  r
}

# The least denominator with which a department enters a row's quartiles, by
# the kind of denominator: 5 of a count, 50 of a day denominator.
quartile_minimum_denominator <- c(count = 5L, days = 50L)

# The departments' quartiles of each row of a table, with their bootstrap
# intervals: `departments` holds one row per row key (the columns `by`) and
# department, with the department's `events` and `denominator` of the row, in
# the order the bootstrap takes them; `pooled` the rows, with their pooled
# rate `pooled` on the scale `scale`. A department enters a row's quartiles
# where its denominator of the row is positive and reaches the minimum
# `quartile_minimum_denominator` gives its kind (`denominator`), at a rate of 0
# where it has none of the row's events. A row shows its quartiles where at
# least 5 departments enter and their median denominator exceeds the
# denominator in which the pooled rate gives 1 event, `scale / pooled`, so
# that a row whose pooled rate is 0 shows none; the bootstrap
# (`bootstrap_quantile_ci()`, `type` its resampling distribution) takes the
# same departments. Returns `pooled`'s keys, in its order, with `q1`, `q2`,
# `q3`, their intervals `q1_ci_lower` to `q3_ci_upper`, all `NA` where the row
# does not show them, and `quartile_departments`, the number of departments
# that enter the row, whether or not it shows them.
.department_quartiles <- function(departments, pooled, by, scale,
                                  type = c("binomial", "poisson"),
                                  denominator = c("count", "days")) {
  type <- rlang::arg_match(type)
  denominator <- rlang::arg_match(denominator)
  minimum <- quartile_minimum_denominator[[denominator]]

  entering <- departments |>
    dplyr::filter(.data$denominator > 0, .data$denominator >= minimum) |>
    dplyr::summarise(
      quartile_departments = dplyr::n(),
      median_denominator = stats::median(.data$denominator),
      events = list(.data$events),
      exposure = list(.data$denominator),
      .by = tidyselect::all_of(by))
  r <- pooled |>
    dplyr::select(tidyselect::all_of(c(by, "pooled"))) |>
    dplyr::left_join(entering, by = by) |>
    dplyr::mutate(
      quartile_departments = dplyr::coalesce(.data$quartile_departments, 0L),
      shown = .data$quartile_departments >= 5L &
        !is.na(.data$pooled) & .data$pooled > 0 &
        .data$median_denominator > scale / .data$pooled)

  hidden <- tibble::tibble(
    q1 = NA_real_, q2 = NA_real_, q3 = NA_real_,
    q1_ci_lower = NA_real_, q1_ci_upper = NA_real_,
    q2_ci_lower = NA_real_, q2_ci_upper = NA_real_,
    q3_ci_lower = NA_real_, q3_ci_upper = NA_real_)
  quartiles <- purrr::pmap(
    list(r$shown, r$events, r$exposure),
    function(shown, events, exposure) {
      if (!shown)
        return(hidden)
      q <- stats::quantile(events / exposure * scale, quartile_probs,
                           names = FALSE)
      dplyr::bind_cols(
        tibble::tibble(q1 = q[1], q2 = q[2], q3 = q[3]),
        bootstrap_quantile_ci(events, exposure, type = type,
                              multiplier = scale))
    }) |>
    purrr::list_rbind()

  dplyr::bind_cols(r[by], if (nrow(r) == 0L) hidden[0, ] else quartiles) |>
    dplyr::mutate(quartile_departments = r$quartile_departments)
}
