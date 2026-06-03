validation_rules <- list(
  list(
    id = 1L,
    fun = validation_rule_1,
    formatter = function(x) {
      gettext("The patient record does not have an enrolment.")
    }
  ),
  list(
    id = 2L,
    fun = validation_rule_2,
    formatter = function(x) {
      gettext(
        "The patient record has a completed surveillance end form but the enrolment is still active.")
    }
  ),
  list(
    id = 3L,
    fun = validation_rule_3,
    formatter = function(x) {
      gettextf(
        "The admission date in the admission form (%s) differs from the admission date in the enrolment (%s).",
        format(x$occurredAt, format = "%x"),
        format(x$enrolledAt, format = "%x"))
    }
  ),
  list(
    id = 4L,
    fun = validation_rule_4,
    formatter = function(x) {
      gettextf(
        "The date of the end of the surveillance (%s) is earlier than the date of admission on the admission form (%s).",
        format(x$endOccurredAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"))
    }
  ),
  list(
    id = 5L,
    fun = validation_rule_5,
    formatter = function(x) {
      gettextf(
        "The patient record's admission form is not completed (status is '%s').",
        as.character(x$status))
    }
  ),
  list(
    id = 6L,
    fun = validation_rule_6,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment but the surveillance end form is not completed (status is '%s').",
        as.character(x$status))
    }
  ),
  list(
    id = 7L,
    fun = validation_rule_7,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a sepsis form is not completed (enrolment status is '%s', surveillance end form status is '%s', sepsis form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$bsi_status)
      )
    }
  ),
  list(
    id = 8L,
    fun = validation_rule_8,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a necrotizing enterocolitis form is not completed (enrolment status is '%s', surveillance end form status is '%s', necrotizing enterocolitis form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$nec_status)
      )
    }
  ),
  list(
    id = 9L,
    fun = validation_rule_9,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a pneumonia form is not completed (enrolment status is '%s', surveillance end form status is '%s', pneumonia form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$hap_status)
      )
    }
  ),
  list(
    id = 10L,
    fun = validation_rule_10,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a surgical procedure form is not completed (enrolment status is '%s', surveillance end form status is '%s', surgical procedure form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$pro_status)
      )
    }
  ),
  list(
    id = 11L,
    fun = validation_rule_11,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a surgical site infection form is not completed (enrolment status is '%s', surveillance end form status is '%s', surgical site infection form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$ssi_status)
      )
    }
  ),
  list(
    id = 12L,
    fun = validation_rule_12,
    formatter = function(x) {
      gettextf(
        "The patient record contains a sepsis form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', sepsis date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$bsiOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 13L,
    fun = validation_rule_13,
    formatter = function(x) {
      gettextf(
        "The patient record contains a necrotizing enterocolitis form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', necrotizing enterocolitis date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$necOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 14L,
    fun = validation_rule_14,
    formatter = function(x) {
      gettextf(
        "The patient record contains a pneumonia form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', pneumonia date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$hapOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 15L,
    fun = validation_rule_15,
    formatter = function(x) {
      gettextf(
        "The patient record contains a surgical procedure form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', surgical procedure date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$surOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 16L,
    fun = validation_rule_16,
    formatter = function(x) {
      gettextf(
        "The patient record contains a surgical site infection form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', surgical site infection date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$ssiOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 17L,
    fun = validation_rule_17,
    formatter = function(x) {
      gettextf(
        "The patient record contains an enrolment with a time interval that overlaps with that of another enrolment (this enrolment has an interval from %s to %s and the other enrolment has an interval from %s to %s).",
        format(x$admOccurredAt_1, format = "%x"),
        format(x$endOccurredAt_1, format = "%x"),
        format(x$admOccurredAt_2, format = "%x"),
        format(x$endOccurredAt_2, format = "%x")
      )
    }
  ),
  list(
    id = 18L,
    fun = validation_rule_18,
    formatter = function(x) {
      gettextf(
        "The number of patient days (%s) does not match the calculated value (%s).",
        as.character(x$patient_days),
        as.character(x$patient_days_calculated)
      )
    }
  ),
  list(
    id = 19L,
    fun = validation_rule_19,
    formatter = function(x) {
      inf_type_string <- switch(
        x$ssi_infection_type,
        gettext("superficial incisional SSI"),
        gettext("deep incisional SSI"),
        gettext("organ/space SSI"))
      gettextf(
        "The surgical site infection (%s) did not occur during the follow-up period of a recorded surgical procedure.",
        inf_type_string
      )
    }
  ),
  list(
    id = 20L,
    fun = validation_rule_20,
    formatter = function(x) {
      sec_bsi_part <- ""
      if(x$is_secondary_bsi)
        sec_bsi_part <- gettext(" causing secondary sepsis", trim = FALSE)

      gettextf(
        "The pathogen manually entered as pathogen %i%s ('%s') cannot be assigned.",
        x$pathogen_index,
        as.character(sec_bsi_part),
        as.character(x$pathogen_name)
      )
    }
  ),
  list(
    id = 21L,
    fun = validation_rule_21,
    formatter = function(x) {
      gettextf(
        "The sum of all antibiotic substance days (%i) is less than the total number of antibiotic days (%i).",
        x$ab_substance_days,
        x$ab_days
      )
    }
  ),
  list(
    id = 22L,
    fun = validation_rule_22,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the main procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 23L,
    fun = validation_rule_23,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the first side procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 24L,
    fun = validation_rule_24,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the second side procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 25L,
    fun = validation_rule_25,
    formatter = function(x) {
      gettext(
        "The patient record has a completed enrolment but no surveillance end form."
      )
    }
  ),
  list(
    id = 26L,
    fun = validation_rule_26,
    formatter = function(x) {
      gettext(
        "The patient record has a completed enrolment but no admission form."
      )
    }
  ),
  list(
    id = 27L,
    fun = validation_rule_27,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the sepsis form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 28L,
    fun = validation_rule_28,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the sepsis form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 29L,
    fun = validation_rule_29,
    formatter = function(x) {
      gettextf(
        "The sepsis occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 30L,
    fun = validation_rule_30,
    formatter = function(x) {
      gettextf(
        "The sepsis occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 31L,
    fun = validation_rule_31,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the pneumonia form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 32L,
    fun = validation_rule_32,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the pneumonia form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 33L,
    fun = validation_rule_33,
    formatter = function(x) {
      gettextf(
        "The pneumonia occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 34L,
    fun = validation_rule_34,
    formatter = function(x) {
      gettextf(
        "The pneumonia occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 35L,
    fun = validation_rule_35,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the necrotising enterocolitis form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 36L,
    fun = validation_rule_36,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the necrotising enterocolitis form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 37L,
    fun = validation_rule_37,
    formatter = function(x) {
      gettextf(
        "The necrotising enterocolitis occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 38L,
    fun = validation_rule_38,
    formatter = function(x) {
      gettextf(
        "The necrotising enterocolitis occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 39L,
    fun = validation_rule_39,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the surgical procedure form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 40L,
    fun = validation_rule_40,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the surgical procedure form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 41L,
    fun = validation_rule_41,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the surgical site infection form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 42L,
    fun = validation_rule_42,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the surgical site infection form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  )
)

# Join hierarchy context from `metadata$departments` onto a fact tibble.
#
# Under the schema contract's inheritance rule, enrollments and patients
# carry `department_key` but not `hospital_key` when
# `include_department = "full"` (departments already has it via
# pre-join). Validation rules need `hospital_key` in their output
# context so the renderer can resolve which hospital a problem belongs
# to. This helper joins it from departments — called explicitly by each
# rule at its fact-tibble entry point rather than silently relying on
# `any_of("hospital_key")` finding the column on the fact tibble.
.with_hierarchy_context <- function(fact_tibble, departments)
{
  if (!("department_key" %in% names(fact_tibble)) ||
      !("department_key" %in% names(departments)))
    return(fact_tibble)

  join_cols <- intersect(
    c("hospital_key", "country_key", "world_bank_class_key"),
    setdiff(names(departments), names(fact_tibble)))

  if (length(join_cols) == 0L)
    return(fact_tibble)

  fact_tibble |>
    dplyr::left_join(
      departments |>
        dplyr::select("department_key", tidyselect::all_of(join_cols)),
      dplyr::join_by("department_key"))
}

validate <- function(x, rules = NULL, exceptions = NULL)
{
  check_neoipcr_ds(x)
  # Validation rules access patients, enrollments, events, and per-event
  # data. If any link-privacy gate is "no", rules that reference those
  # tibbles would fail with unhelpful column-absent errors. Require the
  # same gates as the calc pipeline.
  assert_options_for(x, required = list(
    include_patient    = c("pseudo", "full"),
    include_enrollment = c("pseudo", "full"),
    include_event      = c("pseudo", "full")
  ), fn_name = "validate")

  r <- validation_rules |>
    lapply(\(r)if(is.null(rules)||r$id%in%rules)r$fun(x,exceptions)) |>
    dplyr::bind_rows() |>
    dplyr::select(
      tidyselect::any_of(
        c("rule_id","patient_key","enrollment_key","event_key","context")))

  invisible(r)
}
