# Tests for R/validation-exceptions.R — the exception list's reader and its
# resolution onto a dataset's keys.

# The documented class must sit on the condition itself, where a caller's
# handler for it fires; `expect_error(class =)` also accepts it on a parent
# condition wrapped around the one raised, which a handler would miss.
expect_error_of_class <- function(expr, class) {
  cnd <- rlang::catch_cnd(expr)
  expect_true(inherits(cnd, "error"))
  expect_true(
    inherits(cnd, class),
    info = paste("condition class:", paste(class(cnd), collapse = "/")))
}

# --- read_validation_exceptions() ---

write_exception_csv <- function(rows, path = withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())) {
  readr::write_csv(rows, path, na = "")
  path
}

exception_rows <- function()
  tibble::tibble(
    RULE_ID           = c("3", "12"),
    DEPARTMENT_CODE   = c("DEPT_1", "DEPT_1"),
    NEOIPC_PATIENT_ID = c("PAT_1", "PAT_1"),
    ENROLMENT_DATE    = c("2024-01-01", "2024-01-01"),
    EVENT_TYPE        = c("", "BSI"),
    EVENT_DATE        = c("", "2024-01-06"))

test_that("read_validation_exceptions reads the record columns in their types", {
  path <- write_exception_csv(exception_rows())
  ex <- neoipcr::read_validation_exceptions(path)
  expect_s3_class(ex, "tbl_df")
  expect_equal(nrow(ex), 2L)
  expect_type(ex$RULE_ID, "integer")
  expect_equal(ex$RULE_ID, c(3L, 12L))
  expect_type(ex$NEOIPC_PATIENT_ID, "character")
  expect_type(ex$DEPARTMENT_CODE, "character")
  expect_s3_class(ex$ENROLMENT_DATE, "Date")
  expect_s3_class(ex$EVENT_DATE, "Date")
  # An enrolment-level record leaves the event columns empty.
  expect_true(is.na(ex$EVENT_TYPE[1]))
  expect_true(is.na(ex$EVENT_DATE[1]))
  expect_equal(ex$EVENT_DATE[2], as.Date("2024-01-06"))
})

test_that("read_validation_exceptions accepts a list without department codes", {
  path <- write_exception_csv(exception_rows() |> dplyr::select(!"DEPARTMENT_CODE"))
  ex <- neoipcr::read_validation_exceptions(path)
  expect_false("DEPARTMENT_CODE" %in% names(ex))
  expect_equal(nrow(ex), 2L)
})

test_that("read_validation_exceptions is the shape import_dhis2 accepts", {
  path <- write_exception_csv(exception_rows())
  ex <- neoipcr::read_validation_exceptions(path)
  expect_no_error(neoipcr:::check_exception_list(ex, "unused header"))
})

test_that("read_validation_exceptions refuses a path that is not a file", {
  expect_error_of_class(
    neoipcr::read_validation_exceptions(file.path(tempdir(), "no-such-file.csv")),
    "neoipcr_invalid_exception_list")
  expect_error_of_class(
    neoipcr::read_validation_exceptions(tempdir()),
    "neoipcr_invalid_exception_list")
})

test_that("read_validation_exceptions refuses a file without the record columns", {
  path <- write_exception_csv(exception_rows() |> dplyr::select(!"ENROLMENT_DATE"))
  expect_error(
    neoipcr::read_validation_exceptions(path),
    regexp = "ENROLMENT_DATE",
    class = "neoipcr_invalid_exception_list")
})

test_that("read_validation_exceptions refuses a row with the wrong number of fields", {
  path <- write_exception_csv(exception_rows())
  # A record short of its two event fields would otherwise be read as an
  # enrolment-level one.
  lines <- c(readLines(path), "3,DEPT_1,PAT_2,2024-01-01")
  # A binary connection keeps the fixture LF whatever the platform.
  con <- file(path, open = "wb")
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
  close(con)
  # Named by its line in the file, the header being line one. The first
  # condition signalled is caught, so readr's own warning about the row,
  # were it let through, would be it and fail the class check.
  cnd <- rlang::catch_cnd(neoipcr::read_validation_exceptions(path))
  expect_true(inherits(cnd, "neoipcr_invalid_exception_list"))
  expect_match(conditionMessage(cnd), "Line 4")
})

test_that("read_validation_exceptions refuses a record without a patient id or department code", {
  # A blank value would match nothing and exempt nothing in silence.
  rows <- exception_rows()
  rows$NEOIPC_PATIENT_ID[2] <- ""
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "NEOIPC_PATIENT_ID",
    class = "neoipcr_invalid_exception_list")
  rows <- exception_rows()
  rows$DEPARTMENT_CODE[2] <- ""
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "DEPARTMENT_CODE",
    class = "neoipcr_invalid_exception_list")
})

test_that("read_validation_exceptions treats a department column left empty throughout as absent", {
  # The single-department list written in the six-column shape the tools
  # exchange.
  rows <- exception_rows()
  rows$DEPARTMENT_CODE <- ""
  ex <- neoipcr::read_validation_exceptions(write_exception_csv(rows))
  expect_false("DEPARTMENT_CODE" %in% names(ex))
  expect_equal(nrow(ex), 2L)
})

test_that("read_validation_exceptions keeps the columns of a file without records", {
  # The empty template exempts nothing and is valid for any number of
  # departments, so its department column is not dropped.
  ex <- neoipcr::read_validation_exceptions(write_exception_csv(exception_rows()[0, ]))
  expect_equal(nrow(ex), 0L)
  expect_true("DEPARTMENT_CODE" %in% names(ex))
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events      = make_test_events(1, enrollment_keys = 1L, patient_keys = 1L))
  ds$metadata$departments <- make_test_metadata_departments(n = 2)
  expect_equal(nrow(neoipcr::resolve_validation_exceptions(ds, ex)), 0L)
})

test_that("read_validation_exceptions names a value that does not parse", {
  bad_date <- exception_rows()
  bad_date$EVENT_DATE[2] <- "06.01.2024"
  cnd <- rlang::catch_cnd(neoipcr::read_validation_exceptions(write_exception_csv(bad_date)))
  expect_true(inherits(cnd, "neoipcr_invalid_exception_list"))
  expect_match(conditionMessage(cnd), "EVENT_DATE.*06\\.01\\.2024")
  bad_rule <- exception_rows()
  bad_rule$RULE_ID[1] <- "three"
  cnd <- rlang::catch_cnd(neoipcr::read_validation_exceptions(write_exception_csv(bad_rule)))
  expect_true(inherits(cnd, "neoipcr_invalid_exception_list"))
  expect_match(conditionMessage(cnd), "RULE_ID.*three")
})

test_that("read_validation_exceptions refuses a rule id no rule carries", {
  rows <- exception_rows()
  rows$RULE_ID[1] <- "99"
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "99",
    class = "neoipcr_invalid_exception_list")
  rows$RULE_ID[1] <- ""
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "RULE_ID",
    class = "neoipcr_invalid_exception_list")
})

test_that("read_validation_exceptions refuses an event type outside the stage vocabulary", {
  rows <- exception_rows()
  rows$EVENT_TYPE[2] <- "XYZ"
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "EVENT_TYPE",
    class = "neoipcr_invalid_exception_list")
})

test_that("read_validation_exceptions refuses an event record without its enrolment date", {
  rows <- exception_rows()
  rows$ENROLMENT_DATE[2] <- ""
  expect_error(
    neoipcr::read_validation_exceptions(write_exception_csv(rows)),
    regexp = "ENROLMENT_DATE",
    class = "neoipcr_invalid_exception_list")
})

# --- resolve_validation_exceptions() ---

# One patient (PAT_1) in department 1, enrolled 2024-01-01, with an admission
# event (key 1, 2024-01-01) and a sepsis event (key 2, 2024-01-06).
resolvable_ds <- function(n_departments = 1L, include_department = "full") {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "bsi"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-06"))))
  ds$metadata$departments <- make_test_metadata_departments(
    n = n_departments, include_department = include_department)
  ds
}

# One record at each level: rule 3 on the enrolment, rule 12 on the sepsis
# event, rule 1 on a patient the dataset does not hold.
written_exceptions <- function(department_code = NULL) {
  ex <- tibble::tibble(
    RULE_ID           = c(3L, 12L, 1L),
    NEOIPC_PATIENT_ID = c("PAT_1", "PAT_1", "PAT_9"),
    ENROLMENT_DATE    = as.Date(c("2024-01-01", "2024-01-01", NA)),
    EVENT_TYPE        = c(NA, "bsi", NA),
    EVENT_DATE        = as.Date(c(NA, "2024-01-06", NA)))
  if (!is.null(department_code))
    ex$DEPARTMENT_CODE <- department_code
  ex
}

test_that("resolve_validation_exceptions maps records onto the dataset's keys", {
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), written_exceptions())
  expect_named(keys, c("rule_id", "patient_key", "enrollment_key", "event_key"))
  expect_equal(keys$rule_id, c(3L, 12L, 1L))
  # An enrolment-level record resolves the patient and enrolment only.
  expect_equal(keys$patient_key[1], 1L)
  expect_equal(keys$enrollment_key[1], 1L)
  expect_true(is.na(keys$event_key[1]))
  # An event-level record resolves the event by type and date.
  expect_equal(keys$event_key[2], 2L)
  # A record naming a patient the dataset does not hold resolves to nothing.
  expect_true(all(is.na(c(keys$patient_key[3], keys$enrollment_key[3], keys$event_key[3]))))
})

test_that("resolve_validation_exceptions resolves a record as a whole or not at all", {
  ds <- resolvable_ds()
  # The event named is not in the dataset: nothing resolves, not even the
  # patient and enrolment that are, so the record cannot exempt them.
  ex <- written_exceptions()[2, ] |> dplyr::mutate(EVENT_DATE = as.Date("2024-01-07"))
  keys <- neoipcr::resolve_validation_exceptions(ds, ex)
  expect_equal(nrow(keys), 1L)
  expect_true(all(is.na(c(keys$patient_key, keys$enrollment_key, keys$event_key))))
  # Likewise an enrolment date the patient was not enrolled on.
  ex <- written_exceptions()[1, ] |> dplyr::mutate(ENROLMENT_DATE = as.Date("2024-01-02"))
  keys <- neoipcr::resolve_validation_exceptions(ds, ex)
  expect_true(all(is.na(c(keys$patient_key, keys$enrollment_key))))
  # A patient-level record names the patient alone.
  ex <- written_exceptions()[3, ] |> dplyr::mutate(NEOIPC_PATIENT_ID = "PAT_1")
  keys <- neoipcr::resolve_validation_exceptions(ds, ex)
  expect_equal(keys$patient_key, 1L)
  expect_true(is.na(keys$enrollment_key))
})

test_that("resolve_validation_exceptions refuses a record written at another level than its rule", {
  ds <- resolvable_ds()
  refuse <- function(ex, pattern)
    expect_error(
      neoipcr::resolve_validation_exceptions(ds, ex),
      regexp = pattern,
      class = "neoipcr_invalid_exception_list")
  # Rule 1 concerns the patient alone.
  refuse(written_exceptions()[3, ] |> dplyr::mutate(ENROLMENT_DATE = as.Date("2024-01-01")),
         "ENROLMENT_DATE")
  # Rule 3 is recorded on the enrolment, not on the admission event.
  refuse(written_exceptions()[1, ] |>
           dplyr::mutate(EVENT_TYPE = "adm", EVENT_DATE = as.Date("2024-01-01")),
         "EVENT_TYPE")
  # Rule 12 is recorded on a sepsis event and needs one named.
  refuse(written_exceptions()[2, ] |>
           dplyr::mutate(EVENT_TYPE = NA_character_, EVENT_DATE = as.Date(NA)),
         "EVENT_TYPE")
  refuse(written_exceptions()[2, ] |> dplyr::mutate(EVENT_TYPE = "hap"), "12")
  # The message names the rules concerned.
  refuse(written_exceptions() |> dplyr::mutate(EVENT_TYPE = c("adm", "bsi", NA),
                                              EVENT_DATE = as.Date(c("2024-01-01", "2024-01-06", NA))),
         "rule\\(s\\) 3")
})

test_that("resolve_validation_exceptions resolves a record to every dataset record it fits", {
  # Two sepsis events of one enrolment on one day: a record naming that day
  # names both, since nothing a list can say tells them apart.
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(3,
      enrollment_keys = c(1L, 1L, 1L),
      patient_keys    = c(1L, 1L, 1L),
      event_type_keys = c("adm", "bsi", "bsi"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-06", "2024-01-06"))))
  ds$metadata$departments <- make_test_metadata_departments(n = 1)
  keys <- neoipcr::resolve_validation_exceptions(ds, written_exceptions()[2, ])
  expect_equal(nrow(keys), 2L)
  expect_setequal(keys$event_key, c(2L, 3L))
  expect_equal(keys$rule_id, c(12L, 12L))
})

test_that("resolve_validation_exceptions ignores columns beyond the record's", {
  # A stray column named like a key must not derail the joins.
  ex <- written_exceptions() |> dplyr::mutate(enrollment_key = 7L, note = "x")
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), ex)
  expect_named(keys, c("rule_id", "patient_key", "enrollment_key", "event_key"))
  expect_equal(keys$enrollment_key[1], 1L)
  # A rule id given as a double comes back as the integer the rules compare.
  keys <- neoipcr::resolve_validation_exceptions(
    resolvable_ds(), written_exceptions() |> dplyr::mutate(RULE_ID = c(3, 12, 1)))
  expect_type(keys$rule_id, "integer")
})

test_that("resolve_validation_exceptions matches the event type in any case", {
  ex <- written_exceptions()
  ex$EVENT_TYPE[2] <- "BSI"
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), ex)
  expect_equal(keys$event_key[2], 2L)
})

test_that("resolve_validation_exceptions joins on the department code with more than one department", {
  ds <- resolvable_ds(n_departments = 2L)
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    regexp = "DEPARTMENT_CODE",
    class = "neoipcr_invalid_exception_list")
  keys <- neoipcr::resolve_validation_exceptions(ds, written_exceptions("DEPT_1"))
  expect_named(keys, c("rule_id", "department_key", "patient_key", "enrollment_key", "event_key"))
  expect_equal(keys$event_key[2], 2L)
  # A record for the other department names none of this department's records.
  keys <- neoipcr::resolve_validation_exceptions(ds, written_exceptions("DEPT_2"))
  expect_true(all(is.na(keys$patient_key)))
  # A record for a department that was not imported is kept, resolving to
  # nothing, so the list still comes back one row per record.
  keys <- neoipcr::resolve_validation_exceptions(
    ds, written_exceptions(c("DEPT_1", "DEPT_9", "DEPT_1")))
  expect_equal(keys$rule_id, c(3L, 12L, 1L))
  expect_equal(keys$patient_key[1], 1L)
  expect_true(is.na(keys$department_key[2]))
  expect_true(all(is.na(c(keys$patient_key[2], keys$enrollment_key[2], keys$event_key[2]))))
  # A record whose department matched but whose patient did not is `NA`
  # throughout, the department key included.
  expect_true(is.na(keys$department_key[3]))
})

test_that("resolve_validation_exceptions matches by department code whenever the dataset carries the codes", {
  # One department under the full tier: a list written for another
  # department exempts nothing here, even where the patient ids coincide.
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), written_exceptions("DEPT_2"))
  expect_true(all(is.na(keys$patient_key)))
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), written_exceptions("DEPT_1"))
  expect_equal(keys$patient_key[1], 1L)
  # Under the pseudo tier there are no codes to match against, so the
  # column is ignored and the patient id decides.
  keys <- neoipcr::resolve_validation_exceptions(
    resolvable_ds(include_department = "pseudo"), written_exceptions("DEPT_2"))
  expect_false("department_key" %in% names(keys))
  expect_equal(keys$patient_key[1], 1L)
})

test_that("resolve_validation_exceptions needs the department codes for several departments", {
  ds <- resolvable_ds(n_departments = 2L, include_department = "pseudo")
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions("DEPT_1")),
    class = "neoipcr_validation_needs_facts")
  # A department column left empty throughout counts as absent, so with
  # several departments the list is refused for want of the codes.
  expect_error(
    neoipcr::resolve_validation_exceptions(
      resolvable_ds(n_departments = 2L), written_exceptions(NA_character_)),
    regexp = "DEPARTMENT_CODE",
    class = "neoipcr_invalid_exception_list")
  # With one department it resolves like a list without the column.
  keys <- neoipcr::resolve_validation_exceptions(resolvable_ds(), written_exceptions(NA_character_))
  expect_false("department_key" %in% names(keys))
  expect_equal(keys$patient_key[1], 1L)
})

test_that("resolve_validation_exceptions needs a department tier", {
  # Without departments the count is unknown, so the list is refused as the
  # import refuses it, rather than misread as several departments.
  ds <- resolvable_ds(include_department = "no")
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    regexp = "include_department",
    class = "neoipcr_validation_needs_facts")
  ds$metadata$departments <- NULL
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    class = "neoipcr_validation_needs_facts")
})

test_that("resolve_validation_exceptions needs the patient ids", {
  # The full patient tier alone does not carry them; the hint names what does.
  ds <- resolvable_ds()
  ds$patients$patient_id <- NULL
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    regexp = "patient_columns",
    class = "neoipcr_validation_needs_facts")
})

test_that("resolve_validation_exceptions refuses blank identifiers in a list built in code", {
  # readr turns an empty field into `NA`; a data frame can carry the empty
  # string or whitespace instead, which would match nothing in silence.
  for (blank in c("", "  ")) {
    expect_error(
      neoipcr::resolve_validation_exceptions(
        resolvable_ds(),
        written_exceptions() |> dplyr::mutate(NEOIPC_PATIENT_ID = c("PAT_1", blank, "PAT_9"))),
      regexp = "NEOIPC_PATIENT_ID",
      class = "neoipcr_invalid_exception_list")
    expect_error(
      neoipcr::resolve_validation_exceptions(
        resolvable_ds(), written_exceptions(c("DEPT_1", blank, "DEPT_1"))),
      regexp = "DEPARTMENT_CODE",
      class = "neoipcr_invalid_exception_list")
  }
})

test_that("resolve_validation_exceptions needs the full enrollment and event tiers", {
  # The joins read the enrollments' patient link and the events' type, which
  # the pseudonymized tiers do not carry.
  ds <- resolvable_ds()
  ds$metadata$dataset_options$include_enrollment <- "pseudo"
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    regexp = "include_enrollment")
  ds <- resolvable_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  expect_error(
    neoipcr::resolve_validation_exceptions(ds, written_exceptions()),
    regexp = "include_event")
})

test_that("resolve_validation_exceptions checks the list's shape", {
  expect_error(
    neoipcr::resolve_validation_exceptions(
      resolvable_ds(), written_exceptions() |> dplyr::select(!"EVENT_DATE")),
    class = "neoipcr_invalid_exception_list")
  # A rule id that names no rule, or is not a whole number, is refused as
  # `validate(rules =)` refuses it, rather than exempting nothing in silence.
  expect_error(
    neoipcr::resolve_validation_exceptions(
      resolvable_ds(), written_exceptions() |> dplyr::mutate(RULE_ID = c(3L, 99L, 1L))),
    regexp = "99",
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::resolve_validation_exceptions(
      resolvable_ds(), written_exceptions() |> dplyr::mutate(RULE_ID = c(3, 1.5, 1))),
    class = "neoipcr_invalid_exception_list")
})
