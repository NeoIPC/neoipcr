generate_programStage <- function(programStageName, programStageElements, dataElements)
{
  programStage <- list()

  if(programStageName == "Admission")
  {
    if(programStageElements)
      programStage <- c(programStage, list(
        id = jsonlite::unbox("YGowWPumDia"),
        name = jsonlite::unbox("Admission"),
        displayName = jsonlite::unbox("Aufnahme")))
    if(dataElements)
      programStage <- c(programStage, list(
        programStageDataElements = list(
          list(
            dataElement = list(
              id = jsonlite::unbox("Lwa9Jp5xSnR"),
              code = jsonlite::unbox("NEOIPC_ADMISSION_LOS"),
              valueType = jsonlite::unbox("INTEGER_ZERO_OR_POSITIVE"),
              displayName = jsonlite::unbox("NeoIPC Admission Length of Stay"),
              displayShortName = jsonlite::unbox("NeoIPC Adm. LOS"),
              displayFormName = jsonlite::unbox("Day of occurrence after admission"),
              displayDescription = jsonlite::unbox("Length of Stay (should always be 0). For Reporting purpose only"))),
          list(
            dataElement = list(
              id = jsonlite::unbox("rvq4L9wWbwW"),
              code = jsonlite::unbox("NEOIPC_ADMISSION_DOL"),
              valueType = jsonlite::unbox("INTEGER_POSITIVE"),
              displayName = jsonlite::unbox("NeoIPC Admission on day of life"),
              displayShortName = jsonlite::unbox("NeoIPC Adm. DOL"),
              displayFormName = jsonlite::unbox("Admission on day of life"),
              displayDescription = jsonlite::unbox("For infants that have not been delivered in your own hospital, record the infant\'s day of life on the day of admission (day of birth = day of life 1. The next day, starting at 00:00, is the second day of life.)"))),
          list(
            dataElement = list(
              id = jsonlite::unbox("AgBqfnnsUzd"),
              code = jsonlite::unbox("NEOIPC_ADMISSION_TYPE"),
              valueType = jsonlite::unbox("INTEGER_POSITIVE"),
              displayName = jsonlite::unbox("NeoIPC Admission type"),
              displayShortName = jsonlite::unbox("NeoIPC Adm. type"),
              displayFormName = jsonlite::unbox("Admission type"),
              displayDescription = jsonlite::unbox("Describes if the infant was born in your hospital or if it was admitted after birth and if so, how long after birth."),
              optionSet = list(id = jsonlite::unbox("l6HnyhcwF28")))))))
  }
  else if(programStageName == "Surgical Procedure")
  {
    if(programStageElements)
      programStage <- c(programStage, list(
        id = jsonlite::unbox("BHWwaviIFvy"),
        name = jsonlite::unbox("Surgical Procedure"),
        displayName = jsonlite::unbox("Operativer Eingriff")))
    if(dataElements)
      programStage <- c(programStage, list(
        programStageDataElements = list(
          list(
            dataElement = list(
              id = jsonlite::unbox("DTZ9HfILgnX"),
              code = jsonlite::unbox("NEOIPC_SURGERY_EMERGENCY_PROCEDURE"),
              valueType = jsonlite::unbox("BOOLEAN"),
              displayName = jsonlite::unbox("NeoIPC Surgery Emergency procedure"),
              displayShortName = jsonlite::unbox("NeoIPC Surg. Emergency"),
              displayFormName = jsonlite::unbox("Emergency procedure"),
              displayDescription = jsonlite::unbox("Yes: A procedure that is documented per the facility’s protocol to be an emergency or urgent procedure. No: The intervention is initiated and performed in a planned manner Unknown: No information available."))))))
  }

  programStage
}

generate_programStages <- function(programStages, dataElements)
{
  programStages <- list(
    generate_programStage("Admission", programStages, dataElements),
    generate_programStage("Surgical Procedure", programStages, dataElements))
}

generate_trackedEntityAttributes <- function()
{
  l <- list(
    list(trackedEntityAttribute = list(
      id = jsonlite::unbox("yQwpowV0o08"),
      code = jsonlite::unbox("NEOIPC_PATIENT_ID"),
      valueType = jsonlite::unbox("TEXT"),
      displayName = jsonlite::unbox("NeoIPC Patient Identifier"),
      displayShortName = jsonlite::unbox("NeoIPC Pat. Id"),
      displayFormName = jsonlite::unbox("NeoIPC Patient Identifier"),
      displayDescription = jsonlite::unbox("Use this identifier to uniquely identify a patient in the system. Ideally use an unique random string of characters. If have a requirement to identify a patient you have entered here, you can use this identifier as the NeoIPC key for pseudonymization. NEVER use an identifier that is used anywhere else and that you do not fully control (e.g. do NOT use the patient id from your hospital information system)."))
    ),
    list(trackedEntityAttribute = list(
      id = jsonlite::unbox("E5OMg8BC8be"),
      code = jsonlite::unbox("NEOIPC_TEA_SEX"),
      valueType = jsonlite::unbox("LETTER"),
      displayName = jsonlite::unbox("NeoIPC Patient Sex"),
      displayShortName = jsonlite::unbox("NeoIPC Pat. Sex"),
      displayFormName = jsonlite::unbox("Sex"),
      displayDescription = jsonlite::unbox("Typically the phenotypic sex of the patient. If sex cannot be determined from the patient\'s phenotype or genotype, or if the genotype is neither XX nor XY, it is considered undetermined for purposes of surveillance."),
      optionSet = list(id = jsonlite::unbox("R2yCnsqxamL")))
    )
  )
  l
}

generate_program <- function(programId, programStages, dataElements, trackedEntityAttributes)
{
  if(programId)
    program <- list(id = jsonlite::unbox("D8mSSpOpsKj"))
  else
    program <- list()

  if(programStages || dataElements)
    program <- c(program, list(programStages = generate_programStages(programStages, dataElements)))

  if(trackedEntityAttributes)
    program <- c(program, list(programTrackedEntityAttributes = generate_trackedEntityAttributes()))

  program
}

generate_system <- function()
{
  list(
    date = jsonlite::unbox("2024-11-08T14:06:41.216+0000"),
    id = jsonlite::unbox("72c2bd70-573a-4d69-8bc3-f7bb431bdc23"),
    rev = jsonlite::unbox("3fcd748"),
    version = jsonlite::unbox("2.40.3.2"))
}

generate_organisationUnitGroups <- function(countries, testUnits)
{
  organisationUnitGroups <- list()

  if(countries)
    organisationUnitGroups <- list(
      code = jsonlite::unbox("COUNTRY"),
      organisationUnits = list(
        list(
          code = jsonlite::unbox("CH"),
          displayName = jsonlite::unbox("Switzerland"),
          displayShortName = jsonlite::unbox("Switzerland")
        ),
        list(
          code = jsonlite::unbox("DE"),
          displayName = jsonlite::unbox("Germany"),
          displayShortName = jsonlite::unbox("Germany"))))

  if(testUnits)
    organisationUnitGroups <- c(organisationUnitGroups, list(
      code = jsonlite::unbox("TEST_UNITS"),
      organisationUnits = list(
        list(
          id = jsonlite::unbox("VUNdfvqcGI7"),
          code = jsonlite::unbox("TEST_01"),
          displayName = jsonlite::unbox("Test unit 01"),
          displayShortName = jsonlite::unbox("Test 01")
        ),
        list(
          id = jsonlite::unbox("hzte6b3Z8Zd"),
          code = jsonlite::unbox("TEST_02"),
          displayName = jsonlite::unbox("Test unit 02"),
          displayShortName = jsonlite::unbox("Test 02")))))

  organisationUnitGroups
}

generate_metadata <- function(system, programId,
                              programStages, dataElements,
                              trackedEntityAttributes, countries,
                              testUnits)
{
  metadata <- list()

  if(system)
    metadata <- c(metadata, list(system = generate_system()))

  if(programId || programStages || dataElements || trackedEntityAttributes)
    metadata <- c(metadata, list(programs = list(generate_program(programId, programStages, dataElements, trackedEntityAttributes))))

  if(countries || testUnits)
    metadata <- c(metadata, list(organisationUnitGroups = list(generate_organisationUnitGroups(countries, testUnits))))

  jsonlite::toJSON(metadata)
}

rmd <- function(add_system = TRUE, add_programId = TRUE,
                add_programStages = TRUE, add_dataElements = TRUE,
                add_trackedEntityAttributes = TRUE, add_Countries = TRUE,
                add_testUnits = TRUE)
{
  json_text <- generate_metadata(add_system, add_programId,
                                 add_programStages, add_dataElements,
                                 add_trackedEntityAttributes, add_Countries,
                                 add_testUnits)

  #browser()
  metadata <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  ret <- read_metadata(metadata)
  ret
}

# Tests below

test_that("dhis2_connection_options defaults", {
  expect_equal(
    dhis2_connection_options("d2pat_test_token")$base_url,
    "https://neoipc.charite.de/api")
})

test_that("dhis2_connection_options defaults", {
  expect_equal(
    dhis2_connection_options(
      token = "d2pat_test_token",
      scheme = "http",
      hostname = "testhost",
      port = 8080,
      path = "/api/41")$base_url,
    "http://testhost:8080/api/41")
})

test_that("dhis2_connection_options fails if token and username are set", {
  expect_error(
    dhis2_connection_options(
      token = "d2pat_test_token",
      username = "admin"),
    "Exactly one of `token` or `username` must be supplied.")
})

test_that("read_metadata fails if system missing", {
  expect_error(rmd(add_system = FALSE), class = "neoipcr_metadata_system_missing")
})

test_that("read_metadata fails if program id missing", {
  expect_error(rmd(add_programId = FALSE), class = "neoipcr_metadata_program_missing")
})

test_that("read_metadata fails if programStages missing", {
  expect_error(rmd(add_programStages = FALSE, add_dataElements = FALSE), class = "neoipcr_metadata_programStages_missing")
})

test_that("read_metadata fails if dataElements missing", {
  expect_error(rmd(add_dataElements = FALSE), class = "neoipcr_metadata_programStageDataElements_missing")
})

test_that("read_metadata fails if trackedEntityAttributes missing", {
  expect_error(rmd(add_trackedEntityAttributes = FALSE), class = "neoipcr_metadata_programTrackedEntityAttributes_missing")
})

test_that("read_metadata fails if trackedEntityAttributes missing", {
  expect_error(rmd(add_trackedEntityAttributes = FALSE), class = "neoipcr_metadata_programTrackedEntityAttributes_missing")
})

test_that("read_metadata succeeds even if countries missing", {
  expect_no_error(rmd(add_Countries = FALSE))
})

test_that("read_metadata succeeds even if testUnits missing", {
  expect_no_error(rmd(add_testUnits = FALSE))
})

test_that("read_metadata succeeds even if countries and testUnits missing", {
  expect_no_error(rmd(add_Countries = FALSE, add_testUnits = FALSE))
})

test_that("read_metadata reads data", {
  metadata <- rmd()

  # system
  expect_equal(
    metadata$system$date,
    readr::parse_datetime("2024-11-08T14:06:41.216+0000"))
  expect_equal(
    metadata$system$id,
    uuid::as.UUID("72c2bd70-573a-4d69-8bc3-f7bb431bdc23"))
  expect_equal(
    metadata$system$rev,
    "3fcd748")
  expect_equal(
    metadata$system$version,
    as.numeric_version("2.40.3.2"))

  # programId
  expect_equal(metadata$programId, "D8mSSpOpsKj")

  # programStages
  expect_equal(metadata$programStages$name, c("Admission", "Surgical Procedure"))

  # dataElements
  expect_equal(metadata$dataElements$id, c("Lwa9Jp5xSnR", "rvq4L9wWbwW", "AgBqfnnsUzd", "DTZ9HfILgnX"))
  expect_equal(metadata$dataElements$optionSet, c(NA, NA, "l6HnyhcwF28", NA))

  # trackedEntityAttributes
  expect_equal(metadata$trackedEntityAttributes$id, c("yQwpowV0o08", "E5OMg8BC8be"))
  expect_equal(metadata$trackedEntityAttributes$optionSet, c(NA, "R2yCnsqxamL"))

  # countries
  expect_equal(metadata$countries$code, c("CH", "DE"))
})


# hospitals
#expect_equal(metadata$hospitals$code, c("DE_TEST_PARENT", "GR_TEST_PARENT"))

# departments
#expect_equal(metadata$departments$displayName, c("Test department 1", "Test department 2", "Test department 3", "Test department 4"))


# test_that("read_metadata reads hospitals", {
#   metadata <- rmd(
#     '
# {
#     "organisationUnits": [
#         {
#             "displayName": "Test department 1",
#             "displayShortName": "Test 1",
#             "id": "wcFyerorAmG",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "TEST_UNITS",
#                 "displayDescription": "A few test units to evaluate DHIS2 as NeoIPC data collection platform.",
#                 "displayName": "Test Units",
#                 "displayShortName": "Test Units",
#                 "id": "aCbeNKDGVks",
#                 "parent": {
#                     "code": "NEOIPC"
#                 }
#             }
#         },
#         {
#             "comment": "A `real` department",
#             "displayDescription": "This simulates a real neo department",
#             "displayName": "Test department 2",
#             "displayShortName": "Test 2",
#             "id": "oSpSLxOVIxE",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "DE_TEST_PARENT",
#                 "comment": "This simulates a real hospital",
#                 "displayName": "Test hospital 1",
#                 "displayShortName": "Test hosp. 1",
#                 "geometry": {
#                     "coordinates": [
#                         13.37819,
#                         52.523628
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "oOpnrFqrBhJ",
#                 "parent": {
#                     "code": "DE"
#                 }
#             }
#         },
#         {
#             "comment": "A `real` department",
#             "displayDescription": "This simulates a real neo department",
#             "displayName": "Test department 3",
#             "displayShortName": "Test 3",
#             "id": "yTKmXIhJQkP",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "DE_TEST_PARENT",
#                 "comment": "This simulates a real hospital",
#                 "displayName": "Test hospital 1",
#                 "displayShortName": "Test hosp. 1",
#                 "geometry": {
#                     "coordinates": [
#                         13.37819,
#                         52.523628
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "oOpnrFqrBhJ",
#                 "parent": {
#                     "code": "DE"
#                 }
#             }
#         },
#         {
#             "displayName": "Test department 4",
#             "displayShortName": "Test 4",
#             "id": "SEnzmeiCYis",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "GR_TEST_PARENT",
#                 "displayName": "Test hospital 2",
#                 "displayShortName": "Test hosp. 2",
#                 "geometry": {
#                     "coordinates": [
#                         20.840438,
#                         39.621562
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "TKfIqRxsPuF",
#                 "parent": {
#                     "code": "EL"
#                 }
#             }
#         }
#     ]
# }')
#
#   expect_equal(metadata$hospitals |> dplyr::arrange(code) |> dplyr::pull(code), c("DE_TEST_PARENT", "GR_TEST_PARENT"))
# })
#
# test_that("read_metadata reads departments", {
#   metadata <- rmd(
#     '
# {
#     "organisationUnits": [
#         {
#             "displayName": "Test department 1",
#             "displayShortName": "Test 1",
#             "id": "wcFyerorAmG",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "TEST_UNITS",
#                 "displayDescription": "A few test units to evaluate DHIS2 as NeoIPC data collection platform.",
#                 "displayName": "Test Units",
#                 "displayShortName": "Test Units",
#                 "id": "aCbeNKDGVks",
#                 "parent": {
#                     "code": "NEOIPC"
#                 }
#             }
#         },
#         {
#             "comment": "A `real` department",
#             "displayDescription": "This simulates a real neo department",
#             "displayName": "Test department 2",
#             "displayShortName": "Test 2",
#             "id": "oSpSLxOVIxE",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "DE_TEST_PARENT",
#                 "comment": "This simulates a real hospital",
#                 "displayName": "Test hospital 1",
#                 "displayShortName": "Test hosp. 1",
#                 "geometry": {
#                     "coordinates": [
#                         13.37819,
#                         52.523628
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "oOpnrFqrBhJ",
#                 "parent": {
#                     "code": "DE"
#                 }
#             }
#         },
#         {
#             "comment": "A `real` department",
#             "displayDescription": "This simulates a real neo department",
#             "displayName": "Test department 3",
#             "displayShortName": "Test 3",
#             "id": "yTKmXIhJQkP",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "DE_TEST_PARENT",
#                 "comment": "This simulates a real hospital",
#                 "displayName": "Test hospital 1",
#                 "displayShortName": "Test hosp. 1",
#                 "geometry": {
#                     "coordinates": [
#                         13.37819,
#                         52.523628
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "oOpnrFqrBhJ",
#                 "parent": {
#                     "code": "DE"
#                 }
#             }
#         },
#         {
#             "displayName": "Test department 4",
#             "displayShortName": "Test 4",
#             "id": "SEnzmeiCYis",
#             "openingDate": "2024-11-10T00:00:00.000",
#             "parent": {
#                 "code": "GR_TEST_PARENT",
#                 "displayName": "Test hospital 2",
#                 "displayShortName": "Test hosp. 2",
#                 "geometry": {
#                     "coordinates": [
#                         20.840438,
#                         39.621562
#                     ],
#                     "type": "Point"
#                 },
#                 "id": "TKfIqRxsPuF",
#                 "parent": {
#                     "code": "EL"
#                 }
#             }
#         }
#     ]
# }')
#
#   expect_equal(
#     metadata$departments |> dplyr::arrange(displayName) |> dplyr::pull(displayName),
#     c("Test department 1", "Test department 2", "Test department 3",
#       "Test department 4"))
# })
#
# # ToDo: get_users,
