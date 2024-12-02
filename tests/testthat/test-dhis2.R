generate_programStage <- function(programStageName, programStageElements, dataElements)
{
  programStage <- list()

  if(programStageName == "Admission")
  {
    if(programStageElements)
      programStage <- c(programStage, list(
        id = jsonlite::unbox("YGowWPumDia"),
        name = jsonlite::unbox("Admission"),
        displayName = jsonlite::unbox("Aufnahme"),
        displayFormName = jsonlite::unbox("Aufnahme")))
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
              optionSet = list(code = jsonlite::unbox("NEOIPC_ADMISSION_TYPES")))))))
  }
  else if(programStageName == "Surgical Procedure")
  {
    if(programStageElements)
      programStage <- c(programStage, list(
        id = jsonlite::unbox("BHWwaviIFvy"),
        name = jsonlite::unbox("Surgical Procedure"),
        displayName = jsonlite::unbox("Operativer Eingriff"),
        displayFormName = jsonlite::unbox("Operativer Eingriff")))
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
  organisationUnitGroups <- NULL

  if(countries)
    organisationUnitGroups <- list(list(
      code = jsonlite::unbox("COUNTRY"),
      organisationUnits = list(
        list(
          id = jsonlite::unbox("P3M2xL6Gtbs"),
          code = jsonlite::unbox("CH"),
          displayName = jsonlite::unbox("Switzerland"),
          displayShortName = jsonlite::unbox("Switzerland")
        ),
        list(
          id = jsonlite::unbox("TS5pOUJsdoa"),
          code = jsonlite::unbox("DE"),
          displayName = jsonlite::unbox("Germany"),
          displayShortName = jsonlite::unbox("Germany")))))

  if(testUnits)
    organisationUnitGroups <- c(organisationUnitGroups, list(list(
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
          displayShortName = jsonlite::unbox("Test 02"))))))

  organisationUnitGroups
}

generate_optionGroupSets <- function(awareCategories, atc5Categories)
{
  optionGroupSets <- NULL

  if(awareCategories)
    optionGroupSets <- list(list(
      code = jsonlite::unbox("WHO_AWARE"),
      optionGroups = list(
        list(
          code = jsonlite::unbox("WHO_AWARE_ACCESS"),
          displayName = jsonlite::unbox("AWaRe Access"),
          displayShortName = jsonlite::unbox("AWaRe A"),
          displayDescription = jsonlite::unbox("Access antibiotics that have a narrow spectrum of activity and a good safety profile in terms of side-effects."),
          options = list(
            list(code = jsonlite::unbox("J01CA04")),
            list(code = jsonlite::unbox("J01CA15"))
          )),
        list(
          code = jsonlite::unbox("WHO_AWARE_RESERVE"),
          displayName = jsonlite::unbox("AWaRe Reserve"),
          displayShortName = jsonlite::unbox("AWaRe R"),
          displayDescription = jsonlite::unbox("Reserve antibiotics that are last-choice antibiotics used to treat multidrug-resistant infections."),
          options = list(
            list(code = jsonlite::unbox("J01DI03")),
            list(code = jsonlite::unbox("J01XA03"))
          )),
        list(
          code = jsonlite::unbox("WHO_AWARE_WATCH"),
          displayName = jsonlite::unbox("AWaRe Watch"),
          displayShortName = jsonlite::unbox("AWaRe W"),
          displayDescription = jsonlite::unbox("Watch antibiotics that are broader-spectrum antibiotics and are recommended as first-choice options for patients with more severe clinical presentations or for infections where the causative pathogens are more likely to be resistant to Access antibiotics."),
          options = list(
            list(code = jsonlite::unbox("J01XA02")),
            list(code = jsonlite::unbox("J01MA09"))
          ))
        )))

  if(atc5Categories)
    optionGroupSets <- c(optionGroupSets, list(list(
      code = jsonlite::unbox("ATC5"),
      optionGroups = list(
        list(
          code = jsonlite::unbox("J01CF"),
          displayName = jsonlite::unbox("Beta-lactamase resistant penicillins"),
          displayShortName = jsonlite::unbox("Beta-lactamase resistant penicillins"),
          options = list(
            list(code = jsonlite::unbox("J01CF04")),
            list(code = jsonlite::unbox("J01CF01"))
          )),
        list(
          code = jsonlite::unbox("J01DH"),
          displayName = jsonlite::unbox("Carbapenems"),
          displayShortName = jsonlite::unbox("Carbapenems"),
          options = list(
            list(code = jsonlite::unbox("J01DH02")),
            list(code = jsonlite::unbox("J01DH56"))
          )),
        list(
          code = jsonlite::unbox("J01CR"),
          displayName = jsonlite::unbox("Combinations of penicillins, incl. beta-lactamase inhibitors"),
          displayShortName = jsonlite::unbox("Penicillins + beta-lactamase inhibitors"),
          displayDescription = jsonlite::unbox("This group comprises combinations of penicillins and/or beta-lactamase inhibitors. Combinations containing one penicillin and enzyme inhibitor are classified at different 5th levels according to the penicillin."),
          options = list(
            list(code = jsonlite::unbox("J01CR02")),
            list(code = jsonlite::unbox("J01CR05"))
          ))
      ))))

  optionGroupSets
}

generate_options <- function()
{
  list(
    list(
      code = jsonlite::unbox("J01CA04"),
      displayName = jsonlite::unbox("Amoxicillin"),
      displayFormName = jsonlite::unbox("Amoxicillin"),
      sortOrder = jsonlite::unbox(2),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01CA15"),
      displayName = jsonlite::unbox("Talampicillin"),
      displayFormName = jsonlite::unbox("Talampicillin"),
      sortOrder = jsonlite::unbox(222),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01DI03"),
      displayName = jsonlite::unbox("Faropenem"),
      displayFormName = jsonlite::unbox("Faropenem"),
      sortOrder = jsonlite::unbox(97),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01XA03"),
      displayName = jsonlite::unbox("Telavancin"),
      displayFormName = jsonlite::unbox("Telavancin"),
      sortOrder = jsonlite::unbox(227),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01XA02"),
      displayName = jsonlite::unbox("Teicoplanin"),
      displayFormName = jsonlite::unbox("Teicoplanin"),
      sortOrder = jsonlite::unbox(226),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01MA09"),
      displayName = jsonlite::unbox("Sparfloxacin"),
      displayFormName = jsonlite::unbox("Sparfloxacin"),
      sortOrder = jsonlite::unbox(187),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01CF04"),
      displayName = jsonlite::unbox("Oxacillin"),
      displayFormName = jsonlite::unbox("Oxacillin"),
      sortOrder = jsonlite::unbox(153),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01CF01"),
      displayName = jsonlite::unbox("Dicloxacillin"),
      displayFormName = jsonlite::unbox("Dicloxacillin"),
      sortOrder = jsonlite::unbox(88),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01DH02"),
      displayName = jsonlite::unbox("Meropenem"),
      displayFormName = jsonlite::unbox("Meropenem"),
      sortOrder = jsonlite::unbox(129),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01DH56"),
      displayName = jsonlite::unbox("Imipenem/Cilastatin/Relebactam"),
      displayFormName = jsonlite::unbox("Imipenem/Cilastatin/Relebactam"),
      sortOrder = jsonlite::unbox(114),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01CR02"),
      displayName = jsonlite::unbox("Amoxicillin/Clavulanic-acid"),
      displayFormName = jsonlite::unbox("Amoxicillin/Clavulanic-acid"),
      sortOrder = jsonlite::unbox(3),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01CR05"),
      displayName = jsonlite::unbox("Piperacillin/Tazobactam"),
      displayFormName = jsonlite::unbox("Piperacillin/Tazobactam"),
      sortOrder = jsonlite::unbox(165),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES"))
    ),
    list(
      code = jsonlite::unbox("J01MA17"),
      displayName = jsonlite::unbox("Prulifloxacin"),
      displayFormName = jsonlite::unbox("Prulifloxacin"),
      sortOrder = jsonlite::unbox(174),
      optionSet = list(code = jsonlite::unbox("NEOIPC_ANTIMICROBIAL_SUBSTANCES")))
  )
}

generate_metadata <- function(system, programId,
                              programStages, dataElements,
                              trackedEntityAttributes, countries,
                              testUnits, awareCategories,
                              atc5Categories, antimicrobials)
{
  metadata <- list()

  if(system)
    metadata <- c(metadata, list(system = generate_system()))

  if(programId || programStages || dataElements || trackedEntityAttributes)
    metadata <- c(metadata, list(programs = list(generate_program(programId, programStages, dataElements, trackedEntityAttributes))))

  if(countries || testUnits)
    metadata <- c(metadata, list(organisationUnitGroups = generate_organisationUnitGroups(countries, testUnits)))

  if(awareCategories || atc5Categories)
    metadata <- c(metadata, list(optionGroupSets = generate_optionGroupSets(awareCategories, atc5Categories)))

  if(antimicrobials)
    metadata <- c(metadata, list(options = generate_options()))

  jsonlite::toJSON(metadata)
}

rmd <- function(add_system = TRUE, add_programId = TRUE,
                add_programStages = TRUE, add_dataElements = TRUE,
                add_trackedEntityAttributes = TRUE, add_Countries = TRUE,
                add_testUnits = TRUE, add_awareCategories = TRUE,
                add_atc5Categories = TRUE, add_antimicrobials = TRUE)
{
  json_text <- generate_metadata(add_system, add_programId,
                                 add_programStages, add_dataElements,
                                 add_trackedEntityAttributes, add_Countries,
                                 add_testUnits, add_awareCategories,
                                 add_atc5Categories, add_antimicrobials)

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
    "Exactly one of `token`, `username`, or `session_id` must be supplied.")
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

  # eventTypes
  expect_equal(sort(as.character(metadata$eventTypes$name)), c("Admission", "Surgical Procedure"))

  # dataElements
  expect_equal(metadata$dataElements$id, c("Lwa9Jp5xSnR", "rvq4L9wWbwW", "AgBqfnnsUzd", "DTZ9HfILgnX"))
  expect_equal(metadata$dataElements$optionSet, c(NA, NA, "NEOIPC_ADMISSION_TYPES", NA))

  # trackedEntityAttributes
  expect_equal(metadata$trackedEntityAttributes$id, c("yQwpowV0o08", "E5OMg8BC8be"))
  expect_equal(metadata$trackedEntityAttributes$optionSet, c(NA, "R2yCnsqxamL"))

  # countries
  expect_equal(metadata$countries$code, ordered(c("CH", "DE")))
})
