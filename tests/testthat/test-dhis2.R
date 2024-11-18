test_that("dhis2_connection_options defaults", {
  expect_equal(
    dhis2_connection_options("test_token")$base_url,
    "https://localhost/api")
})

test_that("dhis2_connection_options defaults", {
  expect_equal(
    dhis2_connection_options(
      token = "test_token",
      scheme = "http",
      hostname = "testhost",
      port = 8080,
      path = "/api/41")$base_url,
    "http://testhost:8080/api/41")
})

test_that("dhis2_connection_options fails if token and username are set", {
  expect_error(
    dhis2_connection_options(
      token = "test_token",
      username = "admin"),
    "Exactly one of `token` or `username` must be supplied.")
})

test_that("read_metadata reads system", {
  metadata <- read_metadata('{"system":{"date":"2024-11-08T14:06:41.216+0000","id":"72c2bd70-573a-4d69-8bc3-f7bb431bdc23","rev":"3fcd748","version":"2.40.3.2"}}')

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
})

test_that("read_metadata fails if system missing", {
  expect_error(
    read_metadata('{"programs":[{"id": "D8mSSpOpsKj"}]}'),
    class = "neoipcr_metadata_system_missing")
})

test_that("read_metadata reads programId", {
  metadata <- read_metadata(
      '{"system":{"date":"2024-11-08T14:06:41.216+0000","id":"72c2bd70-573a-4d69-8bc3-f7bb431bdc23","rev":"3fcd748","version":"2.40.3.2"},"programs":[{"id": "D8mSSpOpsKj"}]}')

  expect_equal(metadata$programId, "D8mSSpOpsKj")
})

test_that("read_metadata reads programStages", {
  metadata <- read_metadata(
    '
{
    "programs": [
        {
            "programStages": [
                {
                    "displayDescription": "The surveillance event where the patient admission is recorded.",
                    "displayFormName": "Admission",
                    "displayName": "Admission",
                    "id": "YGowWPumDia",
                    "name": "Admission",
                    "programStageDataElements": [
                        {
                            "dataElement": {
                                "code": "NEOIPC_ADMISSION_TYPE",
                                "displayDescription": "Describes if the infant was born in your hospital or if it was admitted after birth and if so, how long after birth.",
                                "displayFormName": "Admission type",
                                "displayName": "NeoIPC Admission type",
                                "displayShortName": "NeoIPC Adm. type",
                                "id": "AgBqfnnsUzd",
                                "optionSet": {
                                    "id": "l6HnyhcwF28"
                                },
                                "valueType": "INTEGER_POSITIVE"
                            }
                        }
                    ]
                },
                {
                    "displayDescription": "The surveillance event where a surgical procedure performed on a patient is recorded.",
                    "displayFormName": "Surgical Procedure",
                    "displayName": "Surgical Procedure",
                    "id": "BHWwaviIFvy",
                    "name": "Surgical Procedure",
                    "programStageDataElements": [
                        {
                            "dataElement": {
                                "code": "NEOIPC_SURGERY_WOUND_CLASS",
                                "displayDescription": "An assessment of the degree of contamination of a surgical wound at the time of the surgical procedure. Wound class is assigned by a person involved in the surgical procedure (for example, surgeon, circulating nurse, etc.). The four wound classifications are: Clean, Clean-Contaminated, Contaminated, and Infected.",
                                "displayFormName": "Wound class",
                                "displayName": "NeoIPC Surgery Wound class",
                                "displayShortName": "NeoIPC Surg. Wound class",
                                "id": "Yu7iGKrtyeL",
                                "optionSet": {
                                    "id": "GfBMlp6BqeM"
                                },
                                "valueType": "INTEGER_POSITIVE"
                            }
                        }
                    ]
                }
            ]
        }
    ],
    "system": {
        "date": "2024-11-09T17:11:41.400+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(metadata$programStages$name, c("Admission", "Surgical Procedure"))
})

test_that("read_metadata reads dataElements", {
  metadata <- read_metadata(
    '
{
    "programs": [
        {
            "id": "D8mSSpOpsKj",
            "programStages": [
                {
                    "displayName": "Admission",
                    "id": "YGowWPumDia",
                    "name": "Admission",
                    "programStageDataElements": [
                        {
                            "dataElement": {
                                "code": "NEOIPC_ADMISSION_LOS",
                                "displayDescription": "Length of Stay (should always be 0). For Reporting purpose only",
                                "displayFormName": "Day of occurrence after admission",
                                "displayName": "NeoIPC Admission Length of Stay",
                                "displayShortName": "NeoIPC Adm. LOS",
                                "id": "Lwa9Jp5xSnR",
                                "valueType": "INTEGER_ZERO_OR_POSITIVE"
                            }
                        },
                        {
                            "dataElement": {
                                "code": "NEOIPC_ADMISSION_DOL",
                                "displayDescription": "For infants that have not been delivered in your own hospital, record the infant\'s day of life on the day of admission (day of birth = day of life 1. The next day, starting at 00:00, is the second day of life.)",
                                "displayFormName": "Admission on day of life",
                                "displayName": "NeoIPC Admission on day of life",
                                "displayShortName": "NeoIPC Adm. DOL",
                                "id": "rvq4L9wWbwW",
                                "valueType": "INTEGER_POSITIVE"
                            }
                        },
                        {
                            "dataElement": {
                                "code": "NEOIPC_ADMISSION_TYPE",
                                "displayDescription": "Describes if the infant was born in your hospital or if it was admitted after birth and if so, how long after birth.",
                                "displayFormName": "Admission type",
                                "displayName": "NeoIPC Admission type",
                                "displayShortName": "NeoIPC Adm. type",
                                "id": "AgBqfnnsUzd",
                                "optionSet": {
                                    "id": "l6HnyhcwF28"
                                },
                                "valueType": "INTEGER_POSITIVE"
                            }
                        }
                    ]
                },
                {
                    "displayName": "Surgical Procedure",
                    "id": "BHWwaviIFvy",
                    "name": "Surgical Procedure",
                    "programStageDataElements": [
                        {
                            "dataElement": {
                                "code": "NEOIPC_SURGERY_EMERGENCY_PROCEDURE",
                                "displayDescription": "Yes: A procedure that is documented per the facility’s protocol to be an emergency or urgent procedure. No: The intervention is initiated and performed in a planned manner Unknown: No information available.",
                                "displayFormName": "Emergency procedure",
                                "displayName": "NeoIPC Surgery Emergency procedure",
                                "displayShortName": "NeoIPC Surg. Emergency",
                                "id": "DTZ9HfILgnX",
                                "valueType": "BOOLEAN"
                            }
                        }
                    ]
                }
            ]
        }
    ],
    "system": {
        "date": "2024-11-09T13:38:16.673+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(metadata$dataElements$id, c("Lwa9Jp5xSnR", "rvq4L9wWbwW", "AgBqfnnsUzd", "DTZ9HfILgnX"))
  expect_equal(metadata$dataElements$optionSet_id, c(NA, NA, "l6HnyhcwF28", NA))
})

test_that("read_metadata reads trackedEntityAttributes", {
  metadata <- read_metadata(
    '
{
    "programs": [
        {
            "programTrackedEntityAttributes": [
                {
                    "trackedEntityAttribute": {
                        "code": "NEOIPC_PATIENT_ID",
                        "displayDescription": "Use this identifier to uniquely identify a patient in the system. Ideally use an unique random string of characters. If have a requirement to identify a patient you have entered here, you can use this identifier as the NeoIPC key for pseudonymization. NEVER use an identifier that is used anywhere else and that you do not fully control (e.g. do NOT use the patient id from your hospital information system).",
                        "displayFormName": "NeoIPC patient identifier",
                        "displayName": "NeoIPC Patient Identifier",
                        "displayShortName": "NeoIPC Pat. Id",
                        "id": "yQwpowV0o08",
                        "valueType": "TEXT"
                    }
                },
                {
                    "trackedEntityAttribute": {
                        "code": "NEOIPC_TEA_SEX",
                        "displayDescription": "Typically the phenotypic sex of the patient. If sex cannot be determined from the patient\'s phenotype or genotype, or if the genotype is neither XX nor XY, it is considered undetermined for purposes of surveillance.",
                        "displayFormName": "Sex",
                        "displayName": "NeoIPC Patient Sex",
                        "displayShortName": "NeoIPC Pat. Sex",
                        "id": "E5OMg8BC8be",
                        "optionSet": {
                            "id": "R2yCnsqxamL"
                        },
                        "valueType": "LETTER"
                    }
                }
            ]
        }
    ],
    "system": {
        "date": "2024-11-09T13:38:16.673+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(metadata$trackedEntityAttributes$id, c("yQwpowV0o08", "E5OMg8BC8be"))
  expect_equal(metadata$trackedEntityAttributes$optionSet_id, c(NA, "R2yCnsqxamL"))
})

test_that("read_metadata reads countries", {
  metadata <- read_metadata(
    '
{
    "organisationUnitGroups": [
        {
            "code": "COUNTRY",
            "organisationUnits": [
                {
                    "code": "CH",
                    "displayName": "Switzerland",
                    "displayShortName": "Switzerland"
                },
                {
                    "code": "DE",
                    "displayName": "Germany",
                    "displayShortName": "Germany"
                }
            ]
        }
    ],
    "system": {
        "date": "2024-11-09T16:17:50.893+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(metadata$countries$code, c("CH", "DE"))
})

test_that("read_metadata reads hospitals", {
  metadata <- read_metadata(
    '
{
    "organisationUnits": [
        {
            "displayName": "Test department 1",
            "displayShortName": "Test 1",
            "id": "wcFyerorAmG",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "TEST_UNITS",
                "displayDescription": "A few test units to evaluate DHIS2 as NeoIPC data collection platform.",
                "displayName": "Test Units",
                "displayShortName": "Test Units",
                "id": "aCbeNKDGVks",
                "parent": {
                    "code": "NEOIPC"
                }
            }
        },
        {
            "comment": "A `real` department",
            "displayDescription": "This simulates a real neo department",
            "displayName": "Test department 2",
            "displayShortName": "Test 2",
            "id": "oSpSLxOVIxE",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "DE_TEST_PARENT",
                "comment": "This simulates a real hospital",
                "displayName": "Test hospital 1",
                "displayShortName": "Test hosp. 1",
                "geometry": {
                    "coordinates": [
                        13.37819,
                        52.523628
                    ],
                    "type": "Point"
                },
                "id": "oOpnrFqrBhJ",
                "parent": {
                    "code": "DE"
                }
            }
        },
        {
            "comment": "A `real` department",
            "displayDescription": "This simulates a real neo department",
            "displayName": "Test department 3",
            "displayShortName": "Test 3",
            "id": "yTKmXIhJQkP",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "DE_TEST_PARENT",
                "comment": "This simulates a real hospital",
                "displayName": "Test hospital 1",
                "displayShortName": "Test hosp. 1",
                "geometry": {
                    "coordinates": [
                        13.37819,
                        52.523628
                    ],
                    "type": "Point"
                },
                "id": "oOpnrFqrBhJ",
                "parent": {
                    "code": "DE"
                }
            }
        },
        {
            "displayName": "Test department 4",
            "displayShortName": "Test 4",
            "id": "SEnzmeiCYis",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "GR_TEST_PARENT",
                "displayName": "Test hospital 2",
                "displayShortName": "Test hosp. 2",
                "geometry": {
                    "coordinates": [
                        20.840438,
                        39.621562
                    ],
                    "type": "Point"
                },
                "id": "TKfIqRxsPuF",
                "parent": {
                    "code": "EL"
                }
            }
        }
    ],
    "system": {
        "date": "2024-11-10T16:14:26.354+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(metadata$hospitals |> dplyr::arrange(code) |> dplyr::pull(code), c("DE_TEST_PARENT", "GR_TEST_PARENT"))
})

test_that("read_metadata reads departments", {
  metadata <- read_metadata(
    '
{
    "organisationUnits": [
        {
            "displayName": "Test department 1",
            "displayShortName": "Test 1",
            "id": "wcFyerorAmG",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "TEST_UNITS",
                "displayDescription": "A few test units to evaluate DHIS2 as NeoIPC data collection platform.",
                "displayName": "Test Units",
                "displayShortName": "Test Units",
                "id": "aCbeNKDGVks",
                "parent": {
                    "code": "NEOIPC"
                }
            }
        },
        {
            "comment": "A `real` department",
            "displayDescription": "This simulates a real neo department",
            "displayName": "Test department 2",
            "displayShortName": "Test 2",
            "id": "oSpSLxOVIxE",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "DE_TEST_PARENT",
                "comment": "This simulates a real hospital",
                "displayName": "Test hospital 1",
                "displayShortName": "Test hosp. 1",
                "geometry": {
                    "coordinates": [
                        13.37819,
                        52.523628
                    ],
                    "type": "Point"
                },
                "id": "oOpnrFqrBhJ",
                "parent": {
                    "code": "DE"
                }
            }
        },
        {
            "comment": "A `real` department",
            "displayDescription": "This simulates a real neo department",
            "displayName": "Test department 3",
            "displayShortName": "Test 3",
            "id": "yTKmXIhJQkP",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "DE_TEST_PARENT",
                "comment": "This simulates a real hospital",
                "displayName": "Test hospital 1",
                "displayShortName": "Test hosp. 1",
                "geometry": {
                    "coordinates": [
                        13.37819,
                        52.523628
                    ],
                    "type": "Point"
                },
                "id": "oOpnrFqrBhJ",
                "parent": {
                    "code": "DE"
                }
            }
        },
        {
            "displayName": "Test department 4",
            "displayShortName": "Test 4",
            "id": "SEnzmeiCYis",
            "openingDate": "2024-11-10T00:00:00.000",
            "parent": {
                "code": "GR_TEST_PARENT",
                "displayName": "Test hospital 2",
                "displayShortName": "Test hosp. 2",
                "geometry": {
                    "coordinates": [
                        20.840438,
                        39.621562
                    ],
                    "type": "Point"
                },
                "id": "TKfIqRxsPuF",
                "parent": {
                    "code": "EL"
                }
            }
        }
    ],
    "system": {
        "date": "2024-11-10T16:14:26.354+0000",
        "id": "f78bc2df-35f5-46f5-9500-3944dba1b01d",
        "rev": "3fcd748",
        "version": "2.40.3.2"
    }
}')

  expect_equal(
    metadata$departments |> dplyr::arrange(displayName) |> dplyr::pull(displayName),
    c("Test department 1", "Test department 2", "Test department 3",
      "Test department 4"))
})

# ToDo: get_users,
