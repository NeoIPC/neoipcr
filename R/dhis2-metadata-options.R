read_metadata_options <- function(metadata)
{
  options <- metadata |>
    purrr::pluck("options")

  if(rlang::is_null(options))
    rlang::abort("Invalid DHIS2 metadata. The options list is missing.",
                 "neoipcr_metadata_options_missing")

  options |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider("optionSet", names_sep = "_") |>
    dplyr::arrange(.data$optionSet_code, .data$sortOrder)
}

read_metadata_admissionTypes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_ADMISSION_TYPES") |>
  convert_metadata_options()

read_metadata_asaScores <- function(options)
  options |>
  filter_metadata_options("NEOIPC_ASA_SCORE") |>
  convert_metadata_options()

read_metadata_sepsisDeviceAssociation <- function(options)
  options |>
  filter_metadata_options("NEOIPC_BSI_DEVICE_ASS") |>
  convert_metadata_options()

read_metadata_sepsis_pathogen_sources <- function(options)
  options |>
  filter_metadata_options("NEOIPC_BSI_PATHOGEN_RECOVERED_FROM") |>
  convert_metadata_options()

read_metadata_deliveryModes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_DELIVERY_MODES") |>
  convert_metadata_options()

read_metadata_pneumoniaDeviceAssociation <- function(options)
  options |>
  filter_metadata_options("NEOIPC_HAP_DEVICE_ASS") |>
  convert_metadata_options()

read_metadata_pneumonia_pathogen_sources <- function(options)
  options |>
  filter_metadata_options("NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES") |>
  convert_metadata_options()

read_metadata_sexes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SEX_VALUES") |>
  convert_metadata_options()

read_metadata_ssiTypes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SSI_TYPE") |>
  convert_metadata_options()

read_metadata_surveillanceEndReasons <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SURVEILLANCE_END_REASON") |>
  convert_metadata_options()

read_metadata_woundClasses <- function(options)
  options |>
  filter_metadata_options("NEOIPC_WOUND_CLASSES") |>
  convert_metadata_options()

read_metadata_testResults <- function(options)
  options |>
  filter_metadata_options("NEOIPC_YES_NO_NOT_TESTED") |>
  convert_metadata_options()

read_metadata_surveillanceResults <- function(options)
  options |>
  filter_metadata_options("NEOIPC_YES_NO_NO_FOLLOWUP") |>
  convert_metadata_options()

filter_metadata_options <- function(options, filter)options |>
  dplyr::filter(.data$optionSet_code == filter) |>
  dplyr::select(!"optionSet_code")

convert_metadata_options <- function(options, ordered = FALSE)
  options |>
  dplyr::mutate(
    code = factor(.data$code, levels = unique(.data$code), ordered = ordered),
    displayName = factor(.data$displayName, levels = unique(.data$displayName), ordered = ordered),
    displayFormName = factor(.data$displayFormName, levels = unique(.data$displayFormName), ordered = ordered))
