get_pathogen_list <- function()
{
  pc <- internal_pathogen_concepts |>
    dplyr::rename("name" = "concept") |>
    dplyr::mutate(synonym_for = rlang::na_int)

  not_listed <- pc |>
    dplyr::slice_head()

  rest <- pc |>
    dplyr::filter(.data$id != 0) |>
    dplyr::bind_rows(
      internal_pathogen_synonyms |>
        dplyr::inner_join(
          internal_pathogen_concepts |>
            dplyr::select(!c("concept","concept_source","concept_id")),
          dplyr::join_by("synonym_for" == "id")) |>
        dplyr::relocate("concept_type", .before = "concept_source") |>
        dplyr::relocate("synonym_for", .after = "show_coli_r") |>
        dplyr::rename("name" = "synonym")) |>
    dplyr::arrange(.data$name)

  dplyr::bind_rows(not_listed, rest)
}

#' Gets the available taxonomic information for pathogens
#'
#' @param ids A vector containing the ids of the pathogens to get the taxonomic
#'  information for
#'
#' @returns A tibble containing the taxonomic information
#' @export
get_pathogen_taxonomy <- function(ids = NULL)
{
  internal_pathogen_concepts |>
    dplyr::filter(is.null(ids) | .data$id %in% ids) |>
    dplyr::mutate(
      input_id = .data$id,
      input_name = .data$concept,
      output_id = .data$id,
      .keep = "none") |>
    dplyr::union(
      internal_pathogen_synonyms |>
        dplyr::filter(is.null(ids) | .data$id %in% ids) |>
        dplyr::mutate(
          input_id = .data$id,
          input_name = .data$synonym,
          output_id = .data$synonym_for,
          .keep = "none")) |>
    dplyr::inner_join(
      internal_pathogen_concepts |>
        dplyr::select("id","concept","concept_type","is_cc","coagulase",
                      "species","genus","family","order","class","subdivision",
                      "phylum","subkingdom","kingdom","domain"),
      dplyr::join_by("output_id" == "id")) |>
    dplyr::rename("output_name" = "concept") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("species" == "id")) |>
    dplyr::mutate(species = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("genus" == "id")) |>
    dplyr::mutate(genus = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("family" == "id")) |>
    dplyr::mutate(family = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("order" == "id")) |>
    dplyr::mutate(order = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("class" == "id")) |>
    dplyr::mutate(class = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("subdivision" == "id")) |>
    dplyr::mutate(subdivision = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("phylum" == "id")) |>
    dplyr::mutate(phylum = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("subkingdom" == "id")) |>
    dplyr::mutate(subkingdom = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("kingdom" == "id")) |>
    dplyr::mutate(kingdom = .data$concept, .keep = "unused") |>
    dplyr::left_join(
      internal_pathogen_concepts |>
        dplyr::select("id", "concept"),
      dplyr::join_by("domain" == "id")) |>
    dplyr::mutate(domain = .data$concept, .keep = "unused") |>
    dplyr::mutate(
      concept_type = as.factor(as.character(.data$concept_type)),
      coagulase = as.factor(as.character(.data$coagulase)),
      species = as.factor(.data$species),
      genus = as.factor(.data$genus),
      family = as.factor(.data$family),
      order = as.factor(.data$order),
      class = as.factor(.data$class),
      subdivision = as.factor(.data$subdivision),
      phylum = as.factor(.data$phylum),
      subkingdom = as.factor(.data$subkingdom),
      kingdom = as.factor(.data$kingdom),
      domain = as.factor(.data$domain))
}
