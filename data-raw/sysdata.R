## code to prepare `sysdata` dataset goes here
##
## NOTE on URLs: these point at a personal Brar fork branch
## (Surveillance-Toolkit/ReferenceReport) that carries the 22-column
## pathogen concept CSV this script's `col_types` expects. The canonical
## NeoIPC/Surveillance-Toolkit repository's CSV at
## metadata/common/infectious-agents/ has a different (slimmer) column
## shape and a different path, and the full taxonomy/concept schema is
## still being upstreamed (the YAML-based `NeoIPC-Infectious-Agents.yaml`
## supersedes both CSVs once the package's reader is ported in a later
## upstreaming PR). Pinning to the canonical NeoIPC URL today would
## break the `col_types` contract; leaving the Brar URL preserves
## chronological reproducibility for now. The generated `R/sysdata.rda`
## binary is what the package actually uses at runtime — this script is
## developer-only documentation of how that binary was produced.

internal_pathogen_concepts <- readr::read_csv(
  "https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/ReferenceReport/metadata/common/pathogens/NeoIPC-Pathogen-Concepts.csv",
  col_types = "icffclfllllliiiiiiiiii")
internal_pathogen_synonyms <- readr::read_csv(
  "https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/ReferenceReport/metadata/common/pathogens/NeoIPC-Pathogen-Synonyms.csv",
  col_types = "icfci")

usethis::use_data(internal_pathogen_concepts, internal_pathogen_synonyms, internal = TRUE, overwrite = TRUE, compress = "xz")
