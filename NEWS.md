<!--
Version headings follow R's convention — `# neoipcr <version>`, one H1 per released
version — rather than the `## [x.y.z]` shape the other NeoIPC products use, because
this is a `NEWS.md` that R tooling reads.

The release workflow extracts the section whose heading matches the version in
DESCRIPTION and publishes it as the GitHub Release body, so a release cannot be cut
for a version this file does not describe. Rename `(development version)` to the
version being released in the same commit that bumps DESCRIPTION.
-->

# neoipcr (development version)

* Nothing released yet under this heading.

# neoipcr 0.0.0.9000

First dev-snapshot tag.

* The package is pre-alpha. The first CRAN release is planned as `0.1.0`; until then it carries the
  `.9000` development suffix, and `0.1.0` stays reserved for that release rather than being spent on
  a snapshot.
* Immutable dev-snapshot tags (`v0.0.0.9000`, `v0.0.0.9001`, …) mark the commits the NeoIPC reporting
  container pins, so a built image records exactly which neoipcr snapshot it shipped.
