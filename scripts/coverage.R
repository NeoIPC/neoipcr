# Local coverage report — run from an R session at the package root, or via:
#   Rscript scripts/coverage.R          # HTML report, opens browser
#   Rscript scripts/coverage.R quiet    # HTML report, no browser
#
# Output: coverage.html in the package root (gitignored).

if (!requireNamespace("covr", quietly = TRUE))
  stop("Package 'covr' is required. Install with: install.packages('covr')")

args <- commandArgs(trailingOnly = TRUE)
open_report <- !("quiet" %in% args)

cov <- covr::package_coverage(path = ".", quiet = FALSE, clean = TRUE)

print(cov)

covr::report(cov, file = "coverage.html", browse = open_report)

message("Coverage report written to coverage.html")
