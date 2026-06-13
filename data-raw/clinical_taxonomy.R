# Builds the clinical grouping lookup tables exported as
# `impression_groups` and `injury_cause_groups`.
#
# Source CSVs in this directory are the curated two-tier taxonomies derived
# from a 71,854-row DFR ImageTrend extract (2026-06). To extend a taxonomy
# (new value list items), add rows to the CSV and re-run this script.
#
# The matching key (desc_norm) is built with the package's own
# .norm_clinical_desc() via load_all(), so the stored key and the runtime
# lookup key are guaranteed identical — there is no second copy to keep in
# sync.

library(dplyr)
library(readr)

devtools::load_all(quiet = TRUE)
norm_desc <- samuelr:::.norm_clinical_desc

build_groups <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  tier1_levels <- raw |>
    count(tier1, wt = n, sort = TRUE) |>
    pull(tier1)
  tier2_levels <- raw |>
    count(tier2, wt = n, sort = TRUE) |>
    pull(tier2)
  raw |>
    mutate(
      desc_norm = norm_desc(desc),
      tier1 = factor(tier1, levels = c(tier1_levels, "Other/Unmatched")),
      tier2 = factor(tier2, levels = c(tier2_levels, "Other/Unmatched"))
    ) |>
    select(code, desc, desc_norm, tier1, tier2, n)
}

impression_groups <- build_groups("data-raw/impression_taxonomy.csv")
injury_cause_groups <- build_groups("data-raw/injury_cause_taxonomy.csv")

# A code (or normalized description) must map to exactly one tier pair;
# resolve any collision toward the highest-frequency assignment and report it.
check_collisions <- function(groups, key) {
  collisions <- groups |>
    filter(!is.na(.data[[key]])) |>
    distinct(.data[[key]], tier1, tier2) |>
    count(.data[[key]]) |>
    filter(n > 1)
  if (nrow(collisions) > 0) {
    warning(
      "Ambiguous ", key, " mappings (resolved by max n): ",
      paste(collisions[[key]], collapse = ", ")
    )
  }
}
check_collisions(impression_groups, "code")
check_collisions(impression_groups, "desc_norm")
check_collisions(injury_cause_groups, "code")
check_collisions(injury_cause_groups, "desc_norm")

usethis::use_data(impression_groups, injury_cause_groups, overwrite = TRUE)
