#' Two-tier clinical taxonomy for EMS provider primary impressions
#'
#' Lookup table mapping every provider primary impression value observed in
#' DFR ImageTrend data (eSituation.11) to a consistent two-tier clinical
#' hierarchy. Derived from a 71,854-row extract (2026-06); curated source
#' lives in `data-raw/impression_taxonomy.csv`.
#'
#' @format A tibble with one row per distinct (code, description) pair:
#' \describe{
#'   \item{code}{ICD-10 code as recorded, including ImageTrend custom codes
#'     (`itICD.*`) and the NEMSIS "Not Applicable" code `7701001`.}
#'   \item{desc}{Verbatim description, including historical value-list
#'     variants and deactivated "x"-prefixed items.}
#'   \item{desc_norm}{Normalized description used for matching (lowercased,
#'     squished, "x" prefix stripped).}
#'   \item{tier1}{Broad clinical category (factor, frequency-ordered), e.g.
#'     "Trauma - Blunt/Other", "Respiratory", "Penetrating Trauma".}
#'   \item{tier2}{Specific condition (factor), e.g. "Gunshot Wound",
#'     "COPD Exacerbation", "Hypoglycemia".}
#'   \item{n}{Frequency in the source extract (for level ordering and
#'     collision resolution).}
#' }
#' @seealso [lump_impressions()]
"impression_groups"

#' Two-tier mechanism taxonomy for EMS causes of injury
#'
#' Lookup table mapping cause-of-injury values observed in DFR ImageTrend
#' data (eInjury.01) to a two-tier mechanism hierarchy (e.g. tier1
#' "Penetrating Trauma - Firearm", tier2 "GSW - assault/intentional").
#' Derived from the same extract as [impression_groups]; curated source
#' lives in `data-raw/injury_cause_taxonomy.csv`.
#'
#' @format A tibble with the same structure as [impression_groups].
#' @seealso [lump_injury_causes()]
"injury_cause_groups"
