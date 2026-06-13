#' Group EMS clinical values into a consistent two-tier hierarchy
#'
#' Lumps provider primary impressions (`lump_impressions()`) or causes of
#' injury (`lump_injury_causes()`) into a consistent two-tier clinical
#' taxonomy: `tier1` is a broad patient-type category (e.g. "Penetrating
#' Trauma", "Respiratory", "Cardiac - Chest Pain/ACS") and `tier2` is the
#' specific condition or mechanism (e.g. "Gunshot Wound", "COPD Exacerbation").
#'
#' Matching is keyed on whichever input you supply. When both are supplied,
#' the ICD-10 code match takes precedence (codes are stable across value-list
#' rewordings) and the description fills in where the code is missing or
#' unrecognized. Description matching is normalization-tolerant: case,
#' whitespace, the deactivated-item "x" prefix, and the glued
#' "Behavioral HealthSuicidal Ideation"-style values are all handled.
#'
#' Values that match nothing in the taxonomy return `"Other/Unmatched"` and a
#' warning lists the distinct unmatched values so the taxonomy (CSVs in
#' `data-raw/`) can be extended. Inputs that are `NA` (or both `NA`) return
#' `NA`.
#'
#' @param desc Character vector of description values (e.g.
#'   "Trauma: Gunshot Wound- Chest"). Optional if `code` is supplied.
#' @param code Character vector of ICD-10 codes (e.g. "S21.90XA"). Optional
#'   if `desc` is supplied. Must be the same length as `desc` when both are
#'   given.
#' @param tier Which tier to return: `"tier1"` (default, broad category) or
#'   `"tier2"` (specific condition/mechanism).
#'
#' @return A factor the same length as the input, with levels ordered by
#'   observed frequency in the source data and `"Other/Unmatched"` last.
#'
#' @examples
#' lump_impressions(desc = c(
#'   "Trauma: Gunshot Wound- Chest",
#'   "Respiratory: COPD Exacerbation",
#'   "Cardiac: Chest Pain (Not STEMI)"
#' ))
#'
#' lump_impressions(code = "I21.3", tier = "tier2")
#'
#' @seealso [impression_groups], [injury_cause_groups] for the underlying
#'   lookup tables; [separate_desc_code()] to tidy combined
#'   "Description (Code)" extract columns first.
#' @export
lump_impressions <- function(desc = NULL, code = NULL,
                             tier = c("tier1", "tier2")) {
  lump_clinical(desc, code, samuelr::impression_groups,
                match.arg(tier), "impression")
}

#' @rdname lump_impressions
#' @export
lump_injury_causes <- function(desc = NULL, code = NULL,
                               tier = c("tier1", "tier2")) {
  lump_clinical(desc, code, samuelr::injury_cause_groups,
                match.arg(tier), "injury cause")
}

lump_clinical <- function(desc, code, groups, tier, what) {
  if (is.null(desc) && is.null(code)) {
    stop("Supply `desc`, `code`, or both.", call. = FALSE)
  }
  if (!is.null(desc) && !is.null(code) && length(desc) != length(code)) {
    stop("`desc` and `code` must be the same length.", call. = FALSE)
  }
  n_out <- max(length(desc), length(code))
  levels_out <- levels(groups[[tier]])
  out <- rep(NA_character_, n_out)
  any_input <- rep(FALSE, n_out)

  if (!is.null(code)) {
    code_clean <- stringr::str_squish(code)
    code_clean[code_clean == ""] <- NA_character_
    any_input <- any_input | !is.na(code_clean)
    code_map <- dplyr::distinct(
      dplyr::arrange(
        dplyr::filter(groups, !is.na(.data$code)),
        dplyr::desc(.data$n)
      ),
      .data$code, .keep_all = TRUE
    )
    hit <- match(code_clean, code_map$code)
    out <- as.character(code_map[[tier]])[hit]
  }

  if (!is.null(desc)) {
    desc_norm <- .norm_clinical_desc(desc)
    any_input <- any_input | !is.na(desc_norm)
    desc_map <- dplyr::distinct(
      dplyr::arrange(
        dplyr::filter(groups, !is.na(.data$desc_norm)),
        dplyr::desc(.data$n)
      ),
      .data$desc_norm, .keep_all = TRUE
    )
    hit <- match(desc_norm, desc_map$desc_norm)
    fill <- is.na(out)
    out[fill] <- as.character(desc_map[[tier]])[hit][fill]
  }

  unmatched <- any_input & is.na(out)
  if (any(unmatched)) {
    shown <- unique(if (!is.null(desc)) desc[unmatched] else code[unmatched])
    shown <- shown[!is.na(shown)]
    warning(
      sum(unmatched), " ", what, " value(s) not in taxonomy, ",
      "assigned 'Other/Unmatched': ",
      paste(utils::head(shown, 10), collapse = "; "),
      if (length(shown) > 10) " ..." else "",
      call. = FALSE
    )
    out[unmatched] <- "Other/Unmatched"
  }

  factor(out, levels = levels_out)
}

# Canonical normalizer for matching clinical descriptions. Absorbs the
# wrinkles seen in DFR ImageTrend value lists so equivalent spellings collapse
# to one key:
#   - deactivated list items carry a leading "x" ("xHeat Related-..." )
#   - some exports inject a C1 control char (e.g. U+0097, a Windows-1252
#     em-dash that lost its encoding) where a separator belongs
#   - the same value appears glued ("HealthSuicidal"), dashed, or colon-
#     separated across versions ("Cardiac:" vs "Cardiac-")
# Used by both lump_clinical() at runtime and data-raw/clinical_taxonomy.R at
# build time (sourced via load_all), so the stored key and the lookup key can
# never drift.
.norm_clinical_desc <- function(x) {
  x <- stringr::str_replace(x, "^x(?=[A-Z])", "")
  x <- stringr::str_replace_all(x, "(?<=[a-z])(?=[A-Z])", " ")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  x <- stringr::str_squish(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

#' Separate combined "Description (Code)" columns into tidy columns
#'
#' ImageTrend report extracts often emit a single column combining a
#' description with its code in trailing parentheses — e.g.
#' `"Fall from Standing position (W01)"` — and "List" variants pack multiple
#' such entries into one cell separated by commas:
#' `"Fall off Toilet (W18.1), Fall from Standing position (W01)"`.
#'
#' This splits a combined column into `<col>_desc` and `<col>_code` columns,
#' expanding multi-entry list cells into multiple rows (1:M). Inner
#' parentheses in descriptions are preserved — only the final parenthetical
#' is treated as the code (`"Bite (animal)(W55.81)"` parses correctly, with
#' or without a space before the code). Empty codes (`"Not Applicable ()"`)
#' become `NA`.
#'
#' @param data A data frame.
#' @param col Name of the combined column, as a string.
#' @param remove If `TRUE` (default), drop the original combined column.
#'
#' @return The data frame with `<col>_desc` and `<col>_code` character
#'   columns. Rows with multi-entry cells are duplicated, one row per entry;
#'   all other columns are carried along unchanged.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:2,
#'   cause = c(
#'     "Fall from Standing position (W01)",
#'     "Assault by blunt object (Y00), Contact with blunt object (Y29)"
#'   )
#' )
#' separate_desc_code(df, "cause")
#'
#' @export
separate_desc_code <- function(data, col, remove = TRUE) {
  stopifnot(is.data.frame(data), col %in% names(data))
  vals <- as.character(data[[col]])
  parts <- stringr::str_split(vals, "(?<=\\))\\s*,\\s*")
  lens <- lengths(parts)
  out <- data[rep(seq_len(nrow(data)), lens), , drop = FALSE]
  flat <- unlist(parts, use.names = FALSE)
  m <- stringr::str_match(flat, "^(.*?)\\s*\\(([^()]*)\\)\\s*$")
  desc <- dplyr::coalesce(m[, 2], stringr::str_squish(flat))
  code <- m[, 3]
  code[is.na(code) | code == ""] <- NA_character_
  out[[paste0(col, "_desc")]] <- desc
  out[[paste0(col, "_code")]] <- code
  if (remove) out[[col]] <- NULL
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
