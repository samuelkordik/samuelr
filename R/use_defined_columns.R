#' Use Defined Column Names
#'
#' Processes a dataframe to use pre-defined column names and ensure these names
#' are present in the dataframe.
#'
#' This function starts with a source dataframe and a list of desired defined
#' column names. The dataframe is checked to ensure required defined column names
#' are present and (optionally) to parse or set specific column data types. The
#' dataframe is then returned with the defined columns present using the defined
#' column names instead of the original names, when applicable.
#'
#' Defined column names are matched to the desired original column names by using
#' a regex pattern. This allows for matching multiple variations of names in the
#' source dataframe, provided they have some amount of consistency.
#'
#' This is useful when designing a function to process or analyze data that may
#' have variations in naming conventions and eliminates the need to rigidly
#' enforce a set of naming conventions on end users of the function.
#'
#' # Defining Column Names
#' The key to making this function work is using a regex pattern to match source
#' column names to the desired defined column name. These patterns should ideally
#' only match a single column name in the source dataframe; if they don't, the
#' first column name to match will be used in the output (and a warning will be
#' sent).
#'
#' # Validating column presence
#' If a defined column name is marked as "required" (the default), then an error
#' will be thrown if no matches for this column are found in the source dataframe.
#'
#' # Column datatypes
#' Optionally, a list of desired datatypes is provided and the defined columns
#' are converted into this datatype. Errors will be thrown if parsing into this
#' datatype fail.
#'
#' The available specifications are:
#' - col_logical: logical, contains only TRUE or FALSE
#' - col_integer: Integers.
#' - col_double: Doubles.
#' - col_character: Characters
#' - col_factor: Fixed set of values.
#' - col_date: Date type (with day resol  ution)
#' - col_datetime: POSIXct
#' - col_default: Keeps the original column type.
#'
#' @param data Source dataframe
#' @param defined_colnames A list of column names to be used in the output.
#' @param defined_colname_patterns A list of matching regex patterns to be used
#'  to identify matching column names in the source dataframe.
#' @param column_required (optional) whether the defined column must be present
#'  or not in the source dataframe. Defaults to "required" for all defined columns.
#' @param column_type (optional) What type each defined column should have.
#'  Defaults to "col_default", which just uses the type present in the source
#'  dataframe for the matching dataframe.
#' @param keep Keep non-defined columns in output. Defaults to TRUE.
#'
#' @returns .data dataframe with defined column names used.
#' @export
#'
use_defined_columns <- function(data,
                                defined_colnames,
                                defined_colname_patterns,
                                column_required = c("required",
                                                    "optional"),
                                column_type = c("col_logical",
                                                "col_integer",
                                                "col_double",
                                                "col_character",
                                                "col_factor",
                                                "col_date",
                                                "col_datetime",
                                                "col_default"),
                                keep = TRUE
                                ) {

  ############# INPUT VALIDATION ###############

  # Check column lengths
  attempt::stop_if_not(length(defined_colname_patterns) == length(defined_colnames),
                       msg = "defined_colnames must be same length as defined_colname_patterns")

  # Check for column_required, expand if necessary
  if(missing(column_required)) {
    column_required <- replicate(length(defined_colnames),
                                 "required")
  } else {

    attempt::stop_if_not(
      vapply(column_required, \(x) x %in% c("required", "optional"),  logical(1)),
      msg = "column_types must be either required or optional."
    )

    if(length(column_required == 1)) {
      column_required <- replicate(length(defined_colnames),
                                   column_required)
    } else {
      attempt::stop_if_not(
        length(column_required) == length(defined_colnames),
        msg = "column_required must be either length 1 or length of defined_colnames"
      )
    }
  }

  # Check for column_type, fill if necessary
  c("col_logical",
    "col_integer",
    "col_double",
    "col_character",
    "col_factor",
    "col_date",
    "col_datetime",
    "col_default"
  ) -> col_type_opts

  if(missing(column_type)) {
    column_type <- replicate(length(defined_colnames),
                             "col_default"
                             )
  }

  attempt::stop_if_none(vapply(column_type, is.character, logical(1)),
                        msg = "column_required specifications must all be character.")

  attempt::stop_if_none(
    vapply(column_type, \(x) x %in% col_type_opts,  logical(1)),
    msg = "column_types must be in defined option list"
  )

  ################ CHECKING FOR COLUMN PRESENCE ##################

  # Assemble test tibble

  column_definitions <- tibble::tibble(defined_colnames,
                                       defined_colname_patterns)

  tibble::tibble(colname = stringr::str_to_lower(colnames(data)),
                  column_definitions |> tidyr::pivot_wider(names_from = `defined_colnames`,
                                                           values_from = `defined_colname_patterns`)
  ) -> cols_to_check

  g_defined_cols <- dplyr::n_distinct(column_definitions$defined_colnames)
  g_source_cols <-  length(colnames(data))
  cli::cli_inform("Checking for {g_defined_cols} defined columns in {g_source_cols} columns in source dataframe.")

  # Check for columns
  cols_to_check |>
    dplyr::mutate(dplyr::across(-colname,
                                ~ stringr::str_detect(colname, .x)
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(matched_column_n = sum(dplyr::c_across(dplyr::where(is.logical)),na.rm=TRUE)) |>
    dplyr::mutate(unmatched_column = matched_column_n < 1 | is.na(matched_column_n),
                  overmatched_column = matched_column_n > 1,
                  matched_column = matched_column_n == 1) |>
    dplyr::ungroup() |>
    dplyr::summarize(
      dplyr::across(dplyr::where(is.logical), ~ sum(.x, na.rm=TRUE)
      )
    ) -> checked_cols

  # FINISH IT
  dplyr::bind_cols(
    checked_cols |>
      dplyr::select(-unmatched_column,
                    -overmatched_column,
                    -matched_column) |>
      tidyr::pivot_longer(tidyr::everything(),
                          names_to = "variable") |>
      dplyr::mutate(missing = value < 1 | is.na(value),
                    multiple = value > 1
      ) |>
      dplyr::summarize(matched_columns = sum(value, na.rm=TRUE),
                       missing_columns = sum(missing, na.rm=TRUE),
                       multiple_columns = sum(multiple, na.rm=TRUE)
      ),
    checked_cols |> dplyr::select(unmatched_column,
                                  overmatched_column,
                                  matched_column
    )
  ) |> dplyr::mutate(total_columns = length(colnames(cad))) -> output


  ############### SELECT #########
  get_match <- function(a) {
    temp[temp[[a]],]$colname
  }

  tibble::tibble(defined_colnames) -> crossmatch
  crossmatch$matched_colname <- purrr::map_chr(defined_colnames, get_match)

  tibble::tibble(orig = colnames(data)) |>
    dplyr::left_join(crossmatch, by = c("orig" = "matched_colname")) |>
    dplyr::mutate(newcolname = dplyr::if_else(is.na(defined_colnames),
                                              orig,
                                              defined_colnames)) |>
    dplyr::pull(newcolname) -> colnames(data)

  if (keep) {
   data
  } else {
    dplyr::select(data,
                  dplyr::all_of(defined_colnames))
  }


}
