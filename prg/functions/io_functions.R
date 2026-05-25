#' file input-output
#'
#' @file io_functions.R
#' @author Mariko Ohtsuka
#' @date 2026.5.25
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
#' Read a JSON file and return its contents.
#'
#' This function reads a single JSON file and returns its contents as a list.
#'
#' @param json_filename A single JSON file name.
#' @param targetTrialFolder The path of the folder containing the JSON file.
#' @return A list containing the JSON file contents.
#'
#' @importFrom jsonlite fromJSON
#' @export
ReadJsonFiles <- function(json_filename, targetTrialFolder) {
  json_file <- json_filename %>%
    file.path(targetTrialFolder, .) %>%
    read_json()
  return(json_file)
}
