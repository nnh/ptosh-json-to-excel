#' file input-output
#'
#' @file io_functions.R
#' @author Mariko Ohtsuka
#' @date 2025.10.1
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
#' Read JSON files and return a list of raw and flatten JSON data.
#'
#' This function reads JSON files and returns a list of raw and flatten JSON data.
#'
#' @param json_filenames Vector of JSON file names.
#' @param targetTrialFolder The path of the folder containing the JSON file.
#' @return A list containing raw and flatten JSON data.
#'
#' @importFrom jsonlite fromJSON
#' @export
ReadJsonFiles <- function(json_filenames, targetTrialFolder) {
  json_files <- json_filenames %>%
    file.path(targetTrialFolder, .) %>%
    read_json()
  return(json_files)
}
