#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ functions ------
RemoveListElements <- function(list_to_modify, elements_to_remove) {
  res <- list_to_modify[setdiff(names(list_to_modify), elements_to_remove)]
  return(res)
}

CreateOutputFolder <- function(output_folder_name) {
  output_path <- here(output_folder_name)
  # Check if the 'input' folder exists
  if (!dir.exists(output_path)) {
    # If not, create the 'input' folder
    dir.create(output_path)
  }
  return(output_path)
}
