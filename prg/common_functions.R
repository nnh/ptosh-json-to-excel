#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ functions ------
#' Remove specified elements from a list.
#'
#' This function removes the specified elements from a list.
#'
#' @param list_to_modify The list from which elements will be removed.
#' @param elements_to_remove A character vector specifying the elements to be removed.
#' @return A modified list without the specified elements.
#'
#' @examples
#' # Example usage:
#' my_list <- list(a = 1, b = 2, c = 3)
#' elements_to_remove <- c("b", "c")
#' modified_list <- RemoveListElements(my_list, elements_to_remove)
#'
#' @export
RemoveListElements <- function(list_to_modify, elements_to_remove) {
  res <- list_to_modify[setdiff(names(list_to_modify), elements_to_remove)]
  return(res)
}
#' Create an output folder if it does not exist.
#'
#' This function creates an output folder with the specified name using the `here` package.
#' If the folder already exists, it does nothing.
#'
#' @param output_folder_name The name of the output folder.
#' @return The path to the output folder.
#'
#' @examples
#' # Example usage:
#' output_folder_name <- "output_data"
#' output_path <- CreateOutputFolder(output_folder_name)
#'
#' @importFrom here here
#'
#' @export
CreateOutputFolder <- function(output_folder_name) {
  output_path <- here(output_folder_name)
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  return(output_path)
}
