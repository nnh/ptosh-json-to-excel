#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.1.24
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
#' Remove Nested Lists
#'
#' This function removes elements that are lists within a given list.
#'
#' @param input_list The input list containing nested and non-nested elements.
#'
#' @return A filtered list containing only non-nested elements.
#'
#' @examples
#' input <- list(1, list(2, 3), 4, list(5, 6))
#' RemoveNestedLists(input)
#'
#' @export
RemoveNestedLists <- function(input_list){
  # Filter elements that are not lists within the input_list
  filtered_list <- input_list %>% purrr::discard( ~ is.list(.))
  return(filtered_list)
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
#' @import AddSlashIfMissing
#' @export
CreateOutputFolder <- function(output_folder_name, output_path=NULL) {
  if (is.null(output_path)){
    output_path <- here(output_folder_name)
  } else {
    output_path <- output_path %>% AddSlashIfMissing() %>% str_c(output_folder_name)
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  return(output_path)
}
#' Add Slash if Missing
#'
#' This function adds a trailing slash ("/") to the input string if it is not already present.
#'
#' @param input_string A character string.
#'
#' @return A modified string with a trailing slash if it was missing.
#'
#' @examples
#' input <- "path/to/directory"
#' AddSlashIfMissing(input)
#'
#' @export
AddSlashIfMissing <- function(input_string) {
  if (substr(input_string, nchar(input_string), nchar(input_string)) != "/") {
    return(paste0(input_string, "/"))
  } else {
    return(input_string)
  }
}
#' Execute Reading JSON Files
#'
#' This function reads and processes JSON files from a specified folder.
#'
#' @return A list containing the processed JSON files.
#'
#' @examples
#' ExecReadJsonFiles()
#'
#' @import here
#' @import ReadJsonFiles
#' @export
ExecReadJsonFiles <- function(){
  # Get a list of JSON filenames in the specified folder
  json_filenames <- list.files(here(kInputFolderName), pattern="*.json", full.names=F)
  # Check if any JSON files were found
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- ReadJsonFiles(json_filenames, kInputFolderName)
  return(json_files)
}
#' Replace Text Function
#'
#' This function replaces a NULL value with NA.
#'
#' @param x A variable that may be NULL.
#'
#' @return If x is NULL, returns NA; otherwise, returns x unchanged.
#'
#' @examples
#' ReplaceText(NULL)
#' ReplaceText("Some Text")
#'
#' @export
ReplaceText <- function(x){
  if (is.null(x)){
    return(NA)
  }
  return(x)
}
setFontStyle <- function() {
    style <- createStyle(
    fontName = "Meiryo",     
    fontSize = 11,           
    fontColour = "#000000"   
  )  
  return(style)
}
