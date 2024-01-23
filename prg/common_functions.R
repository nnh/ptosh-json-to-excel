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
RemoveNestedLists <- function(input_list){
  # リスト内の要素がリストでない場合に抽出
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
AddSlashIfMissing <- function(input_string) {
  if (substr(input_string, nchar(input_string), nchar(input_string)) != "/") {
    return(paste0(input_string, "/"))
  } else {
    return(input_string)
  }
}
ExecReadJsonFiles <- function(){
  json_filenames <- list.files(here(kInputFolderName), pattern="*.json", full.names=F)
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- ReadJsonFiles(json_filenames, kInputFolderName)
  return(json_files)
}
ReplaceText <- function(x){
  if (is.null(x)){
    return(NA)
  }
  return(x)
}
