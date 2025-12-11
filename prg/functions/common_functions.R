#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.12.8
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
RemoveNestedLists <- function(input_list) {
  # Filter elements that are not lists within the input_list
  filtered_list <- input_list %>% purrr::discard(~ is.list(.))
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
CreateOutputFolder <- function(output_folder_name, output_path = NULL) {
  if (is.null(output_path)) {
    output_path <- here(output_folder_name)
  } else {
    output_path <- output_path %>%
      AddSlashIfMissing() %>%
      str_c(output_folder_name)
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
ExecReadJsonFiles <- function() {
  json_filenames <- list.files(kInputFolderName, pattern = "*.json", full.names = F)
  if (length(json_filenames) == 0) {
    stop("inputフォルダの中にjsonファイルが存在しません。")
    return(NULL)
  }
  if (length(json_filenames) > 1) {
    stop("inputフォルダの中にjsonファイルが複数存在します。jsonファイルを一つだけ格納して再実行してください。")
    return(NULL)
  }
  json_file <- ReadJsonFiles(json_filenames, kInputFolderName)
  trial_name <- json_filenames %>% str_remove("_[0-9]{6}_[0-9]{4}\\.json$")
  sheet_names <- json_file$sheets %>%
    map(~ c(sheet_alias_name = .x[["alias_name"]], group = .x[["alias_name"]])) %>%
    bind_rows()
  options_flag <- kOptions %in% names(json_file)
  if (options_flag) {
    options_json <- GetListSetName(json_file, kOptions, "name")
    json_file <- json_file[names(json_file) != kOptions]
  }
  is_visit <- !is.null(json_file[[kVisits]]) && length(json_file[[kVisits]]) > 0
  visit_groups <- GetVisitGroups(is_visit, json_file)
  # シート名、aliasname、参照先シートはVISITNUMの一番小さいものを出す
  visit_groups_min <- visit_groups %>%
    group_by(visit) %>%
    filter(visitnum == min(visitnum)) %>%
    ungroup()
  # visit非対応のシートとvisit対応シートの最小visitnumのシートを結合
  visit_groups_min_and_no_visit <- sheet_names %>% left_join(visit_groups_min, by = c("sheet_alias_name" = "alias_name"))
  visit_groups_min_and_no_visit$group <- ifelse(is.na(visit_groups_min_and_no_visit$visit), visit_groups_min_and_no_visit$sheet_alias_name, visit_groups_min_and_no_visit$visit)
  visit_groups_min_and_no_visit <- visit_groups_min_and_no_visit %>% select(alias_name = sheet_alias_name, group)
  res <- list()
  res[["json_files"]] <- json_file
  res[["trialName"]] <- trial_name
  res[["options_flag"]] <- options_flag
  res[["options_json"]] <- options_json
  res[["is_visit"]] <- is_visit
  res[["visit_groups"]] <- visit_groups
  res[["visit_groups_min"]] <- visit_groups_min
  res[["visit_groups_min_and_no_visit"]] <- visit_groups_min_and_no_visit
  return(res)
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
ReplaceText <- function(x) {
  if (is.null(x)) {
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
GetNamesFromList <- function(target_list, target_col_name) {
  res <- target_list %>% map_chr(~ .[[target_col_name]])
  return(res)
}
GetListSetName <- function(input_list, target_name, target_col_name) {
  target_list <- input_list[[target_name]]
  targetNames <- GetNamesFromList(target_list, target_col_name)
  res <- setNames(target_list, targetNames)
  return(res)
}
