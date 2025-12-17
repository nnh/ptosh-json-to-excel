#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.12.17
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
GetSheetNamesAndSortOrderFromJson <- function(json_file) {
  sheets <- json_file[["sheets"]] %>% map_df(~ {
    res <- tibble::tibble(
      sheet_name = .x[["name"]],
      alias_name = .x[["alias_name"]],
      category = .x[["category"]]
    )
    return(res)
  })
  sheet_groups <- json_file[["sheet_groups"]] %>% map_df(~ {
    group_name <- .x[["name"]]
    group_alias_name <- .x[["alias_name"]]
    group_alocation_group <- .x[["allocation_group"]]
    group_is_default <- .x[["is_default"]]
    res <- .x[["sheets"]] %>% map_df(~ {
      res <- tibble::tibble(
        alias_name = .x[["alias_name"]],
        group_name = group_name,
        group_alias_name = group_alias_name,
        allocation_group = group_alocation_group,
        is_default = group_is_default
      )
      return(res)
    })
    return(res)
  })
  sheetsAndGroups <- sheets %>%
    left_join(sheet_groups, by = c("alias_name" = "alias_name"))
  sheet_orders <- json_file[["sheet_orders"]] %>% map_df(~ {
    res <- tibble::tibble(
      alias_name = .x[["sheet"]],
      sort_order = .x[["seq"]] %>% as.numeric()
    )
    return(res)
  })
  sheet_info <- sheetsAndGroups %>%
    left_join(sheet_orders, by = c("alias_name" = "alias_name")) %>%
    arrange(sort_order)
  sheets <- json_file[["sheets"]] %>% map(~ {
    res <- .x
    sheet_group_row <- sheet_groups %>% filter(alias_name == res$alias_name)
    if (nrow(sheet_group_row) == 0) {
      res[["sheet_groups"]] <- NA
    } else {
      res[["sheet_groups"]] <- sheet_group_row
    }
    sheet_order_row <- sheet_orders %>% filter(alias_name == res$alias_name)
    if (nrow(sheet_order_row) == 0) {
      res[["sort_order"]] <- NA
    } else if (nrow(sheet_order_row) == 1) {
      res[["sort_order"]] <- sheet_order_row$sort_order
    } else {
      stop(paste0("sheet_ordersに同じalias_nameが複数存在します: ", res$alias_name))
    }
    res[["stylesheet"]] <- NULL
    res[["fax_stylesheet"]] <- NULL
    res[["odm"]] <- NULL
    res[["registration_config"]] <- NULL
    res[["uuid"]] <- NULL
    res[["digest"]] <- NULL
    res[["lock_version"]] <- NULL
    res[["created_at"]] <- NULL
    res[["updated_at"]] <- NULL
    return(res)
  })
  sheetNameList <- sheets %>% map_chr(~ .x[["alias_name"]])
  names(sheets) <- sheetNameList
  res <- list(sheets = sheets, sheet_info = sheet_info)
  return(res)
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
  options_flag <- kOptions %in% names(json_file)
  if (options_flag) {
    options_json <- GetListSetName(json_file, kOptions, "name")
    json_file <- json_file[names(json_file) != kOptions]
  }
  is_visit <- !is.null(json_file[[kVisits]]) && length(json_file[[kVisits]]) > 0
  if (is_visit) {
    visit_info <- GetVisitGroupAndVisitsFromJson(json_file)
  } else {
    visit_info <- tibble(
      visit_group_name = character(),
      visit_group      = character(),
      alias_name       = character(),
      visitnum         = numeric(),
      reference_sheet  = character(),
      visit_name       = character()
    )
  }
  temp <- GetSheetNamesAndSortOrderFromJson(json_file)
  res <- list()
  res[["sheets"]] <- temp$sheets
  res[["sheet_info"]] <- temp$sheet_info
  res[["json_files"]] <- json_file
  res[["trialName"]] <- trial_name
  res[["options_flag"]] <- options_flag
  res[["options_json"]] <- options_json
  res[["is_visit"]] <- is_visit
  res[["visit_info"]] <- visit_info
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
