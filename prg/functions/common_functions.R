#' Common Functions
#'
#' @file common_functions.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
# ------ functions ------
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
GetSheetGroupRow <- function(sheet_groups, alias) {
  row <- sheet_groups %>% filter(alias_name == alias)
  if (nrow(row) == 0) NA else row
}
GetSheetSortOrder <- function(sheet_orders, alias) {
  row <- sheet_orders %>% filter(alias_name == alias)
  if (nrow(row) == 0) return(NA)
  if (nrow(row) == 1) return(row[["sort_order"]][1])
  stop(paste0("sheet_ordersに同じalias_nameが複数存在します: ", alias))
}
GetSheetNamesAndSortOrderFromJson <- function(json_file) {
  sheets <- json_file[["sheets"]] %>% map_df(~ {
    res <- tibble::tibble(
      sheet_name = .x[["name"]],
      alias_name = .x[[.const[["kAliasName"]]]],
      category = .x[["category"]]
    )
    return(res)
  })
  sheet_groups <- json_file[[.const[["kSheetGroups"]]]] %>% map_df(~ {
    group_name <- .x[["name"]]
    group_alias_name <- .x[[.const[["kAliasName"]]]]
    group_allocation_group <- .x[[.const[["kSheetGroupAllocationGroup"]]]]
    group_is_default <- .x[["is_default"]]
    res <- .x[["sheets"]] %>% map_df(~ {
      res <- tibble::tibble(
        alias_name = .x[[.const[["kAliasName"]]]],
        group_name = group_name,
        group_alias_name = group_alias_name,
        allocation_group = group_allocation_group,
        is_default = group_is_default
      )
      return(res)
    })
    return(res)
  })
  sheetsAndGroups <- sheets %>%
    left_join(sheet_groups, by = .const[["kAliasName"]])
  sheet_orders <- json_file[[.const[["kSheetOrders"]]]] %>% map_df(~ {
    res <- tibble::tibble(
      alias_name = .x[[.const[["kSheetOrdersAliasName"]]]],
      sort_order = .x[[.const[["kSheetSeq"]]]] %>% as.numeric()
    )
    return(res)
  })
  sheet_info <- sheetsAndGroups %>%
    left_join(sheet_orders, by = .const[["kAliasName"]]) %>%
    arrange(sort_order)
  sheets <- json_file[["sheets"]] %>% map(~ {
    res <- .x
    current_alias <- res[[.const[["kAliasName"]]]]
    res[[.const[["kSheetGroups"]]]] <- GetSheetGroupRow(sheet_groups, current_alias)
    res[["sort_order"]] <- GetSheetSortOrder(sheet_orders, current_alias)
    for (key in .const[["kSheetKeysToRemove"]]) {
      res[[key]] <- NULL
    }
    return(res)
  })
  sheetNameList <- sheets %>% map_chr(~ .x[[.const[["kAliasName"]]]])
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
  json_filename <- list.files(.const[["kInputFolderName"]], pattern = "*.json", full.names = F)
  if (length(json_filename) == 0) {
    stop("inputフォルダの中にjsonファイルが存在しません。")
    return(NULL)
  }
  if (length(json_filename) > 1) {
    stop("inputフォルダの中にjsonファイルが複数存在します。jsonファイルを一つだけ格納して再実行してください。")
    return(NULL)
  }
  json_file <- ReadJsonFiles(json_filename, .const[["kInputFolderName"]])
  trial_name <- json_filename %>% str_remove("_[0-9]{6}_[0-9]{4}\\.json$")
  options_flag <- .const[["kOptions"]] %in% names(json_file)
  if (options_flag) {
    options_json <- GetListSetName(json_file, .const[["kOptions"]], "name")
    json_file <- json_file[names(json_file) != .const[["kOptions"]]]
  }
  is_visit <- !is.null(json_file[[.const[["kVisits"]]]]) && length(json_file[[.const[["kVisits"]]]]) > 0
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
  res[["sheets"]] <- temp[[.const[["kSheets"]]]]
  res[["sheet_info"]] <- temp[["sheet_info"]]
  res[["json_files"]] <- json_file
  res[["trialName"]] <- trial_name
  res[["options_flag"]] <- options_flag
  res[["options_json"]] <- if (options_flag) options_json else NULL
  res[["is_visit"]] <- is_visit
  res[["visit_info"]] <- visit_info
  return(res)
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
#' Read a JSON file and return its contents.
#'
#' @param json_filename A single JSON file name.
#' @param targetTrialFolder The path of the folder containing the JSON file.
#' @return A list containing the JSON file contents.
ReadJsonFiles <- function(json_filename, targetTrialFolder) {
  json_file <- json_filename %>%
    file.path(targetTrialFolder, .) %>%
    read_json()
  return(json_file)
}
