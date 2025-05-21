#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2025.5.20
rm(list = ls())
# ------ functions ------
#' Install and Load R Package
#'
#' This function installs and loads an R package if it is not already installed.
#'
#' @param package_name The name of the R package to be installed and loaded.
#'
#' @details
#' If the specified package is not already installed, this function installs it
#' using install.packages(). After installation, the function loads the package
#' into the R session using library().
#'
#' @examples
#' InstallAndLoadPackage("tidyverse")
#'
#' @export
InstallAndLoadPackage <- function(package_name) {
  if (!requireNamespace(package_name, quietly = T)) {
    install.packages(package_name, dependencies = T, type = "binary")
  }
  library(package_name, character.only = T, warn.conflicts = F)
}
# ------ libraries ------
InstallAndLoadPackage("tidyverse")
InstallAndLoadPackage("here")
InstallAndLoadPackage("jsonlite")
InstallAndLoadPackage("openxlsx")
InstallAndLoadPackage("rlang")
source(here("prg", "functions", "common_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_function.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_common.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_allocation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_action.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_display.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_option.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_comment.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_presence.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_title.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_assigned.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_limitation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_convert_column_name.R"), encoding = "UTF-8")

# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputPath <- here(kOutputFolderName)
kAlertTargetColnames <- c("normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to")
kEngToJpnColumnMappings <- GetEngToJpnColumnMappings()
kEngColumnNames <- kEngToJpnColumnMappings %>%
  map(names)
kTargetSheetNames <- c("item", "allocation", "action", "display", "option", "comment", "explanation", "presence", "master", "visit", "title", "assigned", "limitation")
# ------ main ------
temp <- ExecReadJsonFiles()
trialName <- temp$trialName
json_files <- temp$json_files
rm(temp)
CreatedummyDf <- function(target_columns) {
  df <- data.frame(matrix(ncol = length(target_columns), nrow = 0))
  colnames(df) <- target_columns
  return(df)
}
JoinJpnameAndAliasName <- function(df, json_file) {
  df$jpname <- json_file$name
  df$alias_name <- json_file$alias_name
  return(df)
}
SelectColumns <- function(df, target_columns) {
  df <- df %>%
    select(all_of(target_columns))
  return(df)
}
JoinJpnameAndAliasNameAndSelectColumns <- function(df_name, json_file) {
  df <- get(df_name, envir = parent.frame())
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  df <- JoinJpnameAndAliasName(df, json_file)
  df <- SelectColumns(df, kEngColumnNames[[df_name]])
  return(df)
}
GetJsonFile <- function(json_file) {
  json_file <- json_file$rawJson
  return(json_file)
}
GetFieldItems <- function(json_file) {
  return(json_file$field_items)
}

field_list <- json_files %>%
  map(~ {
    json_file <- GetJsonFile(.)
    field_items <- json_file %>% GetFieldItems()
    fields <- field_items %>%
      map(~ {
        res <- tibble::tibble(
          name = .x$name,
          field_number = .x$name %>% str_extract("\\d+") %>% as.numeric(),
          label = .x$label
        )
        return(res)
      }) %>%
      bind_rows()
    fields$jpname <- json_file$name
    fields$alias_name <- json_file$alias_name
    return(fields)
  }) %>%
  bind_rows()
sheet_data_list <- json_files %>% map(~ {
  json_file <- GetJsonFile(.)
  field_items <- json_file %>% GetFieldItems()
  item <- field_items %>%
    GetTargetByType("FieldItem::Article") %>%
    EditItem(json_file$alias_name)
  allocation <- json_file %>% GetAllocation()
  action <- field_items %>% GetAction(json_file$alias_name)
  display <- field_items %>% GetDisplay()
  option <- field_items %>% GetOptions()
  comment <- field_items %>% GetComment("content")
  explanation <- field_items %>% GetComment("description")
  presence <- field_items %>% GetPresence()
  master <- field_items %>% GetComment("link_type")
  visit <- field_items %>% GetVisit()
  title <- field_items %>%
    GetTargetByType("FieldItem::Heading") %>%
    EditTitle()
  assigned <- field_items %>%
    GetTargetByType("FieldItem::Assigned") %>%
    EditAssigned()
  name <- tibble(name = json_file$name, alias_name = json_file$alias_name, images_count = json_file$images_count)
  limitation <- field_items %>%
    GetLimitation() %>%
    EditLimitation()
  res <- kTargetSheetNames %>% map(~ JoinJpnameAndAliasNameAndSelectColumns(.x, json_file))
  names(res) <- kTargetSheetNames
  res$name <- name
  return(res)
})

targetSheetNames <- kTargetSheetNames %>% append("name", .)
sheet_data_combine <- targetSheetNames %>%
  map(~ map(sheet_data_list, pluck, .x) %>%
    compact() %>%
    bind_rows()) %>%
  set_names(targetSheetNames)
# 0行0列のデータフレームを補完
for (nm in names(sheet_data_combine)) {
  df <- sheet_data_combine[[nm]]
  if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
    if (!is.null(kEngColumnNames[[nm]])) {
      sheet_data_combine[[nm]] <- data.frame(matrix(ncol = length(kEngColumnNames[[nm]]), nrow = 0)) %>%
        setNames(kEngColumnNames[[nm]])
    }
  }
}
output_checklist <- convertSheetColumnsToJapanese(sheet_data_combine)
# create output folder.
output_folder_name <- Sys.time() %>%
  format("%Y%m%d%H%M%S") %>%
  str_c("output_", .)
output_folder_path <- CreateOutputFolder(output_folder_name, kOutputPath)
output_file_ymd <- Sys.time() %>%
  format("%Y%m%d")
kOutputChecklistName <- str_c(trialName, " eCRF Spec ", output_file_ymd, ".xlsx")
output_checklist_path <- CreateOutputFolder("list", output_folder_path)
cat(str_c("フォルダ", output_checklist_path, "を作成しました\n"))
OutputChecklistXlsx(output_checklist, output_checklist_path)
cat("処理が終了しました。")
