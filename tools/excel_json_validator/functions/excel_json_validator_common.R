#' test script
#'
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
library(openxlsx, warn.conflicts = F)
library(jsonlite, warn.conflicts = F)
source(here("prg", "functions", "edit_checklist_convert_column_name.R"), encoding = "UTF-8")
# ------ constants ------
engToJpnColumnMappings <- GetEngToJpnColumnMappings()
# ------ functions ------
GetHomeDir <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    home_dir <- Sys.getenv("USERPROFILE")
  } else if (os == "Darwin") {
    home_dir <- Sys.getenv("HOME")
  } else {
    stop("Unsupported OS")
  }
  return(home_dir)
}
GetTargetFolder <- function(trialName) {
  outputPath <- here("output")
  outputDirs <- outputPath |> list.dirs(recursive = F, full.names = F)
  targetDirs <- outputDirs |>
    str_extract_all(str_c("output_", "[0-9]+", trialName)) |>
    keep(~ length(.) > 0)
  if (length(targetDirs) == 0) {
    stop(str_c("No folders found for trial name: ", trialName))
  }
  df <- tibble(folderName = targetDirs)
  df$dateTime <- df$folderName |>
    str_extract("[0-9]+") |>
    as.numeric()
  latestFolder <- df |>
    filter(dateTime == max(dateTime, na.rm = T)) %>%
    .$folderName |>
    list_c()
  print(str_c("target: ", latestFolder))
  return(latestFolder)
}
GetJsonAndSheet <- function(trialName) {
  targetFolder <- GetTargetFolder(trialName)
  sheetList <- targetFolder |> ReadChecklist(trialName)
  jsonList <- here(str_c("forTest_input_", trialName)) |> LoadJsonList()
  return(list(sheetList = sheetList, jsonList = jsonList))
}
ReadChecklist <- function(inputFolder, trialName) {
  checklistFile <- here("output", inputFolder, "list") |>
    list.files(pattern = str_c(trialName, " eCRF Spec [0-9]{8}\\.xlsx$"), full.names = T)
  if (length(checklistFile) == 0) {
    stop("No checklist file found.")
  }
  inputPath <- checklistFile[1]
  sheetNames <- inputPath |> getSheetNames()
  sheets <- sheetNames |> map(~ read.xlsx(inputPath, ., na.strings = NULL))
  names(sheets) <- sheetNames
  return(sheets)
}
GetAliasnameAndFieldIdAndLabel <- function(fieldItems) {
  res <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x
    aliasName <- .y
    res <- fieldItem |> map_df(~ c(fields = .$name, fields.label = .$label))
    res$alias_name <- aliasName
    return(res)
  }) |> bind_rows()
  return(res)
}
GetNameAndAliasNameByJson <- function(json_list) {
  res <- json_list |> map_df(~ list(jpname = .$name, alias_name = .$alias_name))
  return(res)
}
LoadJsonList <- function(input_path) {
  target_Path <- input_path |> list.files(pattern = "*.json", full.names = T)
  jsonList <- target_Path |> map(~ read_json(.))
  names(jsonList) <- target_Path |>
    basename() |>
    str_remove(".json")
  return(jsonList)
}
GetFieldItemsByJsonList <- function(json_list) {
  res <- json_list |> map(~ .$field_items)
  return(res)
}
GetItemsSelectColnames <- function(input_tibble, target_colnames) {
  if (nrow(input_tibble) == 0) {
    res <- as.data.frame(matrix("", nrow = 1, ncol = length(target_colnames)))
    colnames(res) <- target_colnames
  } else {
    res <- input_tibble |>
      inner_join(jpNameAndAliasName, by = "alias_name") |>
      select(all_of(target_colnames)) |>
      as.data.frame()
  }
  return(res)
}

CheckTarget <- function(sheet, json) {
  if (!identical(sheet, json)) {
    return(list(sheet = sheet, json = json))
  }
  return(NULL)
}
CleanTextForComment <- function(text) {
  text %>%
    gsub("\n", "", .) %>% # 改行削除
    gsub("\\s+", "", .) %>% # 半角スペース・タブ削除
    gsub("\u3000", "", .) # 全角スペース削除
}
# item
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_item.R"), encoding = "UTF-8")
# allocation
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_allocation.R"), encoding = "UTF-8")
# action
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_action.R"), encoding = "UTF-8")
# display
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_display.R"), encoding = "UTF-8")
# name
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_name.R"), encoding = "UTF-8")
# options
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_options.R"), encoding = "UTF-8")
# content
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_content.R"), encoding = "UTF-8")
# explanation
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_explanation.R"), encoding = "UTF-8")
# presence
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_presence.R"), encoding = "UTF-8")
# master
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_master.R"), encoding = "UTF-8")
# visit
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_visit.R"), encoding = "UTF-8")
# title
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_title.R"), encoding = "UTF-8")
# assigned
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_assign.R"), encoding = "UTF-8")
# limitation
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_limitation.R"), encoding = "UTF-8")
# ------ main ------
