#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(jsonlite)
library(openxlsx)
source(here("prg", "common_functions.R"), encoding="UTF-8")
source(here("prg", "edit_functions.R"), encoding="UTF-8")
source(here("prg", "io_functions.R"), encoding="UTF-8")
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputListIndex = list(raw_json=1, field_items=2, option=3, cdisc_sheet_configs=4, cdisc_sheet_configs_pivot=5,
                        allocation=6, flip_flops=7)
kOutputListNames = c("raw_json", "Field_Items", "Option", "Cdisc_Sheet_Configs", "Cdisc_Sheet_Configs_Pivot",
                     "Allocation", "Flip_Flops")
# ------ functions ------
#' Generate a list of output data from raw and flatten JSON data.
#'
#' This function generates a list of output data from raw and flatten JSON data.
#'
#' @param json_files List containing raw and flatten JSON data.
#' @param json_filenames Vector of JSON file names.
#' @return A list containing output data.
#'
#' @importFrom dplyr select starts_with colnames is.null
#' @export
OutputDataList <- function(json_files, json_filenames){
  output_path <- CreateOutputFolder(kOutputFolderName)
  data_list <- json_files %>% map( ~ {
    trial_data <- .
    raw_json <- trial_data$rawJson
    flatten_json <- trial_data$flattenJson
    output_list <- list()
    output_list[[kOutputListIndex$raw_json]] <- raw_json
    resEditOutputFieldItems <- EditOutputFieldItems(raw_json, flatten_json)
    output_list[[kOutputListIndex$field_items]] <- resEditOutputFieldItems$field_items
    output_list[[kOutputListIndex$option]] <- resEditOutputFieldItems$options
    cdisc_sheet_configs <- EditCdiscSheetConfigs(trial_data)
    output_list[[kOutputListIndex$cdisc_sheet_configs]] <- cdisc_sheet_configs$cdisc_sheet_configs
    output_list[[kOutputListIndex$cdisc_sheet_configs_pivot]] <- cdisc_sheet_configs$cdisc_sheet_configs_pivots
    output_list[[kOutputListIndex$allocation]] <- EditAllocation(flatten_json)
    output_list[[kOutputListIndex$flip_flops]] <- resEditOutputFieldItems$flip_flops
    names(output_list) <- kOutputListNames
    WriteExcel(output_list, raw_json$alias_name, output_path, kOutputListIndex)
    return(output_list)
  })
  names(data_list) <- json_filenames
  return(data_list)
}
#' Execute conversion of JSON to Excel.
#'
#' This function executes the conversion of JSON files to Excel.
#'
#' @return A list containing output data.
#'
#' @export
ExecJsonToExcel <- function(){
  json_filenames <- list.files(here(kInputFolderName), pattern="*.json", full.names=F)
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- ReadJsonFiles(json_filenames, kInputFolderName)
  data_list <- OutputDataList(json_files, json_filenames)
  return(data_list)
}

# ------ main ------
data_list <- ExecJsonToExcel()
