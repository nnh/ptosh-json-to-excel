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
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputListIndex = list(raw_json=1, field_items=2, option=3, cdisc_sheet_configs=4, cdisc_sheet_configs_pivot=5,
                        allocation=6, flip_flops=7)
kOutputListNames = c("raw_json", "Field_Items", "Option", "Cdisc_Sheet_Configs", "Cdisc_Sheet_Configs_Pivot",
                     "Allocation", "Flip_Flops")
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
#' Edit data frame with Japanese name and alias name.
#'
#' This function creates a data frame with Japanese name and alias name.
#'
#' @param input_json Input JSON data.
#' @return A data frame with Japanese name and alias name.
#'
#' @export
EditDfJpNameAndAliasName <- function(input_json){
  return(data.frame(jpname=input_json$name, alias_name=input_json$alias_name))
}
#' Write data to Excel file.
#'
#' This function writes the provided data to an Excel file.
#'
#' @param output_list List containing various data frames.
#' @param filename Name of the Excel file.
#' @param output_path Path where the Excel file will be saved.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable setColWidths
#' @export
WriteExcel <- function(output_list, filename, output_path){
  df_jpname_aliasname <- EditDfJpNameAndAliasName(output_list[[kOutputListIndex$raw_json]])
  # If the data does not exist, it is not output to Excel.
  write_target_data <- output_list
  # Input source JSON data need not be output.
  write_target_data[[kOutputListIndex$raw_json]] <- NA
  write_target_index <- which(!is.na(write_target_data))
  # Reorder the columns.
  for (i in 1:length(write_target_data)){
    if (is.data.frame(write_target_data[[i]])){
      write_target_data[[i]] <- write_target_data[[i]] %>% cbind(df_jpname_aliasname, .)
    }
  }
  if (!kOutputListIndex$cdisc_sheet_configs %in% write_target_index){
    write_target_data[[kOutputListIndex$cdisc_sheet_configs]] <- df_jpname_aliasname[-1, ]
    write_target_index <- write_target_index %>% c(kOutputListIndex$cdisc_sheet_configs)
  }
  wb <- createWorkbook()
  for (i in 1:length(write_target_index)){
    sheet_name <- names(write_target_data)[write_target_index[i]]
    df_output <- write_target_data[[write_target_index[i]]]
    addWorksheet(wb=wb, sheet=sheet_name)
    writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                   startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                   tableStyle=kTableStyle, keepNA=F)
    setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
    # Reset the width of the ID column.
    id_index <- which(colnames(df_output) == "id")
    removeColWidths(wb=wb, sheet=sheet_name, cols=id_index)
  }
  saveWorkbook(wb=wb, file=str_c(output_path, "/", filename, ".xlsx"), overwrite=T)
}
#' Read JSON files and return a list of raw and flatten JSON data.
#'
#' This function reads JSON files and returns a list of raw and flatten JSON data.
#'
#' @param json_filenames Vector of JSON file names.
#' @return A list containing raw and flatten JSON data.
#'
#' @importFrom jsonlite fromJSON
#' @export
ReadJsonFiles <- function(json_filenames){
  json_files <- json_filenames %>% map( ~ {
    rawJson <- here(kInputFolderName, .) %>% read_json()
    flattenJson <- here(kInputFolderName, .) %>% fromJSON(flatten=T)
    return(list(rawJson=rawJson, flattenJson=flattenJson))
  })
  names(json_files) <- json_filenames %>% str_remove(., ".json")
  return(json_files)
}
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
    WriteExcel(output_list, raw_json$alias_name, output_path)
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
  json_files <- ReadJsonFiles(json_filenames)
  data_list <- OutputDataList(json_files, json_filenames)
  return(data_list)
}

# ------ main ------
data_list <- ExecJsonToExcel()
