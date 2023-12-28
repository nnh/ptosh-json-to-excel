#' title
#' description
#' @file edit_checklist.R
#' @author Mariko Ohtsuka
#' @date 2023.12.27
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(jsonlite)
library(openxlsx)
source(here("prg", "common_functions.R"), encoding="UTF-8")
source(here("prg", "io_functions.R"), encoding="UTF-8")
source(here("prg", "edit_checklist_function.R"), encoding="UTF-8")
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kDfHead <- "df_"
kOptionTargetCols <- c("option.values_name", "option.values_seq", "option.values_code", "option.values_is_usable")
# ------ functions ------
EditInputDataList <- function(json_files){
  input_list <- list()
  input_list$df_field_items <- GetDfFieldItems(json_files)
  input_list$df_option <- GetDfOptions(json_files)
  input_list$df_cdisc_sheet_config <- GetDfCdiscSheetConfig(json_files)
  input_list$df_sheet_items <- GetDfSheetItems(json_files)
  return(input_list)
}
EditOutputDataList <- function(input_list){
  output_list <- list()
  output_list$name <- input_list$df_sheet_items %>%
    select(target_columns$name) %>%
      rename(name=jpname)
  output_list$item <- input_list$df_sheet_items %>%
    left_join(input_list$df_field_items, by=c("id"="sheet_id")) %>%
      select(any_of(target_columns$item))
  output_list$option <- input_list$df_option %>%
    filter(option.values_is_usable) %>%
      JoinJpnameAndAliasName(input_list$df_sheet_items)
  output_list$visit <- input_list$df_field_items %>%
    filter(label == "Visit Number") %>%
      EditOutputColumns(target_columns$visit)
  output_list$number <- input_list$df_field_items %>%
    filter(
      (!is.na(validators.numericality.validate_numericality_less_than_or_equal_to) & validators.numericality.validate_numericality_less_than_or_equal_to !="") |
      (!is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) & validators.numericality.validate_numericality_greater_than_or_equal_to !="")
      ) %>%
      EditOutputColumns(target_columns$number)
  output_list$master <- input_list$df_field_items %>%
    filter(!is.na(link_type) & link_type != "") %>%
      EditOutputColumns(target_columns$master)
  return(output_list)
}
# ------ main ------
json_files <- EditCheckList()
target_columns <- GetTargetColumns()
input_list <- EditInputDataList(json_files)
output_list <- EditOutputDataList(input_list)
OutputChecklistXlsx(output_list)
