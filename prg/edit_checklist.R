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
# ------ main ------
json_files <- EditCheckList()
target_columns <- GetTargetColumns()
target_sheet_names <- target_columns %>% names()
target_sheet_names %>% map( ~ str_c(kDfHead, .) %>% assign(NULL, envir=.GlobalEnv))
for (json_file in json_files){
  df_base <- json_file$flattenJson %>% .[base_target_columns] %>%
    data.frame() %>% rename(jpname=name)
  # name
  df_name <- json_file$flattenJson[target_columns$name] %>%
    map_df( ~ ReplaceText(.)) %>% rbind(df_name, .)
  if (is.data.frame(json_file$flattenJson$field_items)){
    field_items <<- json_file$flattenJson$field_items
    # item
    df_item <- GetFieldItemsAndBindRows(df_item, df_base, field_items, target_columns$item)
    # option
    df_option <- GetOptionValues(df_option, field_items)
    # visit
    df_visit <- GetVisit(df_visit, field_items)
    # number
    df_number <- GetNumber(df_number, field_items)
    # master
    df_master <- GetMaster(df_master, field_items)
    # alert
    df_alert <- GetAlert(df_alert, field_items)
  }
}
df_option <- df_option %>% distinct()
df_output <- check_name
wordwrap_colnames <- c("stylesheet", "fax_stylesheet")
testWrite <- function(df_output){
  output_colnames <- df_output %>% colnames()
  wordwrap_colnames_index <- which(output_colnames %in% wordwrap_colnames)
  wb <- createWorkbook()
  sheet_name="test"
  addWorksheet(wb=wb, sheet=sheet_name)
  writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                 startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                 tableStyle=kTableStyle, keepNA=F)
  if (length(wordwrap_colnames_index) > 0){
    wordwrap_colnames_index %>% map( ~ addStyle(wb=wb, sheet=sheet_name,
                                                style=createStyle(wrapText=T), rows=1:nrow(df_output), cols=.))
  }
  setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
  saveWorkbook(wb=wb, file="/Users/mariko/Downloads/test.xlsx", overwrite=T)
}
testWrite(check_name)


