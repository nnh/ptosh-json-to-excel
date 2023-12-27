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
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kDfHead <- "df_"
kOptionTargetCols <- c("option.values_name", "option.values_seq", "option.values_code", "option.values_is_usable")
# ------ functions ------
EditCheckList <- function(){
  json_filenames <- list.files(here(kInputFolderName), pattern="*.json", full.names=F)
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <<- ReadJsonFiles(json_filenames, kInputFolderName)
}
ReplaceText <- function(x) {
  if (is.null(x)){
    return(NA)
  }
  return(x)
}
GetTargetColumns <- function(){
  base_target_columns <- c("name", "alias_name")
  res <- list()
  res$name <- c(base_target_columns, "stylesheet", "fax_stylesheet", "javascript", "category", "images_count", "lab_department_id")
  res$item <- c("name", "label", "option.name", "default_value", "validators.presence.validate_presence_if", "presence_if_references", "validators.formula.validate_formula_if", "formula_if_references", "validators.formula.validate_formula_message", "validators.date.validate_date_after_or_equal_to", "references_after", "validators.date.validate_date_before_or_equal_to", "references_before")
  res$option <- c("option.name", "option.id")
  res$visit <- c("name", "default_value")
  res$number <- c("name", "label", "default_value", "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to")
  return(res)
}
GetFieldItems <- function(field_items, target_columns){
  target <- field_items %>% select(any_of(target_columns))
  return(target)
}
GetFieldItemsAndJpname <- function(df_base, field_items, target_columns){
  target <- GetFieldItems(field_items, target_columns) %>% cbind(df_base, .)
  return(target)
}
GetFieldItemsAndBindRows <- function(target, df_base, field_items, target_columns){
  target_field_items <- GetFieldItemsAndJpname(df_base, field_items, target_columns)
  target <- target_field_items %>% bind_rows(target, .)
  return(target)
}
GetOptionValues <- function(df_option, field_items){
  if (field_items$option.values %>% map_lgl(is.null) %>% all()){
    return(df_option)
  }
  option_values <- field_items$option.values %>% list_rbind() %>% filter(is_usable) %>%
    rename_with(~ paste0("option.values_", .), everything())
  options <- GetFieldItems(field_items, target_columns$option) %>% filter(!is.na(option.id)) %>%
    distinct() %>% left_join(option_values, by=c("option.id" = "option.values_option_id"))
  df_option <- options %>% cbind(df_base, .) %>% bind_rows(df_option, .)
  return(df_option)
}
GetVisit <- function(df_visit, field_items){
  visit <- field_items %>% filter(label == "Visit Number")
  if (nrow(visit) > 0){
    df_visit <- GetFieldItemsAndBindRows(df_visit, df_base, visit, target_columns$visit)
  }
  return(df_visit)
}
GetNumber <- function(df_number, field_items){
  temp_df_number <<- field_items %>%
    mutate(validators.numericality.validate_numericality_less_than_or_equal_to =
             if (exists("validators.numericality.validate_numericality_less_than_or_equal_to")) {
               validators.numericality.validate_numericality_less_than_or_equal_to
             } else {
               NA
             }) %>%
    mutate(validators.numericality.validate_numericality_greater_than_or_equal_to =
             if (exists("validators.numericality.validate_numericality_greater_than_or_equal_to")) {
               validators.numericality.validate_numericality_greater_than_or_equal_to
             } else {
               NA
             }) %>%
    filter(
      (!is.na(validators.numericality.validate_numericality_less_than_or_equal_to) & validators.numericality.validate_numericality_less_than_or_equal_to !="") |
      (!is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) & validators.numericality.validate_numericality_greater_than_or_equal_to !="")
    )
  if (nrow(temp_df_number) == 0){
    return(df_number)
  }
  temp_df_number <- temp_df_number %>% select(all_of(target_columns$number))
  return(bind_rows(df_number, temp_df_number))
}
# ------ main ------
EditCheckList()
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


