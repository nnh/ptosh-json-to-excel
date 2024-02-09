#' for test
#' description
#' @file issue10.R
#' @author Mariko Ohtsuka
#' @date 2024.2.9
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(jsonlite)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding="UTF-8")
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
kParentPath <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON"
kInputAndOutputFolderList <- list(
  list(input="20240123output_Bev", output="20240208output_Bev"),
  list(input="output ALL-B19", output="20240208outputALL-B19")
)
kJsonPathBev <- "/Users/mariko/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/Bev-FOLFOX-SBC"
kJsonPathAllB19 <- "/Users/mariko/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/ALL-B19"
# ------ functions ------
ExecIssue6 <- function(input_output_path){
  source_excel <- loadWorkbook(input_output_path$input)
  remove_sheetnames <- c("fielditems_sum", "option_sum", "allocation_sum")
  remove_sheetnames %>% map( ~ removeWorksheet(source_excel, .))
  return(source_excel)
}
Issue7ExecReadJsonFiles <- function(input_path){
  # Get a list of JSON filenames in the specified folder
  json_filenames <- list.files(input_path, pattern="*.json", full.names=F)
  # Check if any JSON files were found
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- Issue7ReadJsonFiles(json_filenames, input_path)
  return(json_files)
}
Issue7ReadJsonFiles <- function(json_filenames, input_path){
  json_files <- json_filenames %>% map( ~ {
    file_path <- file.path(input_path, .)
    flattenJson <<- file_path %>% fromJSON(flatten=T)
    return(flattenJson)
  }) %>% keep(~ !is.null(.))
  sheetid_name <- json_files %>% map_df( ~ c(alias_name=.$alias_name, sheet_id=.$id))
  sheetid_name$sheet_id <- as.integer(sheetid_name$sheet_id)
  field_items <- json_files %>% map_df( ~ .$field_items) %>% filter(type != "FieldItem::Article" & !is.na(option.id))
  res <- field_items %>% inner_join(sheetid_name, by="sheet_id") %>% select(c("alias_name", "option.name")) %>% distinct()
  return(res)
}
ExecIssue7 <- function(wb, json_path){
  remove_target <- Issue7ExecReadJsonFiles(json_path)
  option_values <- wb %>% openxlsx::read.xlsx(sheet="option")
  write_values <- option_values %>% anti_join(remove_target, by=c("alias_name", "option.name"))
  wb %>% deleteData(sheet="option", cols=1:ncol(option_values), rows=2:nrow(option_values)+1, gridExpand=T)
  wb %>% writeData(sheet="option", write_values, startRow=2, startCol=1, colNames=F)
  return(wb)
}
Issue8ExecReadJsonFiles <- function(input_path){
  # Get a list of JSON filenames in the specified folder
  json_filenames <- list.files(input_path, pattern="*.json", full.names=F)
  # Check if any JSON files were found
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- Issue8ReadJsonFiles(json_filenames, input_path)
  return(json_files)
}
Issue8ReadJsonFiles <- function(json_filenames, input_path){
  json_files <- json_filenames %>% map( ~ {
    file_path <- file.path(input_path, .)
    flattenJson <<- file_path %>% fromJSON(flatten=T)
    return(flattenJson)
  }) %>% keep(~ !is.null(.))
  sheetid_name <- json_files %>% map_df( ~ c(alias_name=.$alias_name, sheet_id=.$id))
  sheetid_name$sheet_id <- as.integer(sheetid_name$sheet_id)
  field_items <- json_files %>% map_df( ~ .$field_items) %>% filter(type != "FieldItem::Article")
  res <- field_items %>% inner_join(sheetid_name, by="sheet_id") %>% select(c("alias_name", "name")) %>% distinct()
  return(res)
}
ExecIssue8 <- function(wb, json_path){
  remove_target <- Issue8ExecReadJsonFiles(json_path)
  item_values <- wb %>% openxlsx::read.xlsx(sheet="item")
  write_values <- item_values %>% anti_join(remove_target, by=c("alias_name", "name"))
  wb %>% deleteData(sheet="item", cols=1:ncol(item_values), rows=2:nrow(item_values)+1, gridExpand=T)
  wb %>% writeData(sheet="item", write_values, startRow=2, startCol=1, colNames=F)
  return(wb)
}
ExecIssue9 <- function(wb){
  name_values <- wb %>% openxlsx::read.xlsx(sheet="name")
  write_values <- name_values %>% select(c("name", "alias_name", "images_count"))
  wb %>% openxlsx::removeWorksheet(sheet="name")
  wb %>% openxlsx::addWorksheet(sheet="name")
  wb %>% writeData(sheet="name", write_values, startRow=1, startCol=1, colNames=T)
  worksheetOrder(wb) <- c(15, 1:14)
  return(wb)
}
# ------ main ------
fullpath_list <- kInputAndOutputFolderList %>% map( ~ {
  temp <- .
  input <- temp$input %>% str_c(kParentPath, "/", ., "/", "list/checklist.xlsx")
  output <- temp$output %>% str_c(kParentPath, "/", ., "/", "list/checklist.xlsx")
  return(list(input=input, output=output))
})
names(fullpath_list) <- c("bev", "all_b19")

# issue 6
wb_bev <- ExecIssue6(fullpath_list$bev)
wb_all_b19 <- ExecIssue6(fullpath_list$all_b19)
# issue 7
wb_bev <- ExecIssue7(wb_bev, kJsonPathBev)
wb_all_b19 <- ExecIssue7(wb_all_b19, kJsonPathAllB19)
# issue 8
wb_bev <- ExecIssue8(wb_bev, kJsonPathBev)
wb_all_b19 <- ExecIssue8(wb_all_b19, kJsonPathAllB19)
# issue 9
wb_bev <- ExecIssue9(wb_bev)
wb_all_b19 <- ExecIssue9(wb_all_b19)
# output excel
WriteExcel <- function(wb){
  sheetNames <- c("name", "item", "option", "visit", "number", "master", "alert", "action", "allocation", "presence", "display", "comment", "explanation", "title", "assigned")
  for(i in 1:length(sheetNames)){
    sheet_name <- sheetNames[i]
    df_output <- openxlsx::read.xlsx(wb, sheet=sheet_name)
    wb %>% openxlsx::removeWorksheet(sheet=sheet_name)
    wb %>% openxlsx::addWorksheet(sheet=sheet_name)
    writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                   startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                   tableStyle=kTableStyle, keepNA=F)
    setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
    # Reset the width of the ID column.
    id_index <- which(colnames(df_output) == "id")
    removeColWidths(wb=wb, sheet=sheet_name, cols=id_index)
  }
  return(wb)
}
wb_bev <- wb_bev %>% WriteExcel()
wb_all_b19 <- wb_all_b19 %>% WriteExcel()
saveWorkbook(wb_bev, fullpath_list$bev$output, overwrite=T)
saveWorkbook(wb_all_b19, fullpath_list$all_b19$output, overwrite=T)
