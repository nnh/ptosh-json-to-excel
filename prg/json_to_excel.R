#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2024.1.22
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(jsonlite)
library(openxlsx)
library(rlang)
library(future)
plan(multisession)
source(here("prg", "common_functions.R"), encoding="UTF-8")
source(here("prg", "edit_functions.R"), encoding="UTF-8")
source(here("prg", "io_functions.R"), encoding="UTF-8")
source(here("prg", "edit_checklist_function.R"), encoding="UTF-8")
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputPath <- here(kOutputFolderName)
kOutputChecklistName <- "checklist.xlsx"
# ------ main ------
json_files <- ExecReadJsonFiles()
input_list <- EditInputDataList(json_files)
# json to excel
df_dummyNames <- data.frame(matrix(ncol=length(kNames), nrow=0))
colnames(df_dummyNames) <- kNames
output_list <- pmap(
  list(id=input_list[[kInputList$sheet_items]][[kSheetItemsKeys$id]],
       jpname=input_list[[kInputList$sheet_items]][[kSheetItemsKeys$jpname]],
       alias_name=input_list[[kInputList$sheet_items]][[kSheetItemsKeys$alias_name]]), ExecEditOutputData)
names(output_list) <- input_list[[kInputList$sheet_items]][[kSheetItemsKeys$alias_name]]
# create output folder.
output_folder_name <- Sys.time() %>% format("%Y%m%d%H%M%S") %>% str_c("output_", .)
output_folder_path <- CreateOutputFolder(output_folder_name, kOutputPath)
cat(str_c("フォルダ", output_folder_path, "を作成しました\n"))
# write excel.
for (i in 1:length(output_list)){
  WriteExcel(output_list[[i]], names(output_list)[i], output_folder_path)
}
# checklist
target_columns <- GetTargetColumns(input_list)
df_reference <- GetSheetnameAndFieldForReference(json_files)
output_checklist <- EditOutputDataList(input_list)
output_checklist_path <- CreateOutputFolder("list", output_folder_path)
cat(str_c("フォルダ", output_checklist_path, "を作成しました\n"))
OutputChecklistXlsx(output_checklist, output_checklist_path)
cat("処理が終了しました。")
