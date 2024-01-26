#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2024.1.22
rm(list=ls())
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
InstallAndLoadPackage <- function(package_name){
  if (!requireNamespace(package_name, quietly=T)) {
    install.packages(package_name, dependencies=T)
  }
  library(package_name, character.only=TRUE)
}
# ------ libraries ------
InstallAndLoadPackage("tidyverse")
InstallAndLoadPackage("here")
InstallAndLoadPackage("jsonlite")
InstallAndLoadPackage("openxlsx")
InstallAndLoadPackage("rlang")
InstallAndLoadPackage("future")
plan(multisession)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "edit_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "edit_checklist_function.R"), encoding="UTF-8")
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
