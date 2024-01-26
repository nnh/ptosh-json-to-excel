#' Compare tool.
#'
#' @file compareTool.R
#' @author Mariko Ohtsuka
#' @date 2024.1.19
rm(list=ls())
# ------ libraries ------
library(here)
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
# ------ target_dir ------
kTargetFolder <- "20240124output_Bev_win"
#kTargetFolder <- "20240123output_Bev"
#kTargetFolder <- "output ALL-B19"
target_dirs <- GetTargetDirs(kTargetFolder)
# ------ functions ------
ExecCompare <- function(){
  print(target_dirs$json_to_excel_1)
  print(target_dirs$json_to_excel_2)
  filenames_and_sheetnames <- ExecCompare_fileNameAndSheetName(
    target_dirs$json_to_excel_1,
    target_dirs$json_to_excel_2
  )
  if (is.null(filenames_and_sheetnames)){
    return(NULL)
  }
  check_filename_sheetname <- ExecCompare_fileNameAndSheetName(
    target_dirs$json_to_excel_2,
    target_dirs$json_to_excel_1
  )
  if (is.null(check_filename_sheetname)){
    return(NULL)
  }
  output_foldername <- format(Sys.time(), "%Y%m%d%H%M%S") %>% paste0("compare_", .)
  compare_output_path <- CreateOutputFolder(output_foldername, here(kOutputFolderName))
  compare_files <- ExecCompare_values(
    target_dirs$json_to_excel_1,
    target_dirs$json_to_excel_2,
    filenames_and_sheetnames,
    compare_output_path
  )
  if (!compare_files){
    return(NULL)
  }
  cat("チェックリストの比較を実行します\n")
  checklist_filenames_and_sheetnames <- ExecCompare_fileNameAndSheetName(
    target_dirs$checklist_1,
    target_dirs$checklist_2
  )
  compare_checklist <- ExecCompare_values(
    target_dirs$checklist_1,
    target_dirs$checklist_2,
    checklist_filenames_and_sheetnames,
    compare_output_path)
}
# ------ main ------
ExecCompare()
