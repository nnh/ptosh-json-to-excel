#' for test
#'
#' @file issue6.R
#' @author Mariko Ohtsuka
#' @date 2024.2.7
rm(list=ls())
# ------ libraries ------
library(here)
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
# ------ target_dir ------
#kTargetFolder <- "20240124output_Bev_win"
kTargetFolder <- "20240123output_Bev"
#kTargetFolder <- "output ALL-B19"
target_dirs <- GetTargetDirs(kTargetFolder)
# ------ functions ------
issue6_ExecCompare_fileNameAndSheetName <- function(input_path1, input_path2){
  filenames <- CheckExcelFilesConsistency(input_path1, input_path2)
  if (is.null(filenames)){
    return(NULL)
  }
  result <- filenames %>% map( ~ {
    filename <- .
    res <- issue6_CheckExcelSheetsConsistency(filename, input_path1, input_path2)
    if (is.null(res)){
      cat(filename, "is not consistent.\n")
    }
    return(res)
  })
  names(result) <- filenames
  check_sheetnames <- result %>% map_lgl( ~ !is.null(.)) %>% all()
  if (!check_sheetnames){
    return(NULL)
  }
  cat("OK:シート名が全て等しい\n")
  return(result)
}
issue6_CheckExcelSheetsConsistency <- function(filename, input_path1, input_path2) {
  file_path1 <- file.path(input_path1, filename)
  file_path2 <- file.path(input_path2, filename)
  target_path <- c(file_path1, file_path2)
  sheet_names <- target_path %>% map( ~ loadWorkbook(.) %>% .$sheet_names %>% sort())
  # シート名が"*_sum"のものが削除されていればOKなので削除する
  sheet_names[[2]] <- sheet_names[[2]][!grepl("_sum$", sheet_names[[2]])]
  # シート名の比較
  if (!identical(sheet_names[[1]], sheet_names[[2]])) {
    cat("Excel files have different sheet names.\n")
    cat("Sheet names in", basename(file_path1), ":\n", sheet_names[[1]], "\n")
    cat("Sheet names in", basename(file_path2), ":\n", sheet_names[[2]], "\n")
    return(NULL)
  }
  return(sheet_names[[1]])
}
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
  checklist_filenames_and_sheetnames <- issue6_ExecCompare_fileNameAndSheetName(
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
