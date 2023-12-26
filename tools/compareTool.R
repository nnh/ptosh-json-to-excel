#' Compare tool.
#'
#' @file compareTool2.R
#' @author Mariko Ohtsuka
#' @date 2023.12.26
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(tools)
library(daff)
source(here("prg", "common_functions.R"), encoding="UTF-8")
# ------ constants ------
kInputPath1 <- here("output")
kInputPath2 <- "/Users/mariko/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/output Bev-FOLFOX-SBC"
# ------ functions ------
GetXlsxFileNames <- function(dir_path) {
  return(list.files(path=dir_path, pattern="\\.xlsx$", full.names=F))
}
CheckExcelFilesConsistency <- function(dir_path1, dir_path2) {
  # ディレクトリ内の.xlsxファイル一覧を取得
  fileNames1 <- dir_path1 %>% GetXlsxFileNames()
  fileNames2 <- dir_path2 %>% GetXlsxFileNames()
  # ファイルの数と名前を比較
  count1 <- length(fileNames1)
  count2 <- length(fileNames2)
  # ファイル数と名前が同じであるか確認
  if (identical(count1, count2) && identical(fileNames1, fileNames2)) {
    cat("OK:どちらのディレクトリにも、同じ名前の.xlsxファイルが同じ数だけある。\n")
    return(fileNames1)
  } else {
    cat("Directories have different numbers of .xlsx files or files with different names.\n")
    # 差異の情報を表示
    if (count1 != count2) {
      cat("File count difference:\n")
      cat("Directory 1:", count1, "files\n")
      cat("Directory 2:", count2, "files\n")
    }
    if (!identical(fileNames1, fileNames2)) {
      cat("File name difference:\n")
      cat("Files in Directory 1:\n", setdiff(fileNames1, fileNames2), "\n")
      cat("Files in Directory 2:\n", setdiff(fileNames2, fileNames1), "\n")
    }
    return(NULL)
  }
}
CheckExcelSheetsConsistency <- function(filename) {
  file_path1 <- file.path(kInputPath1, filename)
  file_path2 <- file.path(kInputPath2, filename)
  # Excelファイルを開く
  wb1 <- loadWorkbook(file_path1)
  wb2 <- loadWorkbook(file_path2)
  sheets1 <- names(wb1) %>% sort()
  sheets2 <- names(wb2) %>% sort()
  # シート名の比較
  if (!identical(sheets1, sheets2)) {
    cat("Excel files have different sheet names.\n")
    cat("Sheet names in", basename(file_path1), ":\n", sheets1, "\n")
    cat("Sheet names in", basename(file_path2), ":\n", sheets2, "\n")
    return(F)
  }
  return(T)
}

ExecCompare_fileNameAndSheetName <- function(){
  filenames <- CheckExcelFilesConsistency(kInputPath1, kInputPath2)
  if (is.null(filenames)){
    return(F)
  }
  for (filename in filenames){
    if (!CheckExcelSheetsConsistency(filename)){
      cat(filename, "is not consistent.\n")
      return(F)
    }
  }
  cat("OK:シート名が全て等しい\n")
  return(T)
}
CheckExcelValuesConsistency <- function(filename){
  file_path1 <- file.path(kInputPath1, filename)
  file_path2 <- file.path(kInputPath2, filename)
  sheetNames <- loadWorkbook(file_path1) %>% names()
  for (i in 1:length(sheetNames)){
    sheetName <- sheetNames[i]
    wb1 <- read.xlsx(file_path1, sheetName)
    wb2 <- read.xlsx(file_path2, sheetName)
    colnames1 <- wb1 %>% colnames() %>% sort()
    values1 <- wb1 %>% select(all_of(colnames1)) %>% mutate_all(., ~ ifelse(is.na(.), "", .)) %>% mutate_all(., ~ ifelse("\r\n", "\n", .))
    values2 <- wb2 %>% select(all_of(colnames1)) %>% mutate_all(., ~ ifelse(is.na(.), "", .)) %>% mutate_all(., ~ ifelse("\r\n", "\n", .))
    # データなしの時の判定
    if (identical(values2 %>% colnames(), c("alias_name", "jpname")) & nrow(values2) == 0 & nrow(values1) == 1){
      values2[1, 1] <- NA
      values2[1, 2] <- NA
    }
    daff_result <- diff_data(values1, values2)
    render_diff(daff_result, file.path(compare_output_path, paste0(filename, "_", sheetName, ".html")), view=F)
    if(!identical(values1, values2)){
      # 差異の表示（HTML 形式）
      daff_result %>% daff::render_diff()
      cat("Excel files have different values.\n")
      cat(sheetName, "\n")
      return(F)
    }
  }
  return(T)
}
ExecCompare_values <- function(){
  filenames <- GetXlsxFileNames(kInputPath1)
  for (filename in filenames){
    if (!CheckExcelValuesConsistency(filename)){
      cat(filename, "is not consistent.\n")
      return(F)
    }
  }
  cat("OK:全ての値が等しい")
  return(T)
}
# ------ main ------
result <- ExecCompare_fileNameAndSheetName()
if (result){
  output_foldername <- format(Sys.time(), "%Y%m%d%H%M%S") %>% paste0("compare_", .)
  compare_output_path <- CreateOutputFolder(here("output", output_foldername))
  result <- ExecCompare_values()
}
