#' Functions for compare tool.
#'
#' @file compareTool_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.2.19
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(tools)
library(daff)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
# ------ constants ------
kOutputFolderName <- "output"
kListFolderName <- "list"
kJpnameAliasName <- c("jpname", "alias_name")
kNameAliasName <- c("name", "alias_name")
# ------ functions ------
GetTargetDirs <- function(){
  dir_list <- list.dirs(here(kOutputFolderName), full.names=F, recursive=F)
  target_folder_name <- dir_list %>% grep("^output_\\d+$", ., value=T) %>%
    sub("^output_(\\d+)$", "\\1", .) %>% as.numeric() %>% max() %>% str_c("output_", .)
  json_to_excel_1 <- here(kOutputFolderName, target_folder_name)
  file_names <- json_to_excel_1 %>% list.files(full.names=F, include.dirs=F)
  os_info <- tolower(Sys.info()["sysname"])
  kAllB19FileName <- "blin1.xlsx"
  if (os_info == "windows") {
    json_to_excel_2 <- "C:/Users/Mariko/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/"
    compare_target <- ifelse(any(file_names == kAllB19FileName), "20240208outputALL-B19", "20240208output_Bev_win")
  } else {
    json_to_excel_2 <- "/Users/mariko/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/"
    compare_target <- ifelse(any(file_names == kAllB19FileName), "20240208outputALL-B19", "20240208output_Bev")
  }
  json_to_excel_2 <- json_to_excel_2 %>% str_c(compare_target)
  checklist_1 <- here(kOutputFolderName, target_folder_name, kListFolderName)
  checklist_2 <- str_c(json_to_excel_2, "/", kListFolderName)
  return(
    list(
      target_dirs=list(
        json_to_excel_1=json_to_excel_1,
        json_to_excel_2=json_to_excel_2,
        checklist_1=checklist_1,
        checklist_2=checklist_2
      ),
      kTargetFolder=compare_target
    )
  )
}
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
    cat(str_c("OK:どちらのディレクトリにも、同じ名前の.xlsxファイルが同じ数だけある。ファイル数：", count1, "\n"))
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
CheckExcelSheetsConsistency <- function(filename, input_path1, input_path2) {
  file_path1 <- file.path(input_path1, filename)
  file_path2 <- file.path(input_path2, filename)
  target_path <- c(file_path1, file_path2)
  sheet_names <- target_path %>% map( ~ loadWorkbook(.) %>% .$sheet_names %>% sort())
  # シート名の比較
  if (!identical(sheet_names[[1]], sheet_names[[2]])) {
    cat("Excel files have different sheet names.\n")
    cat("Sheet names in", basename(file_path1), ":\n", sheet_names[[1]], "\n")
    cat("Sheet names in", basename(file_path2), ":\n", sheet_names[[2]], "\n")
    return(NULL)
  }
  return(sheet_names[[1]])
}

ExecCompare_fileNameAndSheetName <- function(input_path1, input_path2){
  filenames <- CheckExcelFilesConsistency(input_path1, input_path2)
  if (is.null(filenames)){
    return(NULL)
  }
  result <- filenames %>% map( ~ {
    filename <- .
    res <- CheckExcelSheetsConsistency(filename, input_path1, input_path2)
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
CheckDiffColumns <- function(colnames1, colnames2){
  target <- list(
    list(colnames1, colnames2),
    list(colnames2, colnames1)
  )
  for (i in 1:length(target)){
    diffColnames <- setdiff(target[[i]][[1]], target[[i]][[2]])
    if (length(diffColnames) > 0){
      cat("Excel files have different columns.\n")
      col1 <<- target[[i]][1]
      col2 <<- target[[i]][2]
      return(diffColnames)
    }
  }
  return(NULL)
}
GetSheetNameList <- function(inputPath, filename){
  return(file.path(inputPath, filename) %>% loadWorkbook() %>% names() %>% sort())
}
GetColnamesForCompare <- function(values, sheetName){
  if (sheetName != "name"){
    col_names <- values %>% colnames() %>% sort() %>% setdiff(kJpnameAliasName) %>% c(kJpnameAliasName, .)
  } else {
    col_names <- values %>% colnames() %>% sort() %>% setdiff(kNameAliasName) %>% c(kNameAliasName, .)
  }
  return(col_names)
}
GetValuesForCompare <- function(wb, col_names){
  values <- wb %>%
    select(all_of(col_names)) %>%
      mutate_all(., ~ ifelse(is.na(.), "", .))
  return(values)
}
CheckExcelValuesConsistency <- function(value1, value2, sheetname, filename, compare_output_path=NULL){
  # 列名チェック
  colnames1 <- value1 %>% GetColnamesForCompare(sheetname)
  colnames2 <- value2 %>% GetColnamesForCompare(sheetname)
  if (!is.null(CheckDiffColumns(colnames1, colnames2))){
    cat(filename)
    cat(", ")
    cat(sheetname)
    cat("\n file1:")
    cat(colnames1 %>% paste0(collapse = ","))
    cat("\n file2:")
    cat(colnames2 %>% paste0(collapse = ","))
    cat("\n")
    return(F)
  }
  col_names <- colnames1
  if (nrow(value1) == 0 & nrow(value2) == 0){
    return(T)
  }
  # 値チェック
  check_values1 <- value1 %>% GetValuesForCompare(col_names)
  check_values2 <- value2 %>% GetValuesForCompare(col_names)
  # 出力パッケージの違いによる差異を吸収
  if (nrow(check_values2) == 0 & nrow(check_values1) == 1){
    for (col in 1:ncol(check_values1)){
      if (check_values1[1, col] == "" | is.na(check_values1[1, col])){
        check_values2[1, col] <- check_values1[1, col]
      }
    }
  }
  # 改行コード等の調整
  for (row in 1:nrow(check_values2)){
    for (col in 1:ncol(check_values2)){
      if (str_detect(check_values2[row, col], "\r\n")){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all("\r\n", "\n") %>% str_replace_all("\\n+", "\n")
      }
      if (str_detect(check_values2[row, col], 'xml:space="preserve">')){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all('xml:space="preserve">', "")
      }
      if (str_detect(check_values2[row, col], '&amp;')){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all('&amp;', "&")
      }
      if (str_detect(check_values2[row, col], '&lt;')){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all('&lt;', "<")
      }
      if (str_detect(check_values2[row, col], '&gt;')){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all('&gt;', ">")
      }
      if (str_detect(check_values2[row, col], '<br /<')){
        check_values2[row, col] <- check_values2[row, col] %>% str_replace_all('<br /<', "<br />")
      }
    }
  }
  daff_result <- diff_data(check_values1, check_values2)
  if (!is.null(compare_output_path)){
    render_diff(daff_result, file.path(compare_output_path, paste0(filename, "_", sheetname, ".html")), view=F)
  }
  if(!identical(check_values1, check_values2)){
    cat("Excel files have different values.\n")
    cat(filename, "\n")
    cat(sheetname, "\n")
    diff_value1 <<- check_values1
    diff_value2 <<- check_values2
    return(F)
  }
  return(T)
}
ExecCompare_values <- function(file_path1, file_path2, filenames_and_sheetnames, compare_output_path){
  filenames <- names(filenames_and_sheetnames)
  sheetnames <- filenames_and_sheetnames
  for (i in 1:length(filenames_and_sheetnames)){
    filename <- filenames[i]
    target_file_path1 <- file.path(file_path1, filename)
    target_file_path2 <- file.path(file_path2, filename)
    wb1 <- target_file_path1 %>% loadWorkbook()
    wb2 <- target_file_path2 %>% loadWorkbook()
    checkEqual <- all.equal(wb1, wb2)
    if (!isTRUE(checkEqual)){
      for (j in 1:length(sheetnames[[i]])){
        sheetname <- sheetnames[[i]][j]
        value1 <- target_file_path1 %>% read.xlsx(sheetname, na.strings="test")
        value2 <- target_file_path2 %>% read.xlsx(sheetname, na.strings="test")
        if (!CheckExcelValuesConsistency(value1, value2, sheetname, filename, compare_output_path)){
          cat(filename, "is not consistent.\n")
          return(F)
        }
      }
    }
  }
  cat("OK:全ての値が等しい\n")
  return(T)
}
