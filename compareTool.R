#' Compare output results.
#'
#' @file compareTool.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(readxl)
# ------ constants ------
kInputPath1 <- "~/Downloads"
kInputPath2 <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/output Bev-FOLFOX-SBC"
# ------ functions ------
ReadAllSheets <- function(file_path){
  sheetnames <- file_path %>% excel_sheets()
  res <- sheetnames %>% map( ~ read_excel(file_path, sheet=.))
  names(res) <- sheetnames
  return(res)
}
BindOutputText <- function(text_list){
  print_text <- NULL
  information_text <- NULL
  warning_text <- NULL
  for (fileIndex in 1:length(text_list)){
    target <- text_list[[fileIndex]]
    target_name <- names(text_list)[[fileIndex]]
    for (sheetIndex in 1:length(target)){
      if (!is.null(target[[sheetIndex]]$print_text)){
        print_text <- append(print_text, str_c(target_name, "|", target[[sheetIndex]]$print_text))
      }
      if (!is.null(target[[sheetIndex]]$information_text)){
        information_text <- append(information_text, str_c(target_name, "|", target[[sheetIndex]]$information_text))
      }
      if (!is.null(target[[sheetIndex]]$warning_text)){
        warning_text <- append(warning_text, str_c(target_name, "|", target[[sheetIndex]]$warning_text))
      }
    }
  }
  return(list(print_text=print_text, information_text=information_text, warning_text=warning_text))
}
CheckRows <- function(file1, file2, sheetname){
  sheet1 <- file1[[sheetname]]
  sheet2 <- file2[[sheetname]]
  information_text <- NULL
  print_text <- NULL
  if (nrow(sheet1) != nrow(sheet2)){
    print_text <- str_c("!!!!!!!!!!行数不一致：", sheetname)
  } else if (nrow(sheet1) == 0){
    information_text <- str_c(sheetname, "：データなし")
  }
  return(list(print_text=print_text, information_text=information_text))
}
CheckValue <- function(file1, file2, sheetname){
  sheet1 <- file1[[sheetname]]
  sheet2 <- file2[[sheetname]]
  # 各要素の一致不一致を確認する
  print_text <- NULL
  warning_text <-NULL
  for (column_count in 1:ncol(sheet1)){
    target_colname <- colnames(sheet1)[column_count]
    for (row_count in 1:nrow(sheet1)){
      if (!is.na(sheet1[row_count, target_colname]) & !is.na(sheet2[row_count, target_colname])){
        value1 <- sheet1[row_count, target_colname]
        value2 <- sheet2[row_count, target_colname]
        if (value1 != value2){
          # 改行コードの違いだけならwarningにつっこんでおく
          if (str_replace_all(value1, "\r\n", "\n") != str_replace_all(value2, "\r\n", "\n")){
            errorText <- str_c("!!!!!!!!!!値不一致",
                               "：シート名：", sheetname,
                               "：列名：", target_colname, "：",row_count, "行目:",
                               "A:", sheet1[row_count, target_colname],
                               "|",
                               "B:", sheet2[row_count, target_colname])
            print_text <- append(print_text, errorText)
          } else {
            warning_text <<- str_c("!!!!!!!!!!値不一致",
                               "：シート名：", sheetname,
                               "：列名：", target_colname, "：",row_count, "行目:",
                               "A:", sheet1[row_count, target_colname],
                               "|",
                               "B:", sheet2[row_count, target_colname]) %>% append(warning_text, .)
          }
        }
      } else if (is.na(sheet1[row_count, target_colname]) & !is.na(sheet2[row_count, target_colname])) {
        errorText <- str_c("!!!!!!!!!!値不一致",
                           "：シート名：", sheetname,
                           "：列名：", target_colname, "：",row_count, "行目:",
                           "A:", "NA",
                           "|",
                           "B:", sheet2[row_count, target_colname])
        print_text <- append(print_text, errorText)
      } else if (!is.na(sheet1[row_count, target_colname]) & is.na(sheet2[row_count, target_colname])) {
        errorText <- str_c("!!!!!!!!!!値不一致",
                           "：シート名：", sheetname,
                           "：列名：", target_colname, "：",row_count, "行目:",
                           "A:", sheet1[row_count, target_colname],
                           "|",
                           "B:", "NA")
        print_text <- append(print_text, errorText)
      }
    }
  }
  information_text <- ifelse(is.null(print_text), str_c(sheetname, "：値一致"), print_text)
  res <- list(print_text=print_text, information_text=information_text, warning_text=warning_text)
  return(res)
}
CheckColumnEqual <- function(file1, file2, sheetname){
  sheet1 <- file1[[sheetname]]
  sheet2 <- file2[[sheetname]]
  sheet1_colnames <- sheet1 %>% colnames()
  sheet2_colnames <- sheet2 %>% colnames()
  sheet1_only_elements <<- setdiff(sheet2_colnames, sheet1_colnames)
  sheet2_only_elements <<- setdiff(sheet1_colnames, sheet2_colnames)
  res <- NULL
  if (length(sheet1_only_elements) != 0 | length(sheet2_only_elements != 0)){
    output_1 <- ifelse(length(sheet2_only_elements) == 0, "該当なし", paste(sheet2_only_elements))
    output_2 <- ifelse(length(sheet1_only_elements) == 0, "該当なし", paste(sheet1_only_elements))
    # 列が不一致の場合
    res <- list(
      str_c("**** ", sheetname, " ****Aに含まれない列****"),
      output_2,
      str_c("**** ", sheetname, " ****Bに含まれない列****"),
      output_1
    )
  }
  return(res)
}
CheckSheets <- function(all_sheet_names, original_file, comparison_file){
  res <- all_sheet_names %>% map( ~ {
    information_text <- NULL
    print_text <- NULL
    warning_text <- NULL
    sheetname <- .
    # 列名が一致しているか
    resCheckColumnEqual <- CheckColumnEqual(original_file, comparison_file, sheetname)
    if (!is.null(resCheckColumnEqual)){
      print_text <- append(print_text, resCheckColumnEqual)
    } else {
      information_text <- append(information_text, str_c(sheetname, "：列名一致"))
      # 行数が一致しているか
      resCheckRows <- CheckRows(original_file, comparison_file, sheetname)
      print_text <- append(print_text, resCheckRows$print_text)
      information_text <- append(information_text, resCheckRows$information_text)
      if (is.null(resCheckRows$print_text) & is.null(resCheckRows$information_text)){
        information_text <- append(information_text, str_c(sheetname, "：行数一致"))
        # 行数、列名が一致した場合、各要素の一致不一致を確認する
        resCheckValue <- CheckValue(original_file, comparison_file, sheetname)
        print_text <- append(print_text, resCheckValue$print_text)
        information_text <- append(information_text, resCheckValue$information_text)
        warning_text <- append(warning_text, resCheckValue$warning_text)
      }
    }
    return(list(print_text=print_text, information_text=information_text, warning_text=warning_text))
  })
  return(res)
}
CheckFiles <- function(compare_file_list, original_idx, comparison_idx){
  res <- compare_file_list %>% map( ~ {
    print_text <- NULL
    original_file <- .[[original_idx]]
    comparison_file <- .[[comparison_idx]]
    all_sheet_names <- names(original_file)
    resCheckSheets <- CheckSheets(all_sheet_names, original_file, comparison_file)
    return(resCheckSheets)
  })
  return(res)
}
CheckDiff <- function(compare_file_list, execFlag){
  original_idx <- ifelse(execFlag, 1, 2)
  comparison_idx <- ifelse(execFlag, 2, 1)
  text_list <- NULL
  information_text <- NULL
  resCheckFiles <- CheckFiles(compare_file_list, original_idx, comparison_idx)
  return(resCheckFiles)
}
# ------ main ------
input_path1 <- kInputPath1
input_path2 <- kInputPath2
list_files <- input_path1 %>% list.files(pattern=".xlsx")
compare_file_list <- list_files %>% map( ~ {
  target_file_name <- .
  original_file <- str_c(input_path1, "/", target_file_name) %>% ReadAllSheets()
  comparison_file <- str_c(input_path2, "/", target_file_name) %>% ReadAllSheets()
  return(list(original_file, comparison_file))
})
names(compare_file_list) <- list_files
check1 <- CheckDiff(compare_file_list, T)
check2 <- CheckDiff(compare_file_list, F)
check1_text_list <- BindOutputText(check1)
check2_text_list <- BindOutputText(check2)
print("オモテ")
print(check1_text_list$print_text)
print("ウラ")
print(check2_text_list$print_text)
