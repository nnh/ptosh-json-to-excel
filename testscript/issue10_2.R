#' title
#' description
#' @file issue10_2.R
#' @author Mariko Ohtsuka
#' @date 2024.2.16
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(daff)
library(openxlsx)
library(testthat)
library(jsonlite)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
# ------ constants ------
kTrialName_bev <- "bev"
kTrialName_allb19 <- "all_b19"
kIssue7Key <- c("alias_name", "option.name", "option.values_code")
# ------ functions ------
ReplaceCheckValue <- function(check_value){
  if (is.na(check_value)){
    return(check_value)
  }
  if (str_detect(check_value, "\r\n")){
    check_value <- check_value %>% str_replace_all("\r\n", "\n") %>% str_replace_all("\\n+", "\n")
  }
  if (str_detect(check_value, 'xml:space="preserve">')){
    check_value <- check_value %>% str_replace_all('xml:space="preserve"> ', "")
    check_value <- check_value %>% str_replace_all('xml:space="preserve">', "")
  }
  if (str_detect(check_value, '&amp;')){
    check_value <- check_value %>% str_replace_all('&amp;', "&")
  }
  if (str_detect(check_value, '&lt;')){
    check_value <- check_value %>% str_replace_all('&lt;', "<")
  }
  if (str_detect(check_value, '&gt;')){
    check_value <- check_value %>% str_replace_all('&gt;', ">")
  }
  if (str_detect(check_value, '<br /<')){
    check_value <- check_value %>% str_replace_all('<br /<', "<br />")
  }
  return(check_value)
}
Issue10_2_ReadJsonFiles <- function(input_path){
  json_filenames <- list.files(input_path, pattern="*.json", full.names=F)
  json_files <- json_filenames %>% map( ~ {
    rawJson <- file.path(input_path, .) %>% read_json()
    flattenJson <- file.path(input_path, .) %>% fromJSON(flatten=T)
    return(list(rawJson=rawJson, flattenJson=flattenJson))
  })
  names(json_files) <- json_filenames %>% str_remove(., ".json")
  return(json_files)
}
ExecTestIssue6 <- function(trial_name){
  test_issue <- "issue6 修正後のシートから、fielditems_sum, option_sum, allocation_sumのみが削除されていればOK"
  bef_sheetname <- path_list[[trial_name]]$before %>% openxlsx::getSheetNames()
  compare_sheetname <- bef_sheetname[-which(bef_sheetname %in% c("fielditems_sum", "option_sum", "allocation_sum"))]
  aft_sheetname <- path_list[[trial_name]]$after %>% openxlsx::getSheetNames()
  print(test_issue)
  test_that(test_issue, {
    expect_equal(compare_sheetname, aft_sheetname)
  })
  return(identical(compare_sheetname, aft_sheetname))
}
ExecReplaceText <- function(df1, df2, target_colname, joinKey){
  replace_target <- df1 %>% anti_join(df2, by=joinKey)
  temp <- replace_target[[target_colname]] %>% map( ~ ReplaceCheckValue(.)) %>% unlist()
  replace_target <- replace_target %>% select(-all_of(target_colname))
  replace_target[[target_colname]] <- temp
  return(replace_target)
}
ExecIssue7Sort <- function(df, issue7Key){
  return(df %>% arrange(!!sym(issue7Key[1]), !!sym(issue7Key[2]), !!sym(issue7Key[3])))
}
ExecIssue7Merge <- function(df_source, df_compare, issue7Key, df_merge){
  remove_target <- df_source %>% anti_join(df_compare, by=issue7Key)
  df1 <- df_source %>% anti_join(remove_target, by=issue7Key)
  res <- df1 %>% bind_rows(df_merge) %>% ExecIssue7Sort(issue7Key)
  return(res)
}
ExecTestIssue7 <- function(trial_name, field_items_not_article){
  issue7Key <- c("alias_name", "option.name", "option.values_code")
  test_issue <- "issue7 修正後のoptionシートで削除されているものが全てtype:'FieldItem::Article'以外ならばOK"
  before_option <- path_list[[trial_name]]$before %>% openxlsx::read.xlsx(sheet="option", na.strings = "NA")
  after_option <- path_list[[trial_name]]$after %>% openxlsx::read.xlsx(sheet="option", na.strings = "NA")
  compare_option <- before_option %>%
    anti_join(after_option, by=issue7Key) %>%
    select(all_of(issue7Key)) %>% distinct() %>%
    ExecIssue7Sort(issue7Key)
  # エスケープされていないコードが一致しないのでそれを洗い出してサニタイズする
  df_check <- ExecReplaceText(compare_option, field_items_not_article, issue7Key[3], issue7Key) %>%
    ExecIssue7Sort(issue7Key)
  # データフレーム全体の比較
  target <- ExecIssue7Merge(compare_option, field_items_not_article, issue7Key, df_check)
  if (trial_name == kTrialName_bev){
    for (i in 1:nrow(compare_option)){
      compare_option[i, issue7Key[3]] <- compare_option[i, issue7Key[3]] %>% ReplaceCheckValue()
    }
    compare_option <- compare_option %>% ExecIssue7Sort(issue7Key)
  }
  print(test_issue)
  test_that(test_issue, {
    expect_equal(compare_option, target)
  })
  return(identical(compare_option, target))
}
ExecTestIssue8 <- function(trial_name, field_items_not_article){
  issue8Key <- c("alias_name", "name")
  field_items_not_article <- field_items_not_article %>% select(all_of(issue8Key)) %>% arrange(issue8Key[1], issue8Key[2])
  test_issue <- "issue8 修正後のitemシートで削除されているものが全てtype:'FieldItem::Article'以外ならばOK"
  before_item <- path_list[[trial_name]]$before %>% openxlsx::read.xlsx(sheet="item")
  after_item <- path_list[[trial_name]]$after %>% openxlsx::read.xlsx(sheet="item")
  compare_item <- before_item %>%
    anti_join(after_item, by=issue8Key) %>%
      select(all_of(issue8Key)) %>% arrange(issue8Key[1], issue8Key[2])
  print(test_issue)
  test_that(test_issue, {
    expect_equal(compare_item, field_items_not_article)
  })
  return(identical(compare_item, field_items_not_article))
}
ExecTestIssue9 <- function(trial_name){
  test_issue <- "issue9 修正後のnameシートから該当の列が削除されていればOK"
  before_name <- path_list[[trial_name]]$before %>% openxlsx::read.xlsx(sheet="name")
  after_name <- path_list[[trial_name]]$after %>% openxlsx::read.xlsx(sheet="name")
  compare_name <- before_name %>% select(c("name", "alias_name", "images_count"))
  print(test_issue)
  test_that(test_issue, {
    expect_equal(compare_name, after_name)
  })
  return(identical(compare_name, after_name))
}

DiffIssue10 <- function(trial_name){
  input_json <- jsonfiles[[trial_name]]
  if (!ExecTestIssue6(trial_name)){
    return()
  }
  field_items_not_article <- input_json %>%
    map( ~ .$rawJson$field_items %>% keep( ~ .$type != "FieldItem::Article")) %>%
      keep( ~ length(.) > 0)
  issue7_field_items <- field_items_not_article %>% map2(., names(.), ~ {
    alias_name <- .y
    field_items <- .x
    res <- field_items %>% map( ~ {
      option <- .$option
      if (is.null(option)){
        return(option)
      }
      option.name <- option$name
      option.values <- option$values %>% map( ~ {
        if (!.$is_usable){
          return(NULL)
        }
        return(c(alias_name=alias_name, option.name=option.name, option.values_code=.$code))
      }) %>% keep( ~ !is.null(.))
      return(option.values)
    }) %>% keep( ~ !is.null(.)) %>% bind_rows() %>% distinct()
    return(res)
  }) %>% keep( ~ length(.) > 0) %>% bind_rows() %>%
    arrange(alias_name, option.name, option.values_code) %>% as.data.frame()
  if (!ExecTestIssue7(trial_name, issue7_field_items)){
    return()
  }
  issue8_field_items <- field_items_not_article %>% map2_df(., names(.), ~ {
    alias_name <- .y
    field_items <- .x
    res <- field_items %>% map_df( ~ c(name=.$name, type=.$type))
    res$alias_name <- alias_name
    return(res)
  }) %>% as.data.frame()
  if (!ExecTestIssue8(trial_name, issue8_field_items)){
    return()
  }
  if (!ExecTestIssue9(trial_name)){
    return()
  }
  return()
}
# ------ main ------
# issue6〜11の修正適用前後のテストファイルのdiffをとる
path_list <- list()
path_list[[kTrialName_bev]] <- list(
  before="~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240123output_Bev/list/checklist.xlsx",
  after="~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240208output_Bev/list/checklist.xlsx"
)
path_list[[kTrialName_allb19]] <- list(
  before="~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/output ALL-B19/list/checklist.xlsx",
  after="~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240208outputALL-B19/list/checklist.xlsx"
)
jsonfiles <- list()
jsonfiles[[kTrialName_bev]] <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/Bev-FOLFOX-SBC" %>%
  Issue10_2_ReadJsonFiles()
jsonfiles[[kTrialName_allb19]] <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/ALL-B19" %>%
  Issue10_2_ReadJsonFiles()
print("*** ALL-B19 ***")
DiffIssue10(kTrialName_allb19)
print("*** Bev-FOLFOX-SBC ***")
DiffIssue10(kTrialName_bev)
