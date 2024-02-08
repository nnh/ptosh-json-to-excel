#' for test
#' description
#' @file issue7.R
#' @author Mariko Ohtsuka
#' @date 2024.2.8
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(daff)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding="UTF-8")
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
# ------ constants ------
kInputFolderName <- "input"
# ------ functions ------
# ------ main ------
json_files <- ExecReadJsonFiles()
field_items <- json_files %>% map( ~ .$flattenJson$field_items)
jpname_aliasname <- json_files %>%
  map_df( ~ list(
      sheet_id=.$flattenJson$id,
      jpname=.$flattenJson$name,
      alias_name=.$flattenJson$alias_name
    )
  )
# FieldItem::Articleのみ抽出
target_items <- field_items %>%
  map_df( ~ list(
    sheet_id=.$sheet_id,
    option_id=.$option_id,
    type=.$type,
    option.name=.$option.name
  )
) %>% filter(!is.na(option_id)) %>% filter(type == "FieldItem::Article")
# シート名を付与
target_items_names <- target_items %>% inner_join(jpname_aliasname, by="sheet_id") %>% distinct()
# option.value
option_values <- map2(field_items, names(field_items), ~ {
  field_item <- .x
  alias_name <- .y
  option.values <- field_item$option.values %>%
    keep( ~ !is.null(.))
  if (is.null(option.values)){
    return(NULL)
  }
  res <- option.values %>% bind_rows()
  res$alias_name <- alias_name
  return(res)
}) %>% keep( ~ !is.null(.)) %>% bind_rows() %>% distinct()
# 列名の変更
current_colnames <- colnames(option_values)
# 各列のインデックスを取得
name_index <- which(current_colnames == "name")
seq_index <- which(current_colnames == "seq")
code_index <- which(current_colnames == "code")
is_usable_index <- which(current_colnames == "is_usable")
# 新しい列名に置き換え
current_colnames[name_index] <- "option.values_name"
current_colnames[seq_index] <- "option.values_seq"
current_colnames[code_index] <- "option.values_code"
current_colnames[is_usable_index] <- "option.values_is_usable"
# names() を使用して列名を置き換え
colnames(option_values) <- current_colnames
# 抽出条件はoption.values_is_usable is true
option_values_is_usable <- option_values %>% filter(option.values_is_usable)
# option.value
target_items_options <- option_values_is_usable %>%
  inner_join(target_items_names, by=c("alias_name", "option_id"))
# チェック用データフレーム
df_compare <- target_items_options %>%
  select(jpname, alias_name, option.name, option.values_name,
         option.values_seq, option.values_code, option.values_is_usable)
df_compare$option.values_seq <- df_compare$option.values_seq %>% as.numeric()
# checklist読み込み
df_target_checklist_path <- GetTargetDirs(NULL) %>% .$checklist_1
df_target <- openxlsx::read.xlsx(file.path(df_target_checklist_path, "checklist.xlsx"), sheet="option")
# 行や列の順序を考慮して2つのデータフレームが等しいかどうかを確認
# データフレームを比較して差分を取得
df_diff <- diff_data(df_compare, df_target)
render_diff(df_diff, "/Users/mariko/Downloads/issue7.html", view=F)
if(identical(df_compare, df_target)) {
  print("2つのデータフレームは等しいです。")
} else {
  print("2つのデータフレームは異なります。")
  for (i in 1:nrow(df_compare)){
    for (j in 1:ncol(df_compare)){
      if (!identical(df_compare[i, j], df_target[i, j])){
        print(df_compare[i, j])
        print(df_target[i, j])
        break
      }
    }
  }
}
