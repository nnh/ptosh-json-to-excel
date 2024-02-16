#' for test
#' description
#' @file issue9.R
#' @author Mariko Ohtsuka
#' @date 2024.2.8
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(daff)
library(jsonlite)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding="UTF-8")
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
# ------ constants ------
kInputFolderName <- "input"
# ------ functions ------
# ------ main ------
json_files <- ExecReadJsonFiles()
field_items <- json_files %>% map( ~ .$flattenJson$field_items)
target_items <- json_files %>%
  map_df( ~ list(
      name=.$flattenJson$name,
      alias_name=.$flattenJson$alias_name,
      images_count=.$flattenJson$images_count
    )
  )
# チェック用データフレーム
df_compare <- target_items %>% as.data.frame()
df_compare$images_count <- df_compare$images_count %>% as.numeric()
# checklist読み込み
df_target_checklist_path <- GetTargetDirs(NULL) %>% .$checklist_1
df_target <- openxlsx::read.xlsx(file.path(df_target_checklist_path, "checklist.xlsx"), sheet="name")
# 行や列の順序を考慮して2つのデータフレームが等しいかどうかを確認
# データフレームを比較して差分を取得
df_diff <- diff_data(df_compare, df_target)
render_diff(df_diff, "/Users/mariko/Downloads/issue9.html", view=F)
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
