#' for test
#' description
#' @file issue8.R
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
ReplaceRef <- function(input_str, input_sheetname){
  if (is.na(input_str)){
    return(NA)
  }
  field_str <- input_str %>%
    gsub("f(\\d+)", "field\\1", .) %>%
      gsub("(,|\\s)(\\d+)\\)", "\\1field\\2)", .)
  sheetname <- input_str %>% str_match_all("(?<=ref\\(')(.*?)(?=')") %>% .[[1]]
  if (!str_detect(field_str, "field")){
    return(NA)
  }
  target_field <- field_str %>% str_extract_all(str_c("field", "\\d+")) %>% .[[1]] %>% unique()
  if (sheetname %>% length() > 0){
    sheetname <- sheetname[, 2]
  } else {
    sheetname <- input_sheetname
  }
  df_field_label <- target_field %>% map( ~ {
    target <- .
    fields <- aliasname_fields %>% filter(alias_name == sheetname & name == target)
    if (nrow(fields) == 0){
      return(NA)
    }
    res <- str_c("(", sheetname, ",", fields[1, "name"], ",", fields[1, "label"], ")")
    return(res)
  })
  if (length(df_field_label) == 0){
    return(NA)
  }
  return(paste0(df_field_label, collapse=""))
}
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
aliasname_fields <- json_files %>%
  map_df( ~ {
   json_file <- .$flattenJson
   field_items <- json_file$field_items %>% select(c("sheet_id", "name", "label"))
   field_items$alias_name <- json_file$alias_name
   return(field_items)
  })

# FieldItem::Articleのみ抽出
target_items <- map2(field_items, names(field_items), ~ {
  field_item <- .x
  alias_name <- .y
  presence_if_references <- field_item$validators.presence.validate_presence_if %>%
    map_vec( ~ ReplaceRef(., alias_name))
  formula_if_references <- field_item$validators.formula.validate_formula_if %>%
    map_vec( ~ ReplaceRef(., alias_name))
  references_after <- field_item$validators.date.validate_date_after_or_equal_to %>%
    map_vec( ~ ReplaceRef(., alias_name))
  references_before <- field_item$validators.date.validate_date_before_or_equal_to %>%
    map_vec( ~ ReplaceRef(., alias_name))

  res <- list(
    sheet_id=field_item$sheet_id,
    name=field_item$name,
    label=field_item$label,
    option.name=field_item$option.name,
    default_value=field_item$default_value,
    validators.presence.validate_presence_if=field_item$validators.presence.validate_presence_if,
    validators.formula.validate_formula_if=field_item$validators.formula.validate_formula_if,
    presence_if_references=presence_if_references,
    formula_if_references=formula_if_references,
    validators.formula.validate_formula_message=field_item$validators.formula.validate_formula_message,
    validators.date.validate_date_after_or_equal_to=field_item$validators.date.validate_date_after_or_equal_to,
    references_after=references_after,
    validators.date.validate_date_before_or_equal_to=field_item$validators.date.validate_date_before_or_equal_to,
    references_before=references_before,
    type=field_item$type
  ) %>% keep( ~ !is.null(.)) %>% data.frame()
  return(res)
}) %>% bind_rows() %>% filter(type == "FieldItem::Article")
# シート名を付与
target_items_names <- target_items %>% inner_join(jpname_aliasname, by="sheet_id") %>% distinct()
# チェック用データフレーム
df_compare <- target_items_names %>%
  select(c("jpname", "alias_name", "name", "label", "option.name", "default_value", "validators.presence.validate_presence_if", "presence_if_references", "validators.formula.validate_formula_if", "formula_if_references", "validators.formula.validate_formula_message", "validators.date.validate_date_after_or_equal_to", "references_after", "validators.date.validate_date_before_or_equal_to", "references_before"))
# checklist読み込み
df_target_checklist_path <- GetTargetDirs(NULL) %>% .$checklist_1
df_target <- openxlsx::read.xlsx(file.path(df_target_checklist_path, "checklist.xlsx"), sheet="item")
# 行や列の順序を考慮して2つのデータフレームが等しいかどうかを確認
# データフレームを比較して差分を取得
df_diff <- diff_data(df_compare, df_target)
render_diff(df_diff, "/Users/mariko/Downloads/issue8.html", view=F)
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
