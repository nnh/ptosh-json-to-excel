#' title
#' description
#' @file excel_json_validator_allb19.R
#' @author Mariko Ohtsuka
#' @date 2025.6.26
if (exists("keep_objects")) {
  rm(list = setdiff(ls(), keep_objects))
}
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |> GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item <- jsonSheetItemList$json
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)",
  "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)",
  df_item$formula_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)",
  "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)",
  df_item$presence_if_references
)
df_item_json <- df_item |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

for (row in 1:nrow(df_item_sheet)) {
  for (col in 1:ncol(df_item_sheet)) {
    if (df_item_sheet[row, col] != df_item_json[row, col]) {
      stop(
        sprintf(
          "Mismatch found at row %d, column %d: sheet='%s', json='%s'",
          row, col, df_item_sheet[row, col], df_item_json[row, col]
        )
      )
    }
  }
}

checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList)
##########
# action #
##########
checkChecklist$action <- sheetList |> CheckAction()
###########
# display #
###########
checkChecklist$display <- sheetList |> CheckDisplay()
########
# name #
########
checkChecklist$name <- sheetList |> CheckName(jsonList)
##########
# option #
##########
checkChecklist$option <- sheetList |> CheckOption()
###########
# comment #
###########
checkChecklist$content <- sheetList |> CheckContent()
###############
# explanation #
###############
checkChecklist$explanation <- sheetList |> CheckExplanation()
############
# presence #
############
checkChecklist$presence <- sheetList |> CheckPresence()
##########
# master #
##########
checkChecklist$master <- sheetList |> CheckMaster()
#########
# visit #
#########
checkChecklist$visit <- sheetList |> CheckVisit()
#########
# title #
#########
checkChecklist$title <- sheetList |> CheckTitle()
############
# assigned #
############
checkChecklist$assigned <- sheetList |> CheckAssigned()
##############
# limitation #
##############
checkChecklist$limitation <- CheckLimitation(sheetList, jsonList)
########
# date #
########
# checkChecklist$date <- CheckDate(sheetList, jsonList)
