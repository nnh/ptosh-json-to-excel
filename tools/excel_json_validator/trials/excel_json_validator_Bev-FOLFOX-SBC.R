#' title
#' description
#' @file excel_json_validator_Bev-FOLFOX-SBC.R
#' @author Mariko Ohtsuka
#' @date 2025.5.16
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
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(STAT.blank?) && (ref('registration', 4)=='F')", "(registration,field4,性別)", df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "ref('registration', 4)=='F'", "(registration,field4,性別)", df_item$presence_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "ref('registration', 4)=='F'", "(registration,field4,性別)", df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == " ((ORRES.blank? && STAT == 'NOT DONE') || (ORRES.present? && STAT.blank?)) && (ref('registration', 4)=='F')", "(registration,field4,性別)", df_item$formula_if_references)

df_item_json <- df_item |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
for (row in 1:nrow(df_item_sheet)) {
  for (col in 1:ncol(df_item_sheet)) {
    if (df_item_json[row, col] != df_item_sheet[row, col]) {
      checkChecklist$item$checklist[row, col] <- paste0("NG: ", df_item_json[row, col], " != ", df_item_sheet[row, col])
    }
  }
}
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
