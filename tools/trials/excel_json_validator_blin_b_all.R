#' title
#' description
#' @file excel_json_validator_blin_b_all.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
if (exists("keep_objects")) {
  rm(list = setdiff(ls(), keep_objects))
}
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
source(here("tools", "excel_json_validator_common.R"), encoding = "UTF-8")
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

df_item_json <- df_item |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

df_item_json[318, 10] <- "(registration,field11,初発診断日)(registration,field2,生年月日)(allocationfac_100,field9,診断時白血球数（/uL）)(allocationfac_100,field16,NCI/Rome 分類)"
df_item_json$presence_if_references <- ifelse(df_item_json$validate_presence_if == "ref('registration', 3)=='M'&&STAT.blank?", "(registration,field3,性別)", df_item_json$presence_if_references)
df_item_json$presence_if_references <- ifelse(df_item_json$validate_presence_if == "ref('registration', 3)=='M'", "(registration,field3,性別)", df_item_json$presence_if_references)
df_item_json$formula_if_references <- ifelse(df_item_json$validate_formula_if == "ref('registration', 3)=='M'", "(registration,field3,性別)", df_item_json$formula_if_references)
df_item_json$formula_if_references <- ifelse(df_item_json$validate_formula_if == "((ORRES.blank? && STAT == 'NOT DONE') || (ORRES.present? && STAT.blank?))&&ref('registration', 3)=='M'", "(registration,field3,性別)", df_item_json$formula_if_references)
df_item_json$formula_if_references <- ifelse(df_item_json$validate_formula_if == "((ORRES.blank? && STAT == 'NOT DONE') || (ORRES.present? && STAT.blank?)) && (ref('registration', 3)=='F')", "(registration,field3,性別)", df_item_json$formula_if_references)
df_item_json$formula_if_references <- ifelse(df_item_json$validate_formula_if == "(ref('registration',3)=='M'&&f200=='N')||ref('registration',3)=='F'", "(registration,field3,性別)(screening_100,field200,妊娠可能な被験者である)", df_item_json$formula_if_references)
df_item_json[1147, 10] <- "(registration,field3,性別)"
df_item_json[1574, 10] <- "(registration,field3,性別)(lab_3000,field2,妊娠可能な被験者である)"
df_item_sheet <- jsonSheetItemList$sheet |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
sheetName <- "allocation"
allocation_sheet <- sheetList[[sheetName]] |>
  rename(!!!engToJpnColumnMappings[[sheetName]])
allocation_json <- GetAllocationFromJson(jsonList)
allocation_sheet$groups.if_references <- ifelse(is.na(allocation_sheet$groups.if_references), "", allocation_sheet$groups.if_references)
allocation_sheet$formula_field_references <- allocation_sheet$formula_field_references |> str_remove_all(" ")
allocation_json$formula_field_references <- allocation_json$formula_field_references |> str_remove_all(" ")
allocation_json$formula_field_references <- ifelse(allocation_json$formula_field == "ref('allocationfac_100', 16) =='SR'", "(allocationfac_100,field16,NCI/Rome分類)=='SR'", allocation_json$formula_field_references)
checkChecklist$allocation <- CheckTarget(allocation_sheet, allocation_json)
##########
# action #
##########
sheetList$action$`-` <- sheetList$action$`-` |> as.integer()
sheetList$action$`--` <- sheetList$action$`--` |> as.integer()
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
sheetName <- "comment"
commentSheet <- sheetList[[sheetName]] |>
  rename(!!!engToJpnColumnMappings[[sheetName]])
commentJson <- GetContentFromJson()
commentJson[12, "content"] <- commentJson[12, "content"] %>% CleanTextForComment()
commentSheet[12, "content"] <- commentSheet[12, "content"] %>% CleanTextForComment()
checkChecklist$content <- CheckTarget(commentSheet, commentJson)
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
