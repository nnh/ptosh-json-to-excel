#' title
#' description
#' @file excel_json_validator_TAS0728-HER2.R
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
df_item$references_after <- ifelse(
  df_item$validate_date_after_or_equal_to == "ref('registration', 1)",
  "(registration,field1,同意取得日)",
  df_item$references_after
)
df_item$references_after <- ifelse(
  df_item$validate_date_after_or_equal_to == "ref('registration', 2)",
  "(registration,field2,生年月日)",
  df_item$references_after
)
df_item$formula_if_references <- ifelse(
  df_item$validate_formula_if == "(ref('registration',3)=='M' && field522=='N') || ref('registration',3)=='F'", "(registration,field3,性別)(lab_10000,field522,妊娠可能な被験者である)",
  df_item$formula_if_references
)
df_item$formula_if_references <- ifelse(
  df_item$validate_formula_if == "(ref('registration',3)=='M' && field301=='N') || ref('registration',3)=='F'", "(registration,field3,性別)(lab_30000,field301,妊娠可能な被験者である)",
  df_item$formula_if_references
)
df_item_json <- df_item |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet[1167, 10] <- ifelse(
  df_item_sheet[1167, 10] == "(lab_10000,field522,妊娠可能な被験者である)(registration,field3,性別)",
  "(registration,field3,性別)(lab_10000,field522,妊娠可能な被験者である)",
  NA
)
df_item_sheet[7778, 10] <- ifelse(
  df_item_sheet[7778, 10] == "(lab_30000,field301,妊娠可能な被験者である)(registration,field3,性別)",
  "(registration,field3,性別)(lab_30000,field301,妊娠可能な被験者である)",
  NA
)
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
sheetName <- "comment"
commentSheet <- sheetList[[sheetName]] |>
  rename(!!!engToJpnColumnMappings[[sheetName]])
commentJson <- GetContentFromJson()
commentJson[2, "content"] <- commentJson[2, "content"] %>% CleanTextForComment()
commentSheet[2, "content"] <- commentSheet[2, "content"] %>% CleanTextForComment()
commentJson[3, "content"] <- commentJson[3, "content"] %>% CleanTextForComment()
commentSheet[3, "content"] <- commentSheet[3, "content"] %>% CleanTextForComment()
checkChecklist$content <- CheckTarget(commentSheet, commentJson)
###############
# explanation #
###############
sheetName <- "explanation"
explanationSheet <- sheetList[[sheetName]] |>
  rename(!!!engToJpnColumnMappings[[sheetName]])
explanationJson <- GetExplanationFromJson()
targetRow <- 22
explanationSheet[targetRow, "description"] <- explanationSheet[targetRow, "description"] %>% CleanTextForComment()
explanationJson[targetRow, "description"] <- explanationJson[targetRow, "description"] %>% CleanTextForComment()
checkChecklist$explanation <- CheckTarget(explanationSheet, explanationJson)
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
