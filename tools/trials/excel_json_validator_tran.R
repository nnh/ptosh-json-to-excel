#' title
#' description
#' @file excel_json_validator_tran.R
#' @author Mariko Ohtsuka
#' @date 2025.5.13
if (exists("keep_objects")) {
  rm(list=setdiff(ls(), keep_objects))
}
# ------ libraries ------
library(tidyverse, warn.conflicts=F)
library(here, warn.conflicts=F)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |>  GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item_json <- jsonSheetItemList$json |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList)
##########
# action #
##########
sheetList$action$id <- sheetList$action$id |> as.integer()
sheetList$action$field_item_id <- sheetList$action$field_item_id |> as.integer()
sheetAction <- sheetList[["action"]] |> arrange(id, field_item_id, codes)
jsonAction <- GetActionFromJson() |> arrange(id, field_item_id, codes)
checkChecklist$action <- CheckTarget(sheetAction, jsonAction)
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
