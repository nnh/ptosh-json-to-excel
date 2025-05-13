#' title
#' description
#' @file excel_json_validator_bev.R
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
df_item <- jsonSheetItemList$json
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 4\\)"), 
                                         "(registration,field4,性別)", 
                                         df_item$presence_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 4\\)"), 
                                        "(registration,field4,性別)", 
                                        df_item$formula_if_references)
df_item_json <- df_item |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList)
##########
# action #
##########
temp_action <- sheetList
temp_action$action$id <- temp_action$action$id |> as.integer()
temp_action$action$field_item_id <- temp_action$action$field_item_id |> as.integer()
checkChecklist$action <- temp_action |> CheckAction()
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
