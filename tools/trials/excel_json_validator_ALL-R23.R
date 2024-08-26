#' title
#' description
#' @file excel_json_validator_ALL-R23.R
#' @author Mariko Ohtsuka
#' @date 2024.8.26
keep_objects <- c("jsonList", "sheetList")
rm(list = setdiff(ls(), keep_objects))
# ------ libraries ------
library(tidyverse)
library(here)
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
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 3\\)"), 
                                         "(registration,field3,性別)", 
                                         df_item$presence_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 3\\)"), 
                                        "(registration,field3,性別)", 
                                        df_item$formula_if_references)
df_item[177, 8] <- ifelse(df_item[177, 8] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
df_item[176, 10] <- ifelse(df_item[176, 10] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
df_item[177, 10] <- ifelse(df_item[177, 10] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
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
checkChecklist$action <- sheetList |> CheckAction()
###########
# display #
###########
checkChecklist$display <- sheetList |> CheckDisplay()
##########
# number #
##########
checkChecklist$number <- sheetList |> CheckNumber()
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
# alert #
#########
checkChecklist$alert <- sheetList |> CheckAlert()
#########
# title #
#########
checkChecklist$title <- sheetList |> CheckTitle()
############
# assigned #
############
checkChecklist$assigned <- sheetList |> CheckAssigned()
allr23Checklist <- checkChecklist