#' test script
#'
#' @file excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2026.1.7
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
kAliasNameJapaneseColumnName <- "シート名英数字別名"
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
keep_objects <- c("keep_objects", "target_json", "sheetList", "trialName", "kTrialNames", "kAliasNameJapaneseColumnName")
kTrialNames <- c("Bev-FOLFOX-SBC", "AML224-FLT3-ITD", "ALL-B19")
kTrialNames <- c("ALL-B19")
# ------ functions ------
ExecExcelJsonValidator <- function(trialName) {
  if (exists("keep_objects")) {
    rm(list = setdiff(ls(), keep_objects))
  }
  library(tidyverse, warn.conflicts = F)
  library(here, warn.conflicts = F)
  source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
  kOptions <- "options"
  sheetOrders <<- target_json[["sheet_orders"]] %>%
    map(~ tibble(sheet = .x[["sheet"]], seq = .x[["seq"]])) %>%
    bind_rows() %>%
    arrange(seq)
  fieldOrders <<- GetFieldOrders()
  sheetAndFieldOrders <<- GetSheetAndFieldOrders(sheetOrders, c("alias_name" = "sheet"))
  isVisit <<- GetVisitInfo()
  jpNameAndAliasName <<- target_json |> GetNameAndAliasNameByJson()
  jpNameAndGroup <<- GetNameAndGroupByJson()
  fieldItems <<- target_json |> GetFieldItemsByJsonList()
  fieldItems_not_in_visitGroups <<- fieldItems[!names(fieldItems) %in% visitGroups[["alias_name"]]]
  fieldItems_in_visitGroups <<- fieldItems[names(fieldItems) %in% visitGroups[["alias_name"]]]
  options_json <<- target_json[[kOptions]]
  fieldInfoForGetReference <<- GetFieldInfoForGetRef()

  # シート並び順のチェックを実行
  checkSheetNames <- sheetList |> names()
  sheetSortOrders <- c("item_visit", "item_nonvisit", "visit", "allocation", "limitation", "date", "option", "name", "master", "assigned")
  if (!identical(sheetSortOrders, checkSheetNames)) {
    print(checkSheetNames)
    stop(str_c("Sheet order is incorrect in trial: ", trialName))
  }
  checkChecklist <- list()
  # ########
  # # item #
  # ########
  sheetName <- "item_nonvisit"
  checkChecklist[[sheetName]] <- sheetList |> CheckItemNonVisit(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##################
  # # item_visit_old #
  # ##################
  sheetName <- "item_visit_old"
  itemVisitData <<- GetItemVisitOldFromJson()
  sheetList |> CheckSheetNotExists(sheetName)
  # ##############
  # # item_visit #
  # ##############
  sheetName <- "item_visit"
  #  checkChecklist[[sheetName]] <- sheetList |> CheckItemVisit(sheetName)
  #  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  warning("item_visit sheet check is skipped.")
  # ##############
  # # allocation #
  # ##############
  sheetName <- "allocation"
  checkChecklist[[sheetName]] <- sheetList |> CheckAllocation(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##########
  # # action #
  # ##########
  sheetName <- "action"
  sheetList |> CheckSheetNotExists(sheetName)
  # ###########
  # # display #
  # ###########
  sheetName <- "display"
  sheetList |> CheckSheetNotExists(sheetName)
  # ########
  # # name #
  # ########
  sheetName <- "name"
  checkChecklist[[sheetName]] <- sheetList |> CheckName(target_json, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##########
  # # option #
  # ##########
  sheetName <- "option"
  checkChecklist[[sheetName]] <- sheetList |> CheckOption(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ###########
  # # comment #
  # ###########
  sheetName <- "comment"
  sheetList |> CheckSheetNotExists(sheetName)
  # ###############
  # # explanation #
  # ###############
  sheetName <- "explanation"
  sheetList |> CheckSheetNotExists(sheetName)
  # ############
  # # presence #
  # ############
  sheetName <- "presence"
  sheetList |> CheckSheetNotExists(sheetName)
  # ##########
  # # master #
  # ##########
  sheetName <- "master"
  checkChecklist[[sheetName]] <- sheetList |> CheckMaster(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # #########
  # # visit #
  # #########
  sheetName <- "visit"
  checkChecklist[[sheetName]] <- sheetList |> CheckVisit(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # #########
  # # title #
  # #########
  sheetName <- "title"
  sheetList |> CheckSheetNotExists(sheetName)
  # ############
  # # assigned #
  # ############
  sheetName <- "assigned"
  checkChecklist[[sheetName]] <- sheetList |> CheckAssigned(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##############
  # # limitation #
  # ##############
  sheetName <- "limitation"
  checkChecklist[[sheetName]] <- sheetList |> CheckLimitation(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ########
  # # date #
  # ########
  sheetName <- "date"
  checkChecklist[[sheetName]] <- sheetList |> CheckDate(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  print(str_c("Validation completed for trial: ", trialName))
  return(checkChecklist)
}
# ------ main ------
for (i in 1:length(kTrialNames)) {
  trialName <- kTrialNames[i]
  jsonAndSheet <- GetJsonAndSheet(trialName)
  target_json <- jsonAndSheet[["json"]]
  sheetList <- jsonAndSheet[["sheetList"]]
  checkChecklist <- ExecExcelJsonValidator(trialName)
  if (length(checkChecklist) > 0) {
    print(names(checkChecklist))
    stop(str_c("Validation error:", trialName))
  } else {
    print(str_c("Validation ok:", trialName))
  }
}
