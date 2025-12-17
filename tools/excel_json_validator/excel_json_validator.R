#' test script
#'
#' @file excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2025.12.17
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
kAliasNameJapaneseColumnName <- "シート名英数字別名"
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
keep_objects <- c("keep_objects", "target_json", "sheetList", "trialName", "kTrialNames", "kAliasNameJapaneseColumnName")
kTrialNames <- c("Bev-FOLFOX-SBC", "AML224-FLT3-ITD")
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
    bind_rows()
  if (length(target_json[["visits"]]) > 0) {
    visit <<- target_json[["visits"]] %>%
      map(~ tibble(visit_num = .x[["num"]] %>% as.numeric(), visit_name = .x[["name"]])) %>%
      bind_rows() %>%
      arrange(visit_num)
  } else {
    visit <<- tibble(visit_num = numeric(), visit_name = character())
  }
  visitGroups <<- GetVisitGroupsValidator(target_json, sheetOrders, visit)
  jpNameAndAliasName <<- target_json |> GetNameAndAliasNameByJson()
  jpNameAndGroup <<- GetNameAndGroupByJson()
  fieldItems <<- target_json |> GetFieldItemsByJsonList()
  fieldItems_not_in_visitGroups <<- fieldItems[!names(fieldItems) %in% visitGroups[["alias_name"]]]
  fieldItems_in_visitGroups <<- fieldItems[names(fieldItems) %in% visitGroups[["alias_name"]]]
  options_json <<- target_json[[kOptions]]
  fieldInfoForGetReference <<- GetFieldInfoForGetRef()

  checkChecklist <- list()
  # ########
  # # item #
  # ########
  sheetName <- "item"
  checkChecklist[[sheetName]] <- sheetList |> CheckItemNonVisit(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##################
  # # item_visit_old #
  # ##################
  sheetName <- "item_visit_old"
  checkChecklist[[sheetName]] <- sheetList |> CheckItemVisitOld(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##############
  # # item_visit #
  # ##############
  sheetName <- "item_visit"
  checkChecklist[[sheetName]] <- sheetList |> CheckItemVisit(sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
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
  checkChecklist[[sheetName]] <- sheetList |> CheckAction(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ###########
  # # display #
  # ###########
  sheetName <- "display"
  checkChecklist[[sheetName]] <- sheetList |> CheckDisplay(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
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
  checkChecklist[[sheetName]] <- sheetList |> CheckContent(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ###############
  # # explanation #
  # ###############
  sheetName <- "explanation"
  checkChecklist[[sheetName]] <- sheetList |> CheckExplanation(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ############
  # # presence #
  # ############
  sheetName <- "presence"
  checkChecklist[[sheetName]] <- sheetList |> CheckPresence(target_json, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
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
  checkChecklist[[sheetName]] <- sheetList |> CheckTitle(fieldItems, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
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
