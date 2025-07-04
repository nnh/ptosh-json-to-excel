#' test script
#'
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2025.7.4
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
kAliasNameJapaneseColumnName <- "シート名英数字別名"
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
keep_objects <- c("keep_objects", "jsonList", "sheetList", "trialName", "kTrialNames", "kAliasNameJapaneseColumnName")
kTrialNames <- c("JCCG-LFS25", "amld24", "Bev-FOLFOX-SBC", "TAS0728-HER2", "gpower", "bev", "allb19", "tran", "allr23", "blin_b_all")
# ------ functions ------
ExecExcelJsonValidator <- function(trialName) {
  if (exists("keep_objects")) {
    rm(list = setdiff(ls(), keep_objects))
  }
  library(tidyverse, warn.conflicts = F)
  library(here, warn.conflicts = F)
  source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
  jpNameAndAliasName <- jsonList |> GetNameAndAliasNameByJson()
  checkChecklist <- list()
  # item
  item_jsonList <- jsonList %>% keep(~ .x$category != "visit")
  item_fieldItems <- item_jsonList |> GetFieldItemsByJsonList()
  jsonSheetItemList <- GetItem_item(sheetList, item_jsonList, item_fieldItems)
  checkChecklist$item <- ExcelJsonValidator_item(jsonSheetItemList, old_flag = FALSE)
  if (!is.null(checkChecklist$item)) {
    for (col in 1:ncol(jsonSheetItemList$sheet)) {
      if (identical(jsonSheetItemList$sheet[, col], jsonSheetItemList$json[, col])) {
        next
      } else {
        print(str_c("Validation error in column: ", col, " of item sheet"))
      }
      for (row in 1:nrow(jsonSheetItemList$sheet)) {
        if (is.na(jsonSheetItemList$sheet[row, col]) && is.na(jsonSheetItemList$json[row, col])) {
          next
        }
        if (jsonSheetItemList$sheet[row, col] != jsonSheetItemList$json[row, col]) {
          stop(str_c(
            "Validation error in item at row ", row, " and column ", col, ": ",
            jsonSheetItemList$sheet[row, col], " != ", jsonSheetItemList$json[row, col]
          ))
        }
      }
    }
  }
  ##################
  # item old sheet #
  ##################
  fieldItems <- jsonList |> GetFieldItemsByJsonList()
  jsonSheetItemList <- GetItemOldFromJson(sheetList, jsonList, fieldItems, jpNameAndAliasName)
  checkChecklist$item_old <- ExcelJsonValidator_item(jsonSheetItemList, old_flag = TRUE)
  ##############
  # allocation #
  ##############
  if (trialName == "blin_b_all") {
    sheetName <- "allocation"
    allocation_sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    allocation_json <- GetAllocationFromJson(jsonList, fieldItems, jpNameAndAliasName)
    allocation_sheet$groups.if_references <- ifelse(is.na(allocation_sheet$groups.if_references), "", allocation_sheet$groups.if_references)
    allocation_sheet$formula_field_references <- allocation_sheet$formula_field_references |> str_remove_all(" ")
    allocation_json$formula_field_references <- allocation_json$formula_field_references |> str_remove_all(" ")
    allocation_json$formula_field_references <- ifelse(allocation_json$formula_field == "ref('allocationfac_100', 16) =='SR'", "(allocationfac_100,field16,NCI/Rome分類)=='SR'", allocation_json$formula_field_references)
    checkChecklist$allocation <- CheckTarget(allocation_sheet, allocation_json)
  } else {
    checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList, fieldItems, jpNameAndAliasName)
  }
  ##########
  # action #
  ##########
  if (trialName == "tran") {
    sheetList$action$`-` <- sheetList$action$`-` |> as.integer()
    sheetList$action$`--` <- sheetList$action$`--` |> as.integer()
    sheetName <- "action"
    sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    sheetAction <- sheet |> arrange(id, field_item_id, codes)
    jsonAction <- GetActionFromJson(fieldItems, jpNameAndAliasName) |> arrange(id, field_item_id, codes)
    checkChecklist$action <- CheckTarget(sheetAction, jsonAction)
  } else {
    checkChecklist$action <- sheetList |> CheckAction(fieldItems, jpNameAndAliasName)
  }
  ###########
  # display #
  ###########
  checkChecklist$display <- sheetList |> CheckDisplay(fieldItems, jpNameAndAliasName)
  ########
  # name #
  ########
  checkChecklist$name <- sheetList |> CheckName(jsonList)
  ##########
  # option #
  ##########
  checkChecklist$option <- sheetList |> CheckOption(fieldItems, jpNameAndAliasName)
  ###########
  # comment #
  ###########
  if (trialName == "TAS0728-HER2" || trialName == "blin_b_all") {
    sheetName <- "comment"
    commentSheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    commentJson <- GetContentFromJson(fieldItems, jpNameAndAliasName)
    if (trialName == "TAS0728-HER2") {
      commentJson[2, "content"] <- commentJson[2, "content"] %>% CleanTextForComment()
      commentSheet[2, "content"] <- commentSheet[2, "content"] %>% CleanTextForComment()
      commentJson[3, "content"] <- commentJson[3, "content"] %>% CleanTextForComment()
      commentSheet[3, "content"] <- commentSheet[3, "content"] %>% CleanTextForComment()
    }
    if (trialName == "blin_b_all") {
      commentJson[12, "content"] <- commentJson[12, "content"] %>% CleanTextForComment()
      commentSheet[12, "content"] <- commentSheet[12, "content"] %>% CleanTextForComment()
    }
    checkChecklist$content <- CheckTarget(commentSheet, commentJson)
  } else {
    checkChecklist$content <- sheetList |> CheckContent(fieldItems, jpNameAndAliasName)
  }
  ###############
  # explanation #
  ###############
  if (trialName == "TAS0728-HER2") {
    sheetName <- "explanation"
    explanationSheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    explanationJson <- GetExplanationFromJson(fieldItems, jpNameAndAliasName)
    targetRow <- 22
    explanationSheet[targetRow, "description"] <- explanationSheet[targetRow, "description"] %>% CleanTextForComment()
    explanationJson[targetRow, "description"] <- explanationJson[targetRow, "description"] %>% CleanTextForComment()
    checkChecklist$explanation <- CheckTarget(explanationSheet, explanationJson)
  } else {
    checkChecklist$explanation <- sheetList |> CheckExplanation(fieldItems, jpNameAndAliasName)
  }
  ############
  # presence #
  ############
  if (trialName == "amld24") {
    sheetName <- "presence"
    sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]]) %>%
      arrange(alias_name, name)
    json <- GetPresenceFromJson(fieldItems, jpNameAndAliasName) %>%
      arrange(alias_name, name)
    checkChecklist$presence <- CheckTarget(sheet, json)
  } else {
    checkChecklist$presence <- sheetList |> CheckPresence(fieldItems, jpNameAndAliasName)
  }
  ##########
  # master #
  ##########
  checkChecklist$master <- sheetList |> CheckMaster(fieldItems, jpNameAndAliasName)
  #########
  # visit #
  #########
  checkChecklist$visit <- sheetList |> CheckVisit(fieldItems, jpNameAndAliasName)
  #########
  # title #
  #########
  checkChecklist$title <- sheetList |> CheckTitle(fieldItems, jpNameAndAliasName)
  ############
  # assigned #
  ############
  checkChecklist$assigned <- sheetList |> CheckAssigned(fieldItems, jpNameAndAliasName)
  ##############
  # limitation #
  ##############
  checkChecklist$limitation <- CheckLimitation(sheetList, jsonList)
  ########
  # date #
  ########
  checkChecklist$date <- CheckDate(sheetList, jsonList)
  print(str_c("Validation completed for trial: ", trialName))
  return(checkChecklist)
}
# ------ main ------
for (i in 1:length(kTrialNames)) {
  trialName <- kTrialNames[i]
  jsonAndSheet <- GetJsonAndSheet(trialName)
  jsonList <- jsonAndSheet$jsonList
  sheetList <- jsonAndSheet$sheetList
  checkChecklist <- ExecExcelJsonValidator(trialName)
  if (length(checkChecklist) > 0) {
    print(names(checkChecklist))
    stop(str_c("Validation error:", trialName))
  } else {
    print(str_c("Validation ok:", trialName))
  }
}
