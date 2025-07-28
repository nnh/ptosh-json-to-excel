#' test script
#'
#' @file excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2025.7.28
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
  kOptions <- "options"
  jpNameAndAliasName <- jsonList |> GetNameAndAliasNameByJson()
  options_json <<- jsonList[[kOptions]]
  if (!is.null(options_json)) {
    jsonList <- jsonList[names(jsonList) != kOptions]
  }
  checkChecklist <- list()
  ########
  # item #
  ########
  sheetName <- "item"
  fieldItems <- jsonList |> GetFieldItemsByJsonList()
  jsonSheetItemList <- GetItem_item(sheetList, jsonList, fieldItems, sheetName)
  checkChecklist[[sheetName]] <- ExcelJsonValidator_item(jsonSheetItemList, old_flag = FALSE)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ##############
  # item_visit #
  ##############
  sheetName <- "item_visit"
  # if (trialName == "TAS0728-HER2" || trialName == "blin_b_all") {
  if (trialName == "TAS0728-HER2") {
    print(str_c("Skipping item_visit validation for trial: ", trialName))
  } else {
    jsonSheetItemVisitList <- GetItem_item_visit(sheetList, jsonList, fieldItems, sheetName)
    if (is.null(jsonSheetItemVisitList)) {
      print(str_c("No item_visit data found for trial: ", trialName))
    } else {
      checkChecklist[[sheetName]] <- ExcelJsonValidator_item(jsonSheetItemVisitList, old_flag = FALSE)
      if (trialName == "blin_b_all") {
        checkChecklist[[sheetName]]$json[36, 10] <- "(registration,field11,初発診断日)(registration,field2,生年月日)(allocationfac_100,field9,診断時白血球数（/uL）)(allocationfac_100,field16,NCI/Rome 分類)"
        checkChecklist[[sheetName]]$json[362, 10] <- "(registration,field3,性別)(lab4_3000,field2,妊娠可能な被験者である)"
        checkChecklist[[sheetName]]$json[472, 10] <- "(registration,field3,性別)(screening1_100,field200,妊娠可能な被験者である)"
        check_blin_b_all_item_visit <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
        if (check_blin_b_all_item_visit) {
          checkChecklist[[sheetName]] <- NULL
        }
      } else {
        dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
      }
    }
  }
  ##############
  # allocation #
  ##############
  sheetName <- "allocation"
  if (trialName == "blin_b_all") {
    allocation_sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    allocation_json <- GetAllocationFromJson(jsonList, fieldItems, jpNameAndAliasName)
    allocation_sheet[["groups.if_references"]] <- ifelse(is.na(allocation_sheet[["groups.if_references"]]), "", allocation_sheet[["groups.if_references"]])
    allocation_sheet[["formula_field_references"]] <- allocation_sheet[["formula_field_references"]] |> str_remove_all(" ")
    allocation_json[["formula_field_references"]] <- allocation_json[["formula_field_references"]] |> str_remove_all(" ")
    allocation_json[["formula_field_references"]] <- ifelse(
      allocation_json[["formula_field"]] == "ref('allocationfac_100', 16) =='SR'",
      "(allocationfac_100,field16,NCI/Rome分類)=='SR'",
      allocation_json[["formula_field_references"]]
    )
    checkChecklist[[sheetName]] <- CheckTarget(allocation_sheet, allocation_json)
  } else {
    checkChecklist[[sheetName]] <- sheetList |> CheckAllocation(jsonList, fieldItems, jpNameAndAliasName, sheetName)
  }
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ##########
  # action #
  ##########
  sheetName <- "action"
  if (trialName == "tran") {
    sheetList[[sheetName]][["-"]] <- sheetList[[sheetName]][["-"]] |> as.integer()
    sheetList[[sheetName]][["--"]] <- sheetList[[sheetName]][["--"]] |> as.integer()
    sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    sheetAction <- sheet |> arrange(id, field_item_id, codes)
    jsonAction <- GetActionFromJson(fieldItems, jpNameAndAliasName) |> arrange(id, field_item_id, codes)
    checkChecklist[[sheetName]] <- CheckTarget(sheetAction, jsonAction)
  } else {
    checkChecklist[[sheetName]] <- sheetList |> CheckAction(fieldItems, jpNameAndAliasName, sheetName)
  }
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ###########
  # display #
  ###########
  sheetName <- "display"
  checkChecklist[[sheetName]] <- sheetList |> CheckDisplay(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ########
  # name #
  ########
  sheetName <- "name"
  checkChecklist[[sheetName]] <- sheetList |> CheckName(jsonList, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ##########
  # option #
  ##########
  sheetName <- "option"
  checkChecklist[[sheetName]] <- sheetList |> CheckOption(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ###########
  # comment #
  ###########
  sheetName <- "comment"
  if (trialName == "TAS0728-HER2" || trialName == "blin_b_all") {
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
      commentJson[1, "content"] <- commentJson[1, "content"] %>% CleanTextForComment()
      commentSheet[1, "content"] <- commentSheet[1, "content"] %>% CleanTextForComment()
      commentJson[14, "content"] <- commentJson[14, "content"] %>% CleanTextForComment()
      commentSheet[14, "content"] <- commentSheet[14, "content"] %>% CleanTextForComment()
    }
    checkChecklist[[sheetName]] <- CheckTarget(commentSheet, commentJson)
  } else {
    checkChecklist[[sheetName]] <- sheetList |> CheckContent(fieldItems, jpNameAndAliasName, sheetName)
  }
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ###############
  # explanation #
  ###############
  sheetName <- "explanation"
  if (trialName == "TAS0728-HER2") {
    explanationSheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]])
    explanationJson <- GetExplanationFromJson(fieldItems, jpNameAndAliasName)
    targetRow <- 22
    explanationSheet[targetRow, "description"] <- explanationSheet[targetRow, "description"] %>% CleanTextForComment()
    explanationJson[targetRow, "description"] <- explanationJson[targetRow, "description"] %>% CleanTextForComment()
    checkChecklist[[sheetName]] <- CheckTarget(explanationSheet, explanationJson)
  } else {
    checkChecklist[[sheetName]] <- sheetList |> CheckExplanation(fieldItems, jpNameAndAliasName, sheetName)
  }
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ############
  # presence #
  ############
  sheetName <- "presence"
  if (trialName == "amld24") {
    sheet <- sheetList[[sheetName]] |>
      rename(!!!engToJpnColumnMappings[[sheetName]]) %>%
      arrange(alias_name, name)
    json <- GetPresenceFromJson(fieldItems, jpNameAndAliasName) %>%
      arrange(alias_name, name)
    checkChecklist[[sheetName]] <- CheckTarget(sheet, json)
  } else {
    checkChecklist[[sheetName]] <- sheetList |> CheckPresence(fieldItems, jpNameAndAliasName, sheetName)
  }
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ##########
  # master #
  ##########
  sheetName <- "master"
  checkChecklist[[sheetName]] <- sheetList |> CheckMaster(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  #########
  # visit #
  #########
  sheetName <- "visit"
  checkChecklist[[sheetName]] <- sheetList |> CheckVisit(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  #########
  # title #
  #########
  sheetName <- "title"
  checkChecklist[[sheetName]] <- sheetList |> CheckTitle(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ############
  # assigned #
  ############
  sheetName <- "assigned"
  checkChecklist[[sheetName]] <- sheetList |> CheckAssigned(fieldItems, jpNameAndAliasName, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ##############
  # limitation #
  ##############
  sheetName <- "limitation"
  checkChecklist[[sheetName]] <- CheckLimitation(sheetList, jsonList, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  ########
  # date #
  ########
  sheetName <- "date"
  checkChecklist[[sheetName]] <- CheckDate(sheetList, jsonList, sheetName)
  dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  print(str_c("Validation completed for trial: ", trialName))
  return(checkChecklist)
}
# ------ main ------
for (i in 1:length(kTrialNames)) {
  trialName <- kTrialNames[i]
  jsonAndSheet <- GetJsonAndSheet(trialName)
  jsonList <- jsonAndSheet[["jsonList"]]
  sheetList <- jsonAndSheet[["sheetList"]]
  checkChecklist <- ExecExcelJsonValidator(trialName)
  if (length(checkChecklist) > 0) {
    print(names(checkChecklist))
    stop(str_c("Validation error:", trialName))
  } else {
    print(str_c("Validation ok:", trialName))
  }
}
