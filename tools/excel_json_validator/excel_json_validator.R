#' test script
#'
#' @file excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2025.12.12
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
kAliasNameJapaneseColumnName <- "シート名英数字別名"
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
keep_objects <- c("keep_objects", "target_json", "sheetList", "trialName", "kTrialNames", "kAliasNameJapaneseColumnName")
kTrialNames <- c("AML224-FLT3-ITD")
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
  visit <<- target_json[["visits"]] %>%
    map(~ tibble(visit_num = .x[["num"]] %>% as.numeric(), visit_name = .x[["name"]])) %>%
    bind_rows() %>%
    arrange(visit_num)
  visitGroups <<- GetVisitGroupsValidator(target_json, sheetOrders, visit)
  jpNameAndAliasName <<- target_json |> GetNameAndAliasNameByJson()
  jpNameAndGroup <<- GetNameAndGroupByJson()
  fieldItems <<- target_json |> GetFieldItemsByJsonList()
  fieldItems_not_in_visitGroups <<- fieldItems[!names(fieldItems) %in% visitGroups[["alias_name"]]]
  fieldItems_in_visitGroups <<- fieldItems[names(fieldItems) %in% visitGroups[["alias_name"]]]
  options_json <<- target_json[[kOptions]]
  # temp <- GetIsVisit(target_json)
  # visit_not_visit_json <- temp$visit_not_visit_json %>% setNames(names(temp$visit_not_visit_json))
  # if (is.null(temp$visit_not_visit_fieldItems)) {
  #  visit_not_visit_fieldItems <- fieldItems
  # } else {
  #  visit_not_visit_fieldItems <- temp$visit_not_visit_fieldItems %>% setNames(names(temp$visit_not_visit_fieldItems))
  # }
  # rm(temp)
  checkChecklist <- list()
  # ########
  # # item #
  # ########
  # sheetName <- "item"
  # jsonSheetItemList <- GetItem_item(sheetList, jsonList, fieldItems, sheetName)
  # checkChecklist[[sheetName]] <- ExcelJsonValidator_item(jsonSheetItemList, old_flag = FALSE)
  # dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ##################
  # # item_visit_old #
  # ##################
  # sheetName <- "item_visit_old"
  # if (trialName == "TAS0728-HER2") {
  #   print(str_c("Skipping item_visit_old validation for trial: ", trialName))
  # } else {
  #   jsonSheetItemVisitList <- GetItem_item_visit_old(sheetList, visit_not_visit_fieldItems, visit_not_visit_jsonList, sheetName)
  #   if (is.null(jsonSheetItemVisitList)) {
  #     print(str_c("No item_visit_old data found for trial: ", trialName))
  #   } else {
  #     checkChecklist[[sheetName]] <- ExcelJsonValidator_item(jsonSheetItemVisitList, old_flag = FALSE)
  #     if (trialName == "blin_b_all") {
  #       checkChecklist[[sheetName]]$json[36, 10] <- "(registration,field11,初発診断日)(registration,field2,生年月日)(allocationfac_100,field9,診断時白血球数（/uL）)(allocationfac_100,field16,NCI/Rome 分類)"
  #       checkChecklist[[sheetName]]$json[362, 10] <- "(registration,field3,性別)(lab4_3000,field2,妊娠可能な被験者である)"
  #       checkChecklist[[sheetName]]$json[472, 10] <- "(registration,field3,性別)(screening1_100,field200,妊娠可能な被験者である)"
  #       check_blin_b_all_item_visit <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  #       if (check_blin_b_all_item_visit) {
  #         checkChecklist[[sheetName]] <- NULL
  #       }
  #     } else if (trialName == "AML224-PIF") {
  #       checkChecklist[[sheetName]]$json[48, 10] <- "(allocationfac1_110,field11,AML診断時(初回寛解導入療法前)骨髄中芽球（%）)(allocationfac2_150,field7,初回治療が無効と判断した際の骨髄検査時の骨髄中芽球（%）)(allocationfac2_150,field2,骨髄芽球減少率)"
  #       check_aml224_pif_item_visit <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  #       if (check_aml224_pif_item_visit) {
  #         checkChecklist[[sheetName]] <- NULL
  #       }
  #     } else {
  #       dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  #     }
  #   }
  # }
  # ##############
  # # item_visit #
  # ##############
  # sheetName <- "item_visit"
  # if (trialName == "TAS0728-HER2") {
  #   print(str_c("Skipping item_visit_old validation for trial: ", trialName))
  #   jsonSheetItemVisitList <- NULL
  # }
  # if (is.null(jsonSheetItemVisitList)) {
  #   checkChecklist[[sheetName]] <- NULL
  # } else {
  #   checkChecklist[[sheetName]] <- CheckItemVisit(jsonSheetItemVisitList[["json"]], sheetName, sheetList)
  # }
  # ##############
  # # allocation #
  # ##############
  # sheetName <- "allocation"
  # if (trialName == "blin_b_all") {
  #   allocation_sheet <- sheetList[[sheetName]] |>
  #     rename(!!!engToJpnColumnMappings[[sheetName]])
  #   allocation_json <- GetAllocationFromJson(jsonList, fieldItems, jpNameAndAliasName)
  #   allocation_sheet[["groups.if_references"]] <- ifelse(is.na(allocation_sheet[["groups.if_references"]]), "", allocation_sheet[["groups.if_references"]])
  #   allocation_sheet[["formula_field_references"]] <- allocation_sheet[["formula_field_references"]] |> str_remove_all(" ")
  #   allocation_json[["formula_field_references"]] <- allocation_json[["formula_field_references"]] |> str_remove_all(" ")
  #   allocation_json[["formula_field_references"]] <- ifelse(
  #     allocation_json[["formula_field"]] == "ref('allocationfac_100', 16) =='SR'",
  #     "(allocationfac_100,field16,NCI/Rome分類)=='SR'",
  #     allocation_json[["formula_field_references"]]
  #   )
  #   checkChecklist[[sheetName]] <- CheckTarget(allocation_sheet, allocation_json)
  # } else {
  #   checkChecklist[[sheetName]] <- sheetList |> CheckAllocation(jsonList, fieldItems, jpNameAndAliasName, sheetName)
  # }
  # dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
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
  checkChecklist[[sheetName]] <- sheetList |> CheckVisit(sheetName, target_json)
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
  # sheetName <- "limitation"
  # checkChecklist[[sheetName]] <- CheckLimitation(sheetList, visit_not_visit_jsonList, sheetName)
  # dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # ########
  # # date #
  # ########
  # sheetName <- "date"
  # checkChecklist[[sheetName]] <- CheckDate(sheetList, visit_not_visit_jsonList, sheetName)
  # dummy <- ExecValidateSheetAndJsonEquality(checkChecklist, sheetName)
  # print(str_c("Validation completed for trial: ", trialName))
  # return(checkChecklist)
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
