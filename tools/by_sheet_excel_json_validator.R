#' test script
#' 
#' @file by_sheet_excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2024.9.3
# ------ libraries ------
library(here)
source(here("tools", "by_sheet_excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
kOkText <- "test_ok"
# ------ functions ------
ExecCompareBySheet <- function(trialName) {
  assign(trialName, GetInputData(trialName), envir=.GlobalEnv)
  assign(str_c("check_", trialName), CompareJsonAndSheet(get(trialName)$json, get(trialName)$sheet), envir=.GlobalEnv)
}
GetInputData <- function(trialName) {
  jsonList <- here(str_c("input_", trialName)) |> LoadJsonList()
  sheetList <- trialName |> GetTargetFolder() |> GetSheetList()
  res <- list(
    json=jsonList,
    sheet=sheetList
  )
  return(res)
}
CompareJsonAndSheet <- function(jsonList, sheetList) {
  targetCount <- jsonList |> length()
  if (length(sheetList) != targetCount) {
    stop("Error: The lengths of the provided vectors do not match.")
  }
  for (i in 1:length(ignoreCheckFlag)) {
    flagName <- names(ignoreCheckFlag)[[i]]
    checkFlag <- ignoreCheckFlag[[i]]
    assign(str_c("check_", flagName), list())
    if (checkFlag) {
      print(str_c("skip:", flagName))
    }
  }
  for (i in 1:targetCount) {
    json <- jsonList[[i]]
    sheet <- sheetList[[i]]
    targetSheets <- sheet |> names() |> map( ~ {
      if (isTRUE(ignoreCheckFlag[[.]])) {
        return(NULL)
      } else {
        return(.)
      }
    }) |> compact()
    if (length(targetSheets) > 0) {
      for (j in 1:length(targetSheets)) {
        temp <- TestItems(json, sheet, targetSheets[[j]])
        targetListName <- str_c("check_", targetSheets[[j]])
        tempList <- targetListName |> get()
        tempList[[i]] <- temp
        assign(targetListName, tempList)
      }
    }
  }
  res <- list()
  for (i in 1:length(ignoreCheckFlag)) {
    flagName <- names(ignoreCheckFlag)[[i]]
    res[[flagName]] <- get(str_c("check_", flagName))
  }
  return(res)
}

TestItems <- function(json, sheet, target) {
  testJson <- json |> get(str_c("Get", target))()
  testSheet <- sheet[[target]]
  if (!is.null(testJson)) {
    sortJsonColnames <- colnames(testJson) |> sort()
    testJson <- testJson |> select(all_of(sortJsonColnames)) |> ConvertToCharacter()
  } 
  if (!is.null(testSheet)) {
    sortSheetColnames <- colnames(testSheet) |> sort()
    testSheet <- testSheet |> select(all_of(sortSheetColnames)) |> ConvertToCharacter()
  }
  if (!identical(testJson, testSheet)) {
    return(list(json=json, sheet=sheet))
  }
  return(kOkText)
}
# ------ main ------
