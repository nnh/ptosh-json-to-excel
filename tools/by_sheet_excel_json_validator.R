#' test script
#' 
#' @file by_sheet_excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2024.9.2
# ------ libraries ------
library(here)
source(here("tools", "by_sheet_excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
ignoreCheckFieldItems <- F
# ------ functions ------
GetJsonItemsForTest <- function(jsonList) {
  jsonItems <- jsonList |> map( ~ {
    json <- .
    field_items <- json |> GetFieldItems()
    options <- json |> GetOptions()
    flipFlops <- json |> GetFlipFlops()
    cdiscSheetConfigs <- json |> GetCdiscSheetConfigs()
    res <- list(
      field_items=field_items,
      options=options,
      flipFlops=flipFlops,
      cdiscSheetConfigs=cdiscSheetConfigs)
    return(res)
  })
  return(jsonItems)
}
GetSheetListItemsForTestFlipFlops <- function(sheet) {
  res <- sheet$Flip_Flops |> filter(!if_all(everything(), ~ . == ""))
  if (nrow(res) == 0) {
    return(NULL)
  }
  return(res)
}
GetSheetListItemsForTest <- function(sheetList) {
  sheetListItems <- sheetList |> map( ~ {
    sheet <- .
    field_items <- sheet$Field_Items
    options <- sheet$Option
    flipFlops <- sheet |> GetSheetListItemsForTestFlipFlops()
    cdiscSheetConfigs <- sheet$Cdisc_Sheet_Configs_Pivot
    res <- list(
      field_items=field_items,
      options=options,
      flipFlops=flipFlops,
      cdiscSheetConfigs=cdiscSheetConfigs
    )
    return(res)
  })
  return(sheetListItems)  
}
CompareJsonAndSheet <- function(jsonList, sheetList) {
  targetCount <- jsonList |> length()
  if (length(sheetList) != targetCount) {
    stop("Error: The lengths of the provided vectors do not match.")
  }
  if (ignoreCheckFieldItems) {
    print("skip:fieldItems")
  } else {
    fieldItems <- list()
  }
  for (i in 1:targetCount) {
    json <- jsonList[[i]]
    sheet <- sheetList[[i]]
    if (!ignoreCheckFieldItems) {
      fieldItems[[i]] <- TestFieldItems(json, sheet)
    }
    
  }
  res <- list(
    fieldItems=fieldItems
  )
  return(res)
}

TestFieldItems <- function(json, sheet) {
  testJson <- json |> GetFieldItems()
  testSheet <- sheet$Field_Items
  if (!is.null(testJson)) {
    sortJsonColnames <- colnames(testJson) |> sort()
    testJson <- testJson |> select(all_of(sortJsonColnames))
  } 
  if (!is.null(testSheet)) {
    sortSheetColnames <- colnames(testSheet) |> sort()
    testSheet <- testSheet |> select(all_of(sortSheetColnames))
  }
  if (!identical(testJson, testSheet)) {
    return(list(json=json, sheet=sheet))
  }
  return(NULL)
} 
testOptions <- map2(jsonOptions, sheetListOption, ~ {
  testJson <- .x
  testSheet <-.y
  sortJsonColnames <- colnames(testJson) |> sort()
  sortSheetColnames <- colnames(testSheet) |> sort()
  if (!is.null(testJson)) {
    testJson <- testJson |> select(all_of(sortJsonColnames))
  } 
  if (!is.null(testSheet)) {
    testSheet <- testSheet |> select(all_of(sortSheetColnames))
  }
  if (!identical(testJson, testSheet)) {
    return(list(json=.x, sheet=.y))
  }
  return(NULL)
}) |> keep( ~ !is.null(.))


# ------ main ------
CompareJsonAndSheet(jsonList, sheetList)
