#' title
#' description
#' @file issue34.R
#' @author Mariko Ohtsuka
#' @date 2024.8.29
rm(list=ls())
# ------ libraries ------
source(here("tools", "by_sheet_excel_json_validator_common.R"), encoding="UTF-8")
# ------ main ------
jsonList <- here("input_gpower") |> LoadJsonList()
sheetList <-"output_20240828161023_gpower" |> GetSheetList()
jsonItems <- jsonList |> GetJsonItemsForTest()
sheetItems <- sheetList |> GetSheetListItemsForTest()



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




# "Cdisc_Sheet_Configs"は比較の対象外とする

test <- sheetList |> map( ~ names(.))
