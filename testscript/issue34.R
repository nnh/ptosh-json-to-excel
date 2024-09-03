#' title
#' description
#' @file issue34.R
#' @author Mariko Ohtsuka
#' @date 2024.8.29
rm(list=ls())
# ------ libraries ------
source(here("tools", "by_sheet_excel_json_validator.R"), encoding="UTF-8")
# ------ main ------
jsonList <- here("input_gpower") |> LoadJsonList()
sheetList <-"output_20240828161023_gpower" |> GetSheetList()
#jsonItems <- jsonList |> GetJsonItemsForTest()
#sheetItems <- sheetList |> GetSheetListItemsForTest()

aaa <- CompareJsonAndSheet(jsonList, sheetList)


