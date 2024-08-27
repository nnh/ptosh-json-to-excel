#' title
#' description
#' @file issue27_28.R
#' @author Mariko Ohtsuka
#' @date 2024.8.22
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
ExecCompareIssue27 <- function(bef, aft) {
  if (!identical(names(bef), names(aft))) {
    print(names(bef))
    print(names(aft))
    stop("sheetname error.")
  }
  sheetnames <- names(bef)
  for (i in 1:length(sheetnames)) {
    sheetname <- sheetnames[i]
    if (!identical(bef[[sheetname]], aft[[sheetname]])) {
      print(sheetname)
      stop("values error.")
    }
  }
  print("compare ok.")
} 
# ------ main ------
beforeSheets <- "output_20240820095805_gpower" |> ReadChecklist()
afterSheets <- "output_20240822165938_gpower" |> ReadChecklist()
print("gpower")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095158_allb19" |> ReadChecklist()
afterSheets <- "output_20240822165425_allb19" |> ReadChecklist()
print("allb19")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095547_bev" |> ReadChecklist()
afterSheets <- "output_20240822165821_bev" |> ReadChecklist()
print("bev")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095931_tran" |> ReadChecklist()
afterSheets <- "output_20240822170047_tran" |> ReadChecklist()
print("tran")
ExecCompareIssue27(beforeSheets, afterSheets)

sheetList <- "output_20240822170047_tran" |> ReadChecklist()
jsonList <- here("input_tran") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_tran.R"), encoding="UTF-8")



sheetList <- "output_20240822165505_allr23" |> ReadChecklist()
jsonList <- here("input_allr23") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_ALL-R23.R"), encoding="UTF-8")
