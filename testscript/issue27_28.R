#' title
#' description
#' @file issue27_28.R
#' @author Mariko Ohtsuka
#' @date 2024.8.28
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
sheetList <- "output_20240822165938_gpower" |> ReadChecklist()
tempJsonList <- here("input_gpower") |> LoadJsonList()
removeJsonList <- names(tempJsonList) %>% .[2:10]
jsonList <- tempJsonList
for (i in 1:length(removeJsonList)) {
  jsonList[[removeJsonList[i]]] <- NULL
}
temp <- names(jsonList)
temp[1] <- temp[1] |> str_replace("[0-9]+$", "xxx")
names(jsonList) <- temp
jsonList[[1]]$alias_name <- temp[1] 
source(here("tools", "trials", "excel_json_validator_gpower.R"), encoding="UTF-8")

sheetList <- "output_20240822165425_allb19" |> ReadChecklist()
jsonList <- here("input_allb19") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_allb19.R"), encoding="UTF-8")

sheetList <- "output_20240822165821_bev" |> ReadChecklist()
jsonList <- here("input_bev") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_bev.R"), encoding="UTF-8")

sheetList <- "output_20240828122541" |> ReadChecklist()
jsonList <- here("input_tran") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_tran.R"), encoding="UTF-8")

sheetList <- "output_20240822165505_allr23" |> ReadChecklist()
jsonList <- here("input_allr23") |> LoadJsonList()
source(here("tools", "trials", "excel_json_validator_ALL-R23.R"), encoding="UTF-8")
