#' test script
#'
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2025.7.3
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
keep_objects <- c("keep_objects", "jsonList", "sheetList", "trialName", "kTrialNames")
kTrialNames <- c("Bev-FOLFOX-SBC", "TAS0728-HER2", "gpower", "bev", "allb19", "tran", "allr23", "blin_b_all", "amld24", "JCCG-LFS25")
# ------ functions ------
# ------ main ------
for (i in 1:length(kTrialNames)) {
  trialName <- kTrialNames[i]
  jsonAndSheet <- GetJsonAndSheet(trialName)
  jsonList <- jsonAndSheet$jsonList
  sheetList <- jsonAndSheet$sheetList
  source(here("tools", "excel_json_validator", "trials", str_c("excel_json_validator_", trialName, ".R")), encoding = "UTF-8")
  if (length(checkChecklist) > 0) {
    names(checkChecklist)
    stop(str_c("Validation error:", trialName))
  } else {
    print(str_c("Validation ok:", trialName))
  }
}
