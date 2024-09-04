#' title
#' description
#' @file main_by_sheet_excel_json_validator.R
#' @author Mariko Ohtsuka
#' @date 2024.9.3
rm(list=ls())
# ------ libraries ------
library(here)
source(here("tools", "by_sheet_excel_json_validator.R"), encoding="UTF-8")
# ------ constants ------
ignoreCheckFlag <- list(
  Field_Items=T,
  Option=T,
  Flip_Flops=T,
  Cdisc_Sheet_Configs=T,
  Cdisc_Sheet_Configs_Pivot=F,
  Allocation=T
)
kTargetTrials <- c("tran", "gpower", "allb19", "allr23", "bev")
# ------ main ------
for (i in 1:length(kTargetTrials)) {
  kTargetTrials[[i]] |> ExecCompareBySheet()
}
