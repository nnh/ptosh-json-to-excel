#' title
#' description
#' @file issue34.R
#' @author Mariko Ohtsuka
#' @date 2024.9.3
rm(list=ls())
# ------ libraries ------
source(here("tools", "by_sheet_excel_json_validator.R"), encoding="UTF-8")
# ------ constants ------
ignoreCheckFlag <- list(
  Field_Items=F,
  Option=F,
  Flip_Flops=F,
  Cdisc_Sheet_Configs=T,
  Cdisc_Sheet_Configs_Pivot=F
)
# ------ main ------
"gpower" |> ExecCompareBySheet()