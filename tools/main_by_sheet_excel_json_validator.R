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
  Field_Items=F,
  Option=F,
  Flip_Flops=F,
  Cdisc_Sheet_Configs=T,
  Cdisc_Sheet_Configs_Pivot=F,
  Allocation=F
)
kTargetTrials <- c("tran", "gpower", "allb19", "allr23", "bev")
# ------ main ------
for (i in 1:length(kTargetTrials)) {
  kTargetTrials[[i]] |> ExecCompareBySheet()
}

for (i in 1:length(kTargetTrials)) {
  temp <- get(str_c("check_", kTargetTrials[i])) |> map( ~ {
    temp <- . |> map( ~ {
      if (!is.list(.)) {
        return(NULL)
      }
      return(.)
    }) |> discard( ~ is.null(.))
    if (length(temp) == 0) {
      return(NULL)
    }
    return(temp)
  }) |> discard( ~ is.null(.))
  if (length(temp) == 0) {
    print(str_c("test ok:", kTargetTrials[i]))
  } else {
    stop(str_c("test ng:", kTargetTrials[i]))
  }
}
