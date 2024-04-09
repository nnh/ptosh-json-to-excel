#' title
#' description
#' @file issue15.R
#' @author Mariko Ohtsuka
#' @date 2024.4.9
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(daff)
library(openxlsx)
library(testthat)
library(jsonlite)
source(here("prg", "functions", "common_functions.R"), encoding="UTF-8")
# ------ constants ------
kTrialName_bev <- "bev"
kTrialName_allb19 <- "all-b19"
kTrialName_tranilast <- "tranilast"
# ------ functions ------
ExecCompare <- function(trialName, path_list){
  beforeXlsx <- path_list[[trialName]]$before %>% openxlsx::loadWorkbook()
  afterXlsx <- path_list[[trialName]]$after %>% openxlsx::loadWorkbook()
  print("##########")
  print(path_list[[trialName]]$after)
  before_sheetname <- path_list[[trialName]]$before %>% openxlsx::getSheetNames()
  after_sheetname <- path_list[[trialName]]$after %>% openxlsx::getSheetNames()
  check_sheetname <- identical(before_sheetname, after_sheetname)
  if (!check_sheetname) {
    print("!!! sheetname NG !!!")
    return()
  }
  print("sheetname OK.")
  check_flag <- F
  for (i in 1:length(before_sheetname)) {
    before_value <- beforeXlsx %>% read.xlsx(sheet=before_sheetname[i])
    after_value <- afterXlsx %>% read.xlsx(sheet=before_sheetname[i])
    
    if (!identical(before_value, after_value)) {
      daff_result <- diff_data(before_value, after_value)
      render_diff(daff_result, file.path("C:/Users/MarikoOhtsuka/Downloads", paste0(before_sheetname[i], ".html")), view=F)
      print("!!!test NG!!!")
      check_flag <- T
      # tranilastはチェックリストがNGでも処理継続
      if (trialName != kTrialName_tranilast) {
        break
      }
    }
    print(str_c("test ok.row:", nrow(after_value), ",col:", ncol(after_value)))
  }
  return(check_flag)
}
GetFileList <- function(path) {
  return(list.files(path, full.names=T, include.dirs=F, pattern="*.xlsx"))
}
CheckFileIdentify <- function(path_list) {
  test <- path_list %>% map( ~ basename(.))
  return (identical(test[[1]], test[[2]]))
}
ExecCheckIssue15 <- function(trialName){
  path_list_files <- list()
  path_list_files[[trialName]] <- path_list[[trialName]] %>% map( ~ GetFileList(.))
  filesIdentify <- CheckFileIdentify(path_list_files[[trialName]])
  if (!filesIdentify){
    print("!!! filename NG !!!")
    return(F)    
  }
  path_list_checklist <- list()
  path_list_checklist[[trialName]] <- path_list[[trialName]] %>% map( ~ str_c(., "list/checklist.xlsx"))
  print("*** checklist ***")
  check_flag <- ExecCompare(trialName, path_list_checklist)
  if (trialName != kTrialName_tranilast & check_flag) {
    return(F)
  } else {
    print("tranilastはchecklist ngでもチェック続行")
  }
  print("*** files ***")
  for (i in 1:length(path_list_files[[trialName]]$before)) {
    test <- list()
    test[[trialName]]$before <- path_list_files[[trialName]]$before[i]
    test[[trialName]]$after <- path_list_files[[trialName]]$after[i]
    check_flag <- ExecCompare(trialName, test)
    if (check_flag) {
      return(F)
    }
  }
  return(T)
}
# ------ main ------
# Bev, all-b19は修正前後で同じ内容であればOK、tranilastはchecklistのaction以外が同じならOKなので、ダウンロードフォルダにtranilastのaction.htmlだけできてればOK
path_list <- list()
path_list[[kTrialName_tranilast]] <- list(
  before="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240409output_issue15-2_before_Tranilast-DMD-2/",
  after="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240409output_issue15-2_after_Tranilast-DMD-2/"
)
print("*** Tranilast-DMD-2 ***")
checkTranilast <- ExecCheckIssue15(kTrialName_tranilast)

path_list[[kTrialName_bev]] <- list(
  before="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240405output_Bev/",
  after="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240409output_issue15-2_after_bev/"
)
print("*** Bev-FOLFOX-SBC ***")
checkBev <- ExecCheckIssue15(kTrialName_bev)

path_list[[kTrialName_allb19]] <- list(
  before="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240408outputALL-B19/",
  after="C:/Users/MarikoOhtsuka/Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/20240409output_issue15-2_after_allb19/"
)
print("*** ALL-B19 ***")
checkAllb19 <- ExecCheckIssue15(kTrialName_allb19)
