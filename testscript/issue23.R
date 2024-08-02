#' title
#' description
#' @file issue23.R
#' @author Mariko Ohtsuka
#' @date 2024.8.2
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
# ------ constants ------
# ------ functions ------
ReadChecklist <- function(inputFolder) {
  inputPath <- here("output", inputFolder, "list", "checklist.xlsx")
  sheetNames <- inputPath |> getSheetNames()
  sheets <- sheetNames |> map( ~ read.xlsx(inputPath, .))
  names(sheets) <- sheetNames
  return(sheets)
}
ExecCompareIssue23 <- function(bef, aft) {
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
EditForCompareBeforeIssue23 <- function(beforeSheets) {
  renameAliasName <- beforeSheets$name[1, "alias_name", drop=T]
  renameAfter <- renameAliasName %>% str_replace("[0-9]+$", "xxx")
  removeAliasNames <- beforeSheets$name |> slice(2:10) %>% .[ , "alias_name", drop=T]
  res <- beforeSheets |> map( ~ {
    filtered_df <- . |> filter(!alias_name %in% removeAliasNames)
    filtered_df$alias_name <- ifelse(filtered_df$alias_name == renameAliasName, renameAfter, filtered_df$alias_name)
    return(filtered_df)
  })
  item <- res$item
  itemColnames <- colnames(item)
  for (itemColname in itemColnames) {
    item[[itemColname]] <- ifelse(str_detect(item[[itemColname]], renameAliasName), str_replace(item[[itemColname]], renameAliasName, renameAfter), item[[itemColname]])
  }
  res$item <- item
  return(res)  
}
# ------ main ------
beforeSheets <- "output_20240731120753_gpower" |> ReadChecklist()
afterSheets <- "output_20240802143223_gpower" |> ReadChecklist()
editBefSheets <- beforeSheets |> EditForCompareBeforeIssue23()
print("gpower")
ExecCompareIssue23(editBefSheets, afterSheets)

beforeSheets <- "output_20240731120310_allb19" |> ReadChecklist()
afterSheets <- "output_20240802142711_allb19" |> ReadChecklist()
print("allb19")
ExecCompareIssue23(beforeSheets, afterSheets)

beforeSheets <- "output_20240731120646_bev" |> ReadChecklist()
afterSheets <- "output_20240802143101_bev" |> ReadChecklist()
print("bev")
ExecCompareIssue23(beforeSheets, afterSheets)

beforeSheets <- "output_20240731120858_tran" |> ReadChecklist()
afterSheets <- "output_20240802143336_tran" |> ReadChecklist()
print("tran")
ExecCompareIssue23(beforeSheets, afterSheets)
