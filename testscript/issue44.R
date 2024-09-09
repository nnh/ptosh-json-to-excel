#' title
#' description
#' @file issue44.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(jsonlite)
# ------ constants ------
kTestConstants <- NULL
# ------ functions ------
GetHomeDir <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    home_dir <- Sys.getenv("USERPROFILE")
  } else if (os == "Darwin") {
    home_dir <- Sys.getenv("HOME")
  } else {
    stop("Unsupported OS")
  }
  return (home_dir)
}
# ------ main ------
beforechecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240201131005\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")
afterchecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240906133909\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")
targetFiles <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*lab_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
Issue44Identical <- function(target) {
  target$id <- NULL
  target$alias_name <- NULL
  target$uuid <- NULL
  target$digest <- NULL
  target$seq <- NULL
  target$created_at <- NULL
  target$updated_at <- NULL
  target$lock_version <- NULL
  for (i in 1:length(target$field_items)) {
    target$field_items[[i]]$id <- NULL
    target$field_items[[i]]$sheet_id <- NULL
  }
  for (i in 1:length(target$cdisc_sheet_configs)) {
    target$cdisc_sheet_configs[[i]]$id <- NULL
    target$cdisc_sheet_configs[[i]]$sheet_id <- NULL
    target$cdisc_sheet_configs[[i]]$created_at <- NULL
    target$cdisc_sheet_configs[[i]]$updated_at <- NULL
    target$cdisc_sheet_configs[[i]]$uuid <- NULL
  }
  return(target)
}
sortedJsonList <-  jsonList[order(names(jsonList))]
for (i in 2:length(sortedJsonList)) {
  test1 <- sortedJsonList[[i - 1]] |> Issue44Identical()
  test2 <- sortedJsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    break
  }
}
# [1] "lab_1000.json" "lab_1100.json" "lab_1200.json" "lab_1300.json" "lab_1400.json" "lab_1500.json" "lab_1600.json" "lab_1700.json" "lab_1800.json"までは一致しているので削除
for (i in 9:1) {
  sortedJsonList[[i]] <- NULL
}
print("lab_1900.jsonと不一致のものを探す")
lab_1900 <- sortedJsonList$lab_1900.json
sortedJsonList$lab_1900.json <- NULL
sortedJsonListName <- names(sortedJsonList)
test1 <- lab_1900 |> Issue44Identical()
for (i in 1:length(sortedJsonList)) {
  test2 <- sortedJsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    print(sortedJsonListName[i])
  }
}
print("lab_200.jsonと不一致のものを探す")
lab_200 <- sortedJsonList$lab_200.json
sortedJsonList$lab_200.json <- NULL
sortedJsonListName <- names(sortedJsonList)
test1 <- lab_200 |> Issue44Identical()
for (i in 1:length(sortedJsonList)) {
  test2 <- sortedJsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    print(sortedJsonListName[i])
  }
}
print("lab_2000.jsonと不一致のものを探す")
lab_2000 <- sortedJsonList$lab_2000.json
sortedJsonList$lab_2000.json <- NULL
sortedJsonListName <- names(sortedJsonList)
test1 <- lab_2000 |> Issue44Identical()
for (i in 1:length(sortedJsonList)) {
  test2 <- sortedJsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    print(sortedJsonListName[i])
  }
}
