#' title
#' description
#' @file issue44-1.R
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
    if (length(target$field_items[[i]]$flip_flops) > 0) {
      for (j in 1:length(target$field_items[[i]]$flip_flops)) {
        target$field_items[[i]]$flip_flops[[j]]$id <- NULL
        target$field_items[[i]]$flip_flops[[j]]$field_item_id <- NULL
      }
    }
  }
  for (i in 1:length(target$cdisc_sheet_configs)) {
    target$cdisc_sheet_configs[[i]]$id <- NULL
    target$cdisc_sheet_configs[[i]]$sheet_id <- NULL
    target$cdisc_sheet_configs[[i]]$created_at <- NULL
    target$cdisc_sheet_configs[[i]]$updated_at <- NULL
    target$cdisc_sheet_configs[[i]]$uuid <- NULL
  }
  target$visit <- NULL
  return(target)
}

# ------ main ------
# gpowerは大丈夫なのか調べる
targetFiles <- here("input_gpower") |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*1st_trt_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
sortedJsonList <-  jsonList[order(names(jsonList))]
for (i in 2:length(sortedJsonList)) {
  test1 <- sortedJsonList[[i - 1]] |> Issue44Identical()
  test2 <- sortedJsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    print("test ng")
    break
  }
}
#firstTrt1000 <- sortedJsonList$`1st_trt_1000.json` |> Issue44Identical()
#firstTrt1100 <- sortedJsonList$`1st_trt_1100.json` |> Issue44Identical()
#test1 <- firstTrt1000
#test1$field_items <- NULL
#test1$cdisc_sheet_configs <- NULL
#test2 <- firstTrt1100
#test2$field_items <- NULL
#test2$cdisc_sheet_configs <- NULL
#identical(test1, test2)
#identical(firstTrt1000$field_items, test2$field_items)
#identical(length(firstTrt1000$field_items), length(firstTrt1100$field_items))
#for (i in 1:length(firstTrt1000$field_items)) {
#  if (!identical(firstTrt1000$field_items[[i]],  firstTrt1100$field_items[[i]])) {
#    break
#  }
#}
#identical(firstTrt1000$field_items[[45]]$option, firstTrt1100$field_items[[45]]$option)
#identical(firstTrt1000$field_items[[45]]$flip_flops, firstTrt1100$field_items[[45]]$flip_flops)
#identical(length(firstTrt1000$field_items[[45]]$flip_flops), length(firstTrt1100$field_items[[45]]$flip_flops))
#for (i in 1:length(firstTrt1000$field_items[[45]]$flip_flops)) {
#  if (!identical(firstTrt1000$field_items[[45]]$flip_flops[[i]], firstTrt1100$field_items[[45]]$flip_flops[[i]])) {
#    break
#  }
#}
#test1 <- firstTrt1000$field_items[[45]]$flip_flops[[i]]
#test2 <- firstTrt1100$field_items[[45]]$flip_flops[[i]]
#identical(firstTrt1000$cdisc_sheet_configs, test2$field_items$cdisc_sheet_configs)
