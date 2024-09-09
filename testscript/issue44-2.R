#' title
#' description
#' @file issue44-2.R
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
beforechecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240201131005\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")
afterchecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240906133909\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")
# lab
targetFiles <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*lab_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
lab_fieldItems <- jsonList |> map( ~ .$field_items)
lab_normalRange <- jsonList |> map( ~ {
  fieldItems <- .$field_items
  res <- fieldItems |> map( ~ {
    if (length(.$normal_range) == 0) {
      return(NULL)
    }
    return(list(sheet_id=.$sheet_id, fieldName=.$name, normal_range=.$normal_range))
  }) |> discard( ~ is.null(.))
  return(res)
})
# lab以外
targetFiles <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*6mp_mtx_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
for (i in 2:length(jsonList)) {
  test1 <- jsonList[[i - 1]] |> Issue44Identical()
  test2 <- jsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    break
  }
}

targetFiles <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*blin_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
for (i in 2:length(jsonList)) {
  test1 <- jsonList[[i - 1]] |> Issue44Identical()
  test2 <- jsonList[[i]] |> Issue44Identical()
  if (!identical(test1, test2)) {
    break
  }
}
