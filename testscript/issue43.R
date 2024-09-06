#' title
#' description
#' @file issue43.R
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
  openxlsx::read.xlsx(., sheet = "name")
afterchecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240906133909\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "name")
jsonList <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> map( ~ read_json(.))
filenames <- jsonList |> map_vec( ~ .$alias_name)
names(jsonList) <- filenames
visits <- jsonList |> map( ~ {
  if (.$category == "visit") { 
    return(.)
  } else {
    return(NULL)
  }
}) |> discard( ~ is.null(.))
others <- jsonList |> map( ~ {
  if (.$category != "visit") { 
    return(.)
  } else {
    return(NULL)
  }
}) |> discard( ~ is.null(.))

summaryTarget <- visits |> names() |> map_vec( ~ str_replace(., "_[0-9]+$", "_xxx")) |> sort()

for (i in 2:length(summaryTarget)) {
  if (summaryTarget[i - 1] == summaryTarget[i]) {
    summaryTarget[i - 1] <- ""
  }
}
temp <- summaryTarget |> map( ~ ifelse(. == "", return(NULL), return(.))) |> discard( ~ is.null(.)) |> list_c()

aftTargetNames <- c(names(others), temp)

alertbeforechecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240201131005\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")
alertafterchecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240906133909\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "alert")

