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
  openxlsx::read.xlsx(., sheet = "name")
afterchecklist <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\output_20240906133909\\list\\checklist.xlsx" %>% 
  openxlsx::read.xlsx(., sheet = "name")
targetFiles <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2024\\20240906\\BLIN-B-ALL" |> 
  list.files(pattern = "*.json", full.names = T) |> keep( ~ str_detect(., "^.*lab_[0-9]+.json"))
jsonList <- targetFiles |> map( ~ read_json(.))
filenames <- targetFiles |> basename()
names(jsonList) <- filenames
