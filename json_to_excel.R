#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(jsonlite)
library(openxlsx)
source(here("common_functions.R"), encoding="UTF-8")
source(here("edit_functions.R"), encoding="UTF-8")
# ------ constants ------
kInputPath <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/Bev-FOLFOX-SBC"
kOutputPath <- "/Users/mariko/Downloads/"
kOutputListIndex = list(raw_json=1, field_items=2, option=3, cdisc_sheet_configs=4, cdisc_sheet_configs_pivot=5,
                        allocation=6, flip_flops=7)
kOutputListNames = c("raw_json", "Field_Items", "Option", "Cdisc_Sheet_Configs", "Cdisc_Sheet_Configs_Pivot",
                     "Allocation", "Flip_Flops")
# ------ functions ------
EditDfJpNameAndAliasName <- function(input_json){
  return(data.frame(jpname=input_json$name, alias_name=input_json$alias_name))
}

WriteExcel <- function(output_list, filename){
  df_jpname_aliasname <- EditDfJpNameAndAliasName(output_list[[kOutputListIndex$raw_json]])
  # If the data does not exist, it is not output to Excel.
  write_target_data <- output_list
  # Input source JSON data need not be output.
  write_target_data[[kOutputListIndex$raw_json]] <- NA
  write_target_index <- which(!is.na(write_target_data))
  # Reorder the columns.
  for (i in 1:length(write_target_data)){
    if (is.data.frame(write_target_data[[i]])){
      write_target_data[[i]] <- write_target_data[[i]] %>% cbind(df_jpname_aliasname, .)
    }
  }
  if (!kOutputListIndex$cdisc_sheet_configs %in% write_target_index){
    write_target_data[[kOutputListIndex$cdisc_sheet_configs]] <- df_jpname_aliasname[-1, ]
    write_target_index <- write_target_index %>% c(kOutputListIndex$cdisc_sheet_configs)
  }
  write.xlsx(x=write_target_data[write_target_index],
             file=str_c(kOutputPath, filename, ".xlsx"),
             overewrite=T,
             sheetName=kOutputListNames[write_target_index])
}
# ------ main ------
# フォルダ内の全ての.jsonファイルを読み込む
json_filenames <- list.files(kInputPath, pattern="*.json", full.names=F) %>% .[81:90]
# JSONファイルの読み込み
json_files <- json_filenames %>% map( ~ {
  rawJson <- read_json(str_c(kInputPath, "/", .))
  flattenJson <- fromJSON(str_c(kInputPath, "/", .), flatten=T)
  return(list(rawJson=rawJson, flattenJson=flattenJson))
})
names(json_files) <- json_filenames %>% str_remove(., ".json")
data_list <- json_files %>% map( ~ {
  trial_data <- .
  raw_json <- trial_data$rawJson
  flatten_json <- trial_data$flattenJson
  output_list <- list()
  output_list[[kOutputListIndex$raw_json]] <- raw_json
  resEditOutputFieldItems <- EditOutputFieldItems(raw_json, flatten_json)
  output_list[[kOutputListIndex$field_items]] <- resEditOutputFieldItems$field_items
  output_list[[kOutputListIndex$option]] <- resEditOutputFieldItems$options
  cdisc_sheet_configs <- EditCdiscSheetConfigs(trial_data)
  output_list[[kOutputListIndex$cdisc_sheet_configs]] <- cdisc_sheet_configs$cdisc_sheet_configs
  output_list[[kOutputListIndex$cdisc_sheet_configs_pivot]] <- cdisc_sheet_configs$cdisc_sheet_configs_pivots
  output_list[[kOutputListIndex$allocation]] <- EditAllocation(flatten_json)
  output_list[[kOutputListIndex$flip_flops]] <- resEditOutputFieldItems$flip_flops
  names(output_list) <- kOutputListNames
  WriteExcel(output_list, raw_json$alias_name)
  return(output_list)
})
names(data_list) <- json_filenames
