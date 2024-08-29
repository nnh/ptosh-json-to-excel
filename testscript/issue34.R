#' title
#' description
#' @file issue34.R
#' @author Mariko Ohtsuka
#' @date 2024.8.29
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
GetNormalRange <- function(field_item) {
  normal_range <- field_item$normal_range
  if (length(normal_range) == 0) {
    return(NULL)
  }
  normal_rangeColnames <- names(normal_range) %>% str_c("normal_range.", .)
  res <- normal_range |> map_dfc( ~ .)
  colnames(res) <- normal_rangeColnames
  return(res)
}
GetValidators <- function(field_item) {
  validators <- field_item$validators
  if (is.null(validators)) {
    return(NULL)
  }
  res <- map2_dfc(validators, names(validators), ~ {
    validator <- .x
    validatorName <- .y
    validatorColnames <- names(validator) %>% str_c("validators.", validatorName, ".", .)
    res <- validator |> map_dfc( ~ .)
    if (length(validatorColnames) > 0) {
      colnames(res) <- validatorColnames
    }
    return(res)
  })
  return(res)
}
GetFieldItems <- function(json) {
  field_items <- json$field_items
  if (is.null(field_items)) {
    return(NULL)
  }
  fieldItems <- field_items |> map( ~ {
    temp <- .
    res <- temp |> discard( ~ is.list(.)) |> flatten_df()
    res$jpname <- json$name
    res$alias_name <- json$alias_name
    res$term_code <- ""
    res$auto_calc_field <- ""
    res$content <- ""
    res$argument_type <- ""
    res$reference_type <- ""
    res$reference_field <- ""
    res$formula_field <- ""
    res$flip_flops <- ""
    res$validators.presence <- !is.null(temp$validators$presence)
    validators <- temp |> GetValidators()
    if (!is.null(validators)) {
      res <- res |> bind_cols(validators)
    }
    normal_range <- temp |> GetNormalRange()
    if (!is.null(normal_range)) {
      res <- res |> bind_cols(normal_range)
    }
    return(res)
  }) |> bind_rows()
  return(fieldItems)
  res <- field_items |> map( ~ {
    field_item <- .
    fieldItem <- field_item |> discard( ~ is.list(.))
    fieldList <- field_item |> discard( ~ !is.list(.)) 
    res <- list(fieldItem=fieldItem, fieldList=fieldList)
    return(res)
  })
  return(res)
}
# ------ main ------
jsonList <- here("input_gpower") |> LoadJsonList()
sheetFiles <- here("output", "output_20240828161023_gpower") |> list.files(pattern=".xlsx", full.names=T)
sheetList <- sheetFiles |> 
  map( ~ {
    filepath <- .
    sheetnames <- openxlsx::getSheetNames(.)
    ws <- sheetnames |> map( ~ openxlsx::read.xlsx(filepath, sheet=.))
    names(ws) <- sheetnames
    return(ws)
  })
names(sheetList) <- sheetFiles |> basename() |> str_remove(".xlsx")

test <- jsonList |> map( ~ {
  json <- .
  field_items <- json |> GetFieldItems()
  res <- field_items
  return(res)
  
})
