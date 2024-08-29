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
kFieldItemColnames <- c("jpname", "alias_name", "id", "sheet_id", "name", "label", "description", "seq", "is_invisible",
      "default_value", "field_type", "link_type", "deviation", "term_code", "auto_calc_field", "level",
      "content", "argument_type", "reference_type", "reference_field", "formula_field", "type", "flip_flops",
      "validators.presence.validate_presence_if", "validators.formula.validate_formula_if",
      "validators.formula.validate_formula_message", 
      "validators.numericality.validate_numericality_less_than_or_equal_to", 
      "validators.numericality.validate_numericality_greater_than_or_equal_to", 
      "validators.date.validate_date_before_or_equal_to", 
      "normal_range.less_than_or_equal_to", 
      "option.name", 
      "normal_range.greater_than_or_equal_to",
      "validators.presence")

# ------ functions ------
ConvertToCharacter <- function(df) {
  df_char <- df %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    mutate(across(everything(), ~ replace_na(., "")))
  return(df_char)
}

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
  validators <- validators |> map_if( ~ length(.) == 0, ~ NULL) |> discard( ~ is.null(.))
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
      if (nrow(validators) > 0) {
        res <- res |> bind_cols(validators)
      }
    }
    normal_range <- temp |> GetNormalRange()
    if (!is.null(normal_range)) {
      if (nrow(normal_range) > 0) {
        res <- res |> bind_cols(normal_range)
      }
    }
    res$option.name <- temp$option$name
    return(res)
  }) |> bind_rows()
  targetColnames <- intersect(kFieldItemColnames, colnames(fieldItems))
  res <- fieldItems |> select(all_of(targetColnames)) |> ConvertToCharacter() |> as.data.frame()
  return(res)
}
# ------ main ------
jsonList <- here("input_gpower") |> LoadJsonList()
sheetFiles <- here("output", "output_20240828161023_gpower") |> list.files(pattern=".xlsx", full.names=T)
sheetList <- sheetFiles |> 
  map( ~ {
    filepath <- .
    sheetnames <- openxlsx::getSheetNames(.)
    ws <- sheetnames |> map( ~ openxlsx::read.xlsx(filepath, sheet=., na.strings=NULL) |> ConvertToCharacter())
    names(ws) <- sheetnames
    return(ws)
  })
names(sheetList) <- sheetFiles |> basename() |> str_remove(".xlsx")

jsonFieldItems <- jsonList |> map( ~ {
  json <- .
  field_items <- json |> GetFieldItems()
  res <- field_items
  return(res)
})
testFieldItems <- map2(jsonFieldItems, sheetList, ~ {
  if (!identical(.x, .y$Field_Items)) {
    return(list(json=.x, sheet=.y$Field_Items))
  }
  return(NULL)
}) |> keep( ~ !is.null(.))


for (i in 1:218) {
  if (!identical(testFieldItems[[1]]$json[i, ], testFieldItems[[1]]$sheet[i, ])) {
    break
  }
}
test3 <- testFieldItems[[1]]$json[i, ]
test4 <- testFieldItems[[1]]$sheet[i, ]
str(test3)
str(test4)
for (j in 1:32) {
  if (!identical(testFieldItems[[1]]$json[i, j], testFieldItems[[1]]$sheet[i, j])) {
    break
  }
  
}
