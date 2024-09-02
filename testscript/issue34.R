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
      "validators.presence.validate_presence_if", 
      "validators.formula.validate_formula_if",
      "validators.formula.validate_formula_message", 
      "validators.numericality.validate_numericality_less_than_or_equal_to", 
      "validators.numericality.validate_numericality_greater_than_or_equal_to", 
      "validators.date.validate_date_before_or_equal_to", 
      "normal_range.less_than_or_equal_to", 
      "option.name", "option_id", 
      "normal_range.greater_than_or_equal_to",
      "validators.date.validate_date_after_or_equal_to",
      "validators.presence.validate_presence_id",
      "validators.presence")
kOptionColnames <- c("jpname", "alias_name", 
                     "option_id", "option.id", "option.trial_id", "option.name", "option.source_id", 
                     "option.is_extensible", "option.odm_id", "option.uuid", "option.created_at", 
                     "option.updated_at", "option.controlled_terminology_id", "option.parity", 
                     "option.digest", "option.values_id", "option.values_name", "option.values_seq", 
                     "option.values_code", "option.values_is_usable", "option.values_option_id",
                     "option.controlled_terminology_data.uuid",
                     "option.controlled_terminology_data.cdisc_code", 
                     "option.controlled_terminology_data.cdisc_name")
kFlipFlapColnames <- c("jpname", "alias_name", "id", "field_item_id", "codes", "fields", "created_at", "updated_at")
kCdiscSheetConfigs <- c("jpname", "alias_name", "id", "sheet_id", "prefix", "label", "table.field", "table.field.value")
# ------ functions ------
ConvertToCharacter <- function(df) {
  df_char <- df %>%
    mutate(across(everything(), ~ format(., scientific=F))) %>%
    mutate(across(everything(), ~ trimws(.))) %>%
    mutate(across(everything(), ~ str_replace(., "^NA$", "")))
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
GetFieldItemsList <- function(json) {
  field_items <- json$field_items
  if (is.null(field_items)) {
    return(NULL)
  }
  if (length(field_items) == 0) {
    return(NULL)
  }
  return(field_items)
}
GetFieldItems <- function(json) {
  field_items <- GetFieldItemsList(json)
  if (is.null(field_items)) {
    return(NULL)
  }
  fieldItems <- field_items |> map( ~ {
    temp <- .
    res <- temp |> discard( ~ is.list(.)) |> flatten_df()
    res$jpname <- json$name
    res$alias_name <- json$alias_name
    res$default_value <- ifelse(is.null(temp$default_value), "", as.character(temp$default_value))
    res$field_type <- ifelse(is.null(temp$field_type), "", as.character(temp$field_type))
    res$link_type <- ifelse(is.null(temp$link_type), "", as.character(temp$link_type))
    res$deviation <- ifelse(is.null(temp$deviation), "", as.character(temp$deviation))
    res$term_code <- ifelse(is.null(temp$term_code), "", as.character(temp$term_code))
    res$auto_calc_field <- ifelse(is.null(temp$auto_calc_field), "", as.character(temp$auto_calc_field))
    res$level <- ifelse(is.null(temp$level), "", as.character(temp$level))
    res$content <- ifelse(is.null(temp$content), "", as.character(temp$content))
    res$argument_type <- ifelse(is.null(temp$argument_type), "", as.character(temp$argument_type))
    res$reference_type <- ifelse(is.null(temp$reference_type), "", as.character(temp$reference_type))
    res$reference_field <- ifelse(is.null(temp$reference_field), "", as.character(temp$reference_field))
    res$formula_field <- ifelse(is.null(temp$formula_field), "", as.character(temp$formula_field))
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
  if (!any(colnames(fieldItems) == "option.name")) {
    fieldItems$option_id <- ""
  } else {
    fieldItems <- fieldItems |> select(-c("option_id"))
  }
  targetColnames <- intersect(kFieldItemColnames, colnames(fieldItems))
  res <- fieldItems |> select(all_of(targetColnames)) |> ConvertToCharacter() |> as.data.frame()
  return(res)
}
GetOptions <- function(json) {
  field_items <- GetFieldItemsList(json)
  if (is.null(field_items)) {
    return(NULL)
  }
  options <- field_items |> map( ~ {
    field_item <- .
    option <- field_item$option
    if (is.null(option)) {
      return(NULL)
    }
    option <- option |> discard( ~ is.list(.)) |> map_df( ~ ifelse(is.null(.), "", .))
    colnames(option) <- colnames(option) %>% str_c("option.", .)
    if (!is.null(field_item$option$controlled_terminology_data)) {
      controlled_terminology_data <- field_item$option$controlled_terminology_data |> map_df( ~ .)
      colnames(controlled_terminology_data) <- colnames(controlled_terminology_data) %>% str_c("option.controlled_terminology_data.", .) 
      option <- option |> bind_cols(controlled_terminology_data)
    }
    optionValues <- field_item$option$value |> map_df( ~ .)
    colnames(optionValues) <- colnames(optionValues) %>% str_c("option.values_", .)
    res <- option |> inner_join(optionValues, by=c("option.id"="option.values_option_id"))
    res$jpname <- json$name
    res$alias_name <- json$alias_name
    res$option_id <- res$option.id
    res$option.values_option_id <- res$option.id
    return(res)
  }) |> bind_rows() |> select(any_of(kOptionColnames)) |> distinct()
  if (length(options) == 0) {
    return(NULL)
  }
  res <- options |> ConvertToCharacter() |> as.data.frame()
  return(res)
}
GetFlipFlops <- function(json) {
  field_items <- GetFieldItemsList(json)
  if (is.null(field_items)) {
    return(NULL)
  }
  flipFlops <- field_items |> map( ~ {
    field_item <- .
    flip_flops <- field_item$flip_flops
    if (length(flip_flops) == 0) {
      return(NULL)
    }
    flip_flop_list <- flip_flops |> map( ~ {
      flip_flop <- .
      res <- flip_flop |> discard( ~ is.list(.)) |> map_df( ~ .)
      codes <- flip_flop$codes 
      if (length(codes) > 0) {
        temp_codes <- codes |> tibble()
        res <- res |> bind_cols(temp_codes)
      } else {
        res$codes <- ""
      }
      fields <- flip_flop$fields
      if (length(fields) > 0) {
        temp_fields <- fields |> tibble()
        res <- res |> bind_cols(temp_fields)
      } else {
        res$fields <- ""
      }
      res$codes <- length(codes)
      return(res)
    }) |> bind_rows()
    return(flip_flop_list)
  }) |> discard( ~ is.null(.))
  if (length(flipFlops) > 0) {
    for (i in 1:length(flipFlops)) {
      flipFlops[[i]]$jpname <- json$name
      flipFlops[[i]]$alias_name <- json$alias_name
    }
    res <- flipFlops |> bind_rows()
  } else {
    res <- NULL
  }
  return(res)
}

GetCdiscSheetConfigs <- function(json) {
  cdiscSheetConfigs <- json$cdisc_sheet_configs |> map_df( ~ {
    cdisc_sheet_config <- .
    df <- . |> discard( ~ is.list(.)) |> map_df( ~ .)
    table <- .$table |> enframe(name = "table.field", value = "table.field.value")
    res <- df |> merge(table, by=NULL)
    return(res)
  })
  cdiscSheetConfigs$jpname <- json$name
  cdiscSheetConfigs$alias_name <- json$alias_name
  res <- cdiscSheetConfigs |> select(all_of(kCdiscSheetConfigs)) |> ConvertToCharacter() 
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
  testJson <- .x
  testSheet <-.y$Field_Items
  sortJsonColnames <- colnames(testJson) |> sort()
  sortSheetColnames <- colnames(testSheet) |> sort()
  if (!is.null(testJson)) {
    testJson <- testJson |> select(all_of(sortJsonColnames))
  } 
  if (!is.null(testSheet)) {
    testSheet <- testSheet |> select(all_of(sortSheetColnames))
  }
  if (!identical(testJson, testSheet)) {
    return(list(json=.x, sheet=.y$Field_Items))
  }
  return(NULL)
}) |> keep( ~ !is.null(.))

jsonOptions <- jsonList |> map( ~ {
  json <- .
  options <- json |> GetOptions()
  return(options)
}) |> keep( ~ !is.null(.))

sheetListOption <- sheetList %>% map( ~ .$Option) |> keep( ~ !is.null(.))
testOptions <- map2(jsonOptions, sheetListOption, ~ {
  testJson <- .x
  testSheet <-.y
  sortJsonColnames <- colnames(testJson) |> sort()
  sortSheetColnames <- colnames(testSheet) |> sort()
  if (!is.null(testJson)) {
    testJson <- testJson |> select(all_of(sortJsonColnames))
  } 
  if (!is.null(testSheet)) {
    testSheet <- testSheet |> select(all_of(sortSheetColnames))
  }
  if (!identical(testJson, testSheet)) {
    return(list(json=.x, sheet=.y))
  }
  return(NULL)
}) |> keep( ~ !is.null(.))

jsonFlipFlaps <- jsonList |> map( ~ {
  json <- .
  flipFlops <- json |> GetFlipFlops()
  return(flipFlops)
}) |> keep( ~ !is.null(.))

sheetListFlip_Flops <- sheetList |> map( ~ {
  temp <- .$Flip_Flops
  res <- temp |> filter(!if_all(everything(), ~ . == ""))
  if (nrow(res) == 0) {
    return(NULL)
  }
  return(res)
}) |> keep( ~ !is.null(.))

jsonCdiscSheetConfigsPivot <- jsonList |> map( ~ {
  json <- .
  cdiscSheetConfigs <- json |> GetCdiscSheetConfigs()
  return(cdiscSheetConfigs)
}) |> keep( ~ !is.null(.))

sheetListCdiscSheetConfigsPivots <- sheetList %>% map( ~ .$Cdisc_Sheet_Configs_Pivot) |> keep( ~ !is.null(.))
# "Cdisc_Sheet_Configs"は比較の対象外とする

test <- sheetList |> map( ~ names(.))
