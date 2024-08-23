#' title
#' description
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2024.8.23
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
ReadChecklist <- function(inputFolder) {
  inputPath <- here("output", inputFolder, "list", "checklist.xlsx")
  sheetNames <- inputPath |> getSheetNames()
  sheets <- sheetNames |> map( ~ read.xlsx(inputPath, ., na.strings=NULL))
  names(sheets) <- sheetNames
  return(sheets)
}
GetNameAndAliasNameByJson <- function(json_list) {
  res <- json_list |> map_df( ~ list(jpname=.$name, alias_name=.$alias_name))
  return(res)
}
LoadJsonList <- function(input_path) {
  target_Path <- input_path |> list.files(pattern="*.json", full.names=T)
  jsonList <- target_Path |> map( ~ read_json(.))
  names(jsonList) <- target_Path |> basename() |> str_remove(".json")
  return(jsonList)
}
GetFieldItemsByJsonList <- function(json_list) {
  res <- json_list |> map( ~ .$field_items)
  return(res)
}
GetItemsSelectColnames <- function(input_tibble, target_colnames) {
  if (nrow(input_tibble) == 0) {
    res <- as.data.frame(matrix("", nrow = 1, ncol = length(target_colnames)))
    colnames(res) <- target_colnames
  } else {
    res <- input_tibble |> 
      inner_join(jpNameAndAliasName, by = "alias_name") |> 
      select(all_of(target_colnames)) |> 
      as.data.frame()
  }
  return(res)
}

CheckTarget <- function(sheet, json) {
  if (!identical(sheet, json)) {
    return(list(sheet=sheet, json=json))
  }
  return(NULL)
}
# action
CheckAction <- function(sheetList) {
  sheet <- sheetList[["action"]]
  json <- GetActionFromJson()
  return(CheckTarget(sheet, json))
}
GetActionFromJson <- function() {
  df <- fieldItems |> map( ~ {
    fieldItem <- .
    res <- fieldItem |> map( ~ .$flip_flops) |> keep( ~ length(.) > 0)
    return(res)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "id", "field_item_id", "field_item_id.name", "field_item_id.label", "codes", "fields", "fields.label"))
  return(res)
}
# display
CheckDisplay <- function(sheetList) {
  sheet <- sheetList[["display"]]
  json <- GetDisplayFromJson()
  return(CheckTarget(sheet, json))
}
GetDisplayFromJson <- function() {
  df <- fieldItems |> map_df( ~ {
    fieldItem <- .
    res <- fieldItem |> map( ~ {
      type <- .$type
      is_invisible <- .$is_invisible
      if (type == "FieldItem::Assigned" & !is_invisible) {
        return(.)
      }
      if (type == "FieldItem::Article" & is_invisible) {
        return(.)
      }
      return(NULL)
    }) |> keep( ~ !is.null(.))
    return(res)
  })
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label"))
  return(res)
}
# number
CheckNumber <- function(sheetList) {
  sheet <- sheetList[["number"]]
  json <- GetNumberFromJson()
  return(CheckTarget(sheet, json))
}
GetNumberFromJson <- function() {
  number_target <- fieldItems |> map( ~ {
    fieldItem <- .
    res <- fieldItem |> map( ~ {
      lessThan <- .$validators$numericality$validate_numericality_less_than_or_equal_to
      greaterThan <- .$validators$numericality$validate_numericality_greater_than_or_equal_to
      if (is.null(lessThan) & 
          is.null(greaterThan)) {
        return(NULL)
      }
      defaultValue <- ifelse(is.null(.$default_value), NA, .$default_value)
      res <- list(name=.$name, label=.$label, default_value=.$default_value, 
                  validators.numericality.validate_numericality_less_than_or_equal_to=lessThan,
                  validators.numericality.validate_numericality_greater_than_or_equal_to=greaterThan, 
                  default_value=defaultValue)
      return(res)
    }) |> keep( ~ !is.null(.)) |> list_flatten()
  }) |> keep( ~ length(.) > 0)
  df_number <- number_target |> map_df( ~ .)
  df_number$alias_name <- names(number_target)
  res <- GetItemsSelectColnames(df_number, c("jpname", "alias_name", "name", "label", "default_value", "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to"))
  res <- res |> mutate(default_value = as.numeric(default_value))
  return(res)
}
# name
CheckName <- function(sheetList, jsonList) {
  sheet <- sheetList[["name"]]
  json <- jsonList |> map_df( ~ list(name=.$name, alias_name=.$alias_name, images_count=.$images_count)) |> as.data.frame()
  json <- json |> mutate(images_count = as.numeric(images_count))
  return(CheckTarget(sheet, json))
}
# options
CheckOption <- function(sheetList) {
  sheet <- sheetList[["option"]]
  json <- GetOptionFromJson()
  return(CheckTarget(sheet, json))
}
GetOptionFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> map( ~ {
      item <- .
      if (item$type != "FieldItem::Article") {
        return(NULL)
      }
      option <- item$option
      if (is.null(option)){
        return(NULL)
      }
      option$values <- option$values |> keep( ~ .$is_usable)
      optionName <- option$name
      optionValues <- option$values |> map( ~  list(option.name=optionName, option.values_name=.$name, option.values_seq=.$seq, option.values_code=.$code, option.values_is_usable=.$is_usable))
      return(optionValues)
    }) |> keep( ~ !is.null(.)) |> map_df(~ .)
    res$alias_name <- aliasName
    return(res)
  }) |> bind_rows() |> distinct()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "option.name", "option.values_name", "option.values_seq", "option.values_code", "option.values_is_usable"))
  res <- res |> mutate(option.values_seq = as.numeric(option.values_seq))
  return(res)
}
# content
CheckContent <- function(sheetList) {
  sheet <- sheetList[["comment"]]
  json <- GetContentFromJson()
  return(CheckTarget(sheet, json))
}
GetContentFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ !is.null(.$content))
    if (length(res) == 0) {
      return(NULL)
    }
    content <- res |> map_df( ~ list(name=.$name, label=.$label, content=.$content))
    content$alias_name <- aliasName
    return(content)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "content"))
  return(res)
}
# ------ main ------