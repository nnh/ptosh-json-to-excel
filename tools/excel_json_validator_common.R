#' title
#' description
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
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
# item
GetRefBefAft <- function(target, befAft) {
  targetAliasNameAndNameAndLabel <<- target |> select(c("alias_name", "name", "label")) |> distinct()
  target$test <- NA
  testList <- list()
  if (befAft == "before" | befAft == "after") {
    target_colname <- str_c("validate_date_", befAft, "_or_equal_to")
    output_colname <-str_c("references_", befAft)
    for (i in 1:length(target[[target_colname]])) {
      if (!is.na(target[[target_colname]][[i]])) {
        if (str_detect(target[[target_colname]][[i]], "f[0-9]*")) {
          testList[[i]] <- target[[target_colname]][[i]] |> str_replace("f", "field")
        } else {
          testList[[i]] <- NA
        }
      } else {
        testList[[i]] <- NA
      }
    }
    
#    target$test <- ifelse(target[[target_colname]] |> str_detect("f[0-9]*"), target[[target_colname]] |> str_replace("f", "field")  , NA)
  } else {
    target_colname <- str_c("validate_", befAft)
    output_colname <-str_c(befAft, "_references")
    for (i in 1:length(target[[target_colname]])) {
      if (!is.na(target[[target_colname]][[i]])) {
        if (str_detect(target[[target_colname]][[i]], "field[0-9]*")) {
          testList[[i]] <- target[[target_colname]][[i]] |> str_extract_all("field[0-9]*")
        } else {
          testList[[i]] <- NA
        }
      } else {
        testList[[i]] <- NA
      }
    }
  }
  for (i in 1:length(testList)) {
    if (!is.na(testList[[i]])) {
      test_item <- testList[[i]][[1]] |> unique()
      refText <- ""
      for (j in 1:length(test_item)) {
        temp_test <- targetAliasNameAndNameAndLabel |> filter(alias_name == target[i, "alias_name", drop=T] & name == test_item[j])
        refText <- str_c(refText, "(", str_c(temp_test, collapse=","), ")")
        target[i, "test"] <- refText
      }
    }
  }

  output_df <- target
  output_df[[output_colname]] <- output_df$test
  output_df <- output_df |> select(-c("test"))
  return(output_df)
}
GetItemFromJson <- function(sheetList, jsonList) {
  article <- fieldItems |> map( ~ {
    df <- .
    res <- df |> map( ~ {
      if (.$type == "FieldItem::Article") {
        return(.)
      } else {
        return(NULL)
      }
    }) |> keep( ~ !is.null(.))
  })
  article_option_name <- article |> map( ~ {
    df <- .
    res <- df |> map( ~ {
      if (!is.list(.)) {
        return("")
      }
      option <- .$option
      if (is.null(option)) {
        return("")
      } else {
        temp <- list(option.name=option$name)
        return(temp)
      }
    })
    return(res)
  })
  article_validatores <- article |> map( ~ {
    df <- .
    res <- df |> map( ~ {
      if (!is.list(.)) {
        return("")
      }
      validators <- .$validators
      if (is.null(validators)) {
        return("")
      } else {
        validatorsDate <- validators$date
        temp <- list()
        temp <- temp |> 
          append(validators$presence) |> 
          append(validators$formula)
        if (!is.null(validatorsDate$validate_date_after_or_equal_to)) {
          temp2 <- list(validate_date_after_or_equal_to=validatorsDate$validate_date_after_or_equal_to)
          temp <- temp |> append(temp2) 
        }
        if (!is.null(validatorsDate$validate_date_before_or_equal_to)) {
          temp2 <- list(validate_date_before_or_equal_to=validatorsDate$validate_date_before_or_equal_to)
          temp <- temp |> append(temp2) 
        }
        return(temp)
      }
    })
    return(res)
  })
  nameAndAliasname <- jsonList |> map( ~ list(jpname=.$name, alias_name=.$alias_name))
  list_items <- list()
  for (i in 1:length(nameAndAliasname)) {
    list_items[[i]] <- list()
    for (j in 1:length(article_option_name[[i]])) {
      list_items[[i]][[j]] <- list()
      list_items[[i]][[j]] <- list_items[[i]][[j]] |> 
        append(nameAndAliasname[[i]]) |> 
        append(list(name=article[[i]][[j]]$name)) |>
        append(list(label=article[[i]][[j]]$label)) |>
        append(list(default_value=article[[i]][[j]]$default_value)) |>
        append(article_option_name[[i]][[j]]) |>
        append(article_validatores[[i]][[j]])
      list_items[[i]][[j]] <- list_items[[i]][[j]] %>% keep(~ !is.null(.) && . != "")
    }
  }
  names(list_items) <- names(jsonList)
  output_items <- sheetList$item |> 
    rename(validate_formula_message=validators.formula.validate_formula_message,
           validate_formula_if=validators.formula.validate_formula_if,
           validate_date_after_or_equal_to=validators.date.validate_date_after_or_equal_to,
           validate_date_before_or_equal_to=validators.date.validate_date_before_or_equal_to,
           validate_presence_if=validators.presence.validate_presence_if)
  itemCols <- output_items |> colnames()
  df_items <- list_items |> flatten_df()
  template_df_items <- tibble(!!!setNames(rep(list(NA), length(itemCols)), itemCols))
  df_items <- template_df_items |> bind_rows(df_items) |> filter(!is.na(jpname))
  nameAndLable <- df_items |> select("jpname", "alias_name", "name", "label")
  test_df_items <- df_items |> GetRefBefAft("before") |>  GetRefBefAft("after") |> GetRefBefAft("formula_if") |>  GetRefBefAft("presence_if")
  return(list(json=test_df_items, sheet=output_items))
}
# allocation
CheckAllocation <- function(sheetList, jsonList) {
  sheet <- sheetList[["allocation"]]
  json <- GetAllocationFromJson(jsonList)
  return(CheckTarget(sheet, json))
}
GetAllocationFromJson <- function(jsonList) {
  allocationColnames <- c("jpname", "alias_name", "is_zelen", "zelen_imbalance", "is_double_blinded", 
                          "double_blind_emails", "allocation_method", "groups.code", "groups.label", 
                          "groups.if", "references", "groups.message")
  allocationList <- jsonList |> keep( ~ .$alias_name |> str_detect("^allocation_[0-9]+"))
  if (length(allocationList) == 0) {
    df <- tibble(!!!setNames(vector("list", length(allocationColnames)), allocationColnames))
  } else {
    df <- allocationList |> map_df( ~ .|> select(all_of(allocationColnames)))
  }
  res <- GetItemsSelectColnames(df, allocationColnames)
  return(res)
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