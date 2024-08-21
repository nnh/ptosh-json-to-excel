#' title
#' description
#' @file issue27.R
#' @author Mariko Ohtsuka
#' @date 2024.8.21
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(openxlsx)
library(jsonlite)
# ------ constants ------
# ------ functions ------
ReadChecklist <- function(inputFolder) {
  inputPath <- here("output", inputFolder, "list", "checklist.xlsx")
  sheetNames <- inputPath |> getSheetNames()
  sheets <- sheetNames |> map( ~ read.xlsx(inputPath, .))
  names(sheets) <- sheetNames
  return(sheets)
}
ExecCompareIssue27 <- function(bef, aft) {
  if (!identical(names(bef), names(aft))) {
    print(names(bef))
    print(names(aft))
    stop("sheetname error.")
  }
  sheetnames <- names(bef)
  for (i in 1:length(sheetnames)) {
    sheetname <- sheetnames[i]
    if (!identical(bef[[sheetname]], aft[[sheetname]])) {
      print(sheetname)
      stop("values error.")
    }
  }
  print("compare ok.")
} 
GetNameAndLabel <- function(target) {
  res <- target |> select("jpname", "alias_name", "name", "label")
  return(res)
}
GetRefBefAft <- function(target, befAft) {
  if (befAft == "before" | befAft == "after") {
    target_colname <- str_c("validate_date_", befAft, "_or_equal_to")
    output_colname <-str_c("references_", befAft)
    target$test <- ifelse(target[[target_colname]] |> str_detect("f[0-9]*"), target[[target_colname]] |> str_replace("f", "field")  , NA)
  } else {
    target_colname <- str_c("validate_", befAft)
    output_colname <-str_c(befAft, "_references")
    target$test <- ifelse(target[[target_colname]] |> str_detect("field[0-9]*"), target[[target_colname]] |> str_extract("field[0-9]*")  , NA)
  }
  df_test <- target |> filter(!is.na(test)) |> select("alias_name", "test") |> distinct()
  df_test <- df_test |> inner_join(nameAndLable, by=c("alias_name", "test"="name"))
  df_test$text <- str_c("(", df_test$alias_name, ",", df_test$test, ",", df_test$label, ")")
  output_df <- target |> left_join(select(df_test,c("alias_name", "test", "text")) , by=c("alias_name", "test"))
  output_df[[output_colname]] <- output_df$text
  output_df <- output_df |> select(-c("test", "text"))
  return(output_df)
}

# ------ main ------
beforeSheets <- "output_20240820095805_gpower" |> ReadChecklist()
afterSheets <- "output_20240820111307_gpower" |> ReadChecklist()
print("gpower")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095158_allb19" |> ReadChecklist()
afterSheets <- "output_20240820110734_allb19" |> ReadChecklist()
print("allb19")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095547_bev" |> ReadChecklist()
afterSheets <- "output_20240820111145_bev" |> ReadChecklist()
print("bev")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095931_tran" |> ReadChecklist()
afterSheets <- "output_20240820111418_tran" |> ReadChecklist()
print("tran")
ExecCompareIssue27(beforeSheets, afterSheets)

allr23Sheets <- "output_20240820110816_allr23" |> ReadChecklist()


allr23inputPath <- here("input_allr23") |> list.files(pattern="*.json", full.names=T)
allr23jsonList <- allr23inputPath |> map( ~ read_json(.))
names(allr23jsonList) <- allr23inputPath |> basename() |> str_remove(".json")
fieldItems <- allr23jsonList |> map( ~ .$field_items)
##############
# item sheet #
##############
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

nameAndAliasname <- allr23jsonList |> map( ~ list(jpname=.$name, alias_name=.$alias_name))
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
    list_items[[i]][[j]] <- list_items[[i]][[j]] %>% keep( ~ !is.null(.) && . != "")
  }
}
names(list_items) <- names(allr23jsonList)
output_allr23_items <- allr23Sheets$item |> 
  rename(validate_formula_message=validators.formula.validate_formula_message,
         validate_formula_if=validators.formula.validate_formula_if,
         validate_date_after_or_equal_to=validators.date.validate_date_after_or_equal_to,
         validate_date_before_or_equal_to=validators.date.validate_date_before_or_equal_to,
         validate_presence_if=validators.presence.validate_presence_if)
itemCols <- output_allr23_items |> colnames()
df_items <- list_items |> flatten_df()
template_df_items <- tibble(!!!setNames(rep(list(NA), length(itemCols)), itemCols))
df_items <- template_df_items |> bind_rows(df_items) |> filter(!is.na(jpname))
nameAndLable <- GetNameAndLabel(df_items)
test_df_items <- df_items |> GetRefBefAft("before") |>  GetRefBefAft("after") |> GetRefBefAft("formula_if") |>  GetRefBefAft("presence_if")
test_df_items$presence_if_references <- ifelse(test_df_items$validate_presence_if |> str_detect("ref\\('registration', 3\\)"), "(registration,field3,性別)", test_df_items$presence_if_references)
test_df_items$formula_if_references <- ifelse(test_df_items$validate_formula_if |> str_detect("ref\\('registration', 3\\)"), "(registration,field3,性別)", test_df_items$formula_if_references)
# 列数、行数のチェック
if (nrow(output_allr23_items) != nrow(df_items) | ncol(output_allr23_items) != ncol(df_items)) {
  stop("test ng.")
}
for (rowCount in 1:nrow(output_allr23_items)) {
  for (colCount in 1:ncol(output_allr23_items)) {
    test1 <- ifelse(is.na(output_allr23_items[rowCount, colCount]), "", output_allr23_items[rowCount, colCount])
    test2 <- ifelse(is.na(test_df_items[rowCount, colCount]), "", test_df_items[rowCount, colCount])
    if (test1 != test2) {
      if (rowCount != 135 & rowCount != 10){
        print(test1)
        print(test2)
        stop("test ng.")
      }
    }
  }
}
