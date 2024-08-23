#' title
#' description
#' @file issue27_28.R
#' @author Mariko Ohtsuka
#' @date 2024.8.22
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
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
IsExceptionItem <- function(rowCount, colCount, test2) {
  (rowCount == 135 & colCount == 10 & test2 == "(primary_diag_100,field87,プロトコール)(primary_diag_100,field85,アーム)") ||
    ((rowCount == 158 | rowCount == 159) & colCount == 15 & test2 == "(prior_medications_110,field2,造血細胞移植歴の有無)(prior_medications_110,field4,ドナー情報)")
}

# ------ main ------
beforeSheets <- "output_20240820095805_gpower" |> ReadChecklist()
afterSheets <- "output_20240822165938_gpower" |> ReadChecklist()
print("gpower")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095158_allb19" |> ReadChecklist()
afterSheets <- "output_20240822165425_allb19" |> ReadChecklist()
print("allb19")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095547_bev" |> ReadChecklist()
afterSheets <- "output_20240822165821_bev" |> ReadChecklist()
print("bev")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095931_tran" |> ReadChecklist()
afterSheets <- "output_20240822170047_tran" |> ReadChecklist()
print("tran")
ExecCompareIssue27(beforeSheets, afterSheets)

allr23Sheets <- "output_20240822165505_allr23" |> ReadChecklist()


#allr23inputPath <- here("input_allr23") |> list.files(pattern="*.json", full.names=T)
#allr23jsonList <- allr23inputPath |> map( ~ read_json(.))
#names(allr23jsonList) <- allr23inputPath |> basename() |> str_remove(".json")
allr23jsonList <- here("input_allr23") |> LoadJsonList()
fieldItems <- allr23jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- allr23jsonList |>  GetNameAndAliasNameByJson()
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
      if (IsExceptionItem(rowCount, colCount, test2)) {
        print(test1)
        print(test2)
        stop("test ng.")
      }
    }
  }
}
print("item check end.")
##############
# allocation #
##############
check_allocation <- allr23Sheets$allocation |> map_lgl( ~ all(is.na(.x) | .x == "")) |> all()
if (check_allocation) {
  print("allocation check end.")
  
} else {
  stop("allocation ng.")
}
##########
# action #
##########
check_action <- allr23Sheets$action |> map_lgl( ~ all(is.na(.x) | .x == "")) |> all()
flipFlops <- fieldItems |> map( ~ {
  temp <- .
  res <- . |> map( ~ .$flip_flops |> list_flatten()) |> list_flatten()
  return(res)
}) |> flatten_chr()
if (flipFlops |> length() == 0 & check_action) {
  print("action check end.")
} else {
  stop("action ng.")
}
###########
# display #
###########
check_display <- allr23Sheets$display |> map_lgl( ~ all(is.na(.x) | .x == "")) |> all()
display <- fieldItems |> map( ~ {
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
}) |> list_flatten()
if (display |> length() == 0 & check_display) {
  print("display check end.")
} else {
  stop("display ng.")
}
##########
# number #
##########
check_number <- allr23Sheets$number
number_target <- fieldItems |> map( ~ {
  fieldItem <- .
  res <- fieldItem |> map( ~ {
    lessThan <- .$validators$numericality$validate_numericality_less_than_or_equal_to
    greaterThan <- .$validators$numericality$validate_numericality_greater_than_or_equal_to
    if (is.null(lessThan) & 
        is.null(greaterThan)) {
      return(NULL)
    }
    res <- list(name=.$name, label=.$label, default_value=.$default_value, 
                validators.numericality.validate_numericality_less_than_or_equal_to=lessThan,
                validators.numericality.validate_numericality_greater_than_or_equal_to=greaterThan)
    return(res)
  }) |> keep( ~ !is.null(.)) |> list_flatten()
}) |> keep( ~ length(.) > 0)
df_number <- number_target |> map_df( ~ .)
df_number$alias_name <- names(number_target)
test_number1 <- check_number |> anti_join(df_number, by=c("alias_name", "name")) |> nrow() == 0
test_number2 <- nrow(check_number) & nrow(df_number)
test_number3 <- T
for (i in 1:nrow(check_number)) {
  if (check_number[i, "validators.numericality.validate_numericality_less_than_or_equal_to"] != df_number[i, "validators.numericality.validate_numericality_less_than_or_equal_to"]) {
    test_number3 <- F
    break
  }
  if (check_number[i, "validators.numericality.validate_numericality_greater_than_or_equal_to"] != df_number[i, "validators.numericality.validate_numericality_greater_than_or_equal_to"]) {
    test_number3 <- F
    break
  }
}
if (test_number1 & test_number2 & test_number3) {
  print("number check end.")
} else {
  stop("number ng.")
}
########
# name #
########
check_name <- allr23Sheets |> CheckName(allr23jsonList)
##########
# option #
##########
check_option <- allr23Sheets |> CheckOption()
###########
# comment #
###########
check_content <- allr23Sheets |> CheckContent()

#########
# alert #
#########
normalRanges <- fieldItems |> map( ~ {
  fieldItem <- .
  res <- fieldItem |> map( ~ .$normal_range) |> list_flatten()
  return(res)
}) |> list_flatten()
