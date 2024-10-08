#' test script
#' 
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2024.9.5
# ------ libraries ------
library(tidyverse, warn.conflicts=F)
library(here, warn.conflicts=F)
library(openxlsx, warn.conflicts=F)
library(jsonlite, warn.conflicts=F)
# ------ constants ------
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
  return(home_dir)
}
GetTargetFolder <- function(trialName) {
  outputPath <- here("output")
  outputDirs <- outputPath |> list.dirs(recursive=F, full.names=F)
  targetDirs <- outputDirs |> str_extract_all(str_c("output_", "[0-9]+_", trialName)) |> keep( ~ length(.) > 0)
  if (length(targetDirs) == 0) {
    stop(str_c("No folders found for trial name: ", trialName))
  }
  df <- tibble(folderName=targetDirs)
  df$dateTime <- df$folderName |> str_extract("[0-9]+") |> as.numeric()
  latestFolder <- df |> filter(dateTime == max(dateTime, na.rm=T)) %>% .$folderName |> list_c()
  print(str_c("target: ", latestFolder))
  return(latestFolder)
}
GetJsonAndSheet <- function(trialName) {
  targetFolder <- GetTargetFolder(trialName)
  sheetList <- targetFolder |> ReadChecklist()
  jsonList <- here(str_c("input_", trialName)) |> LoadJsonList()
  return(list(sheetList=sheetList, jsonList=jsonList))
}
ReadChecklist <- function(inputFolder) {
  inputPath <- here("output", inputFolder, "list", "checklist.xlsx")
  sheetNames <- inputPath |> getSheetNames()
  sheets <- sheetNames |> map( ~ read.xlsx(inputPath, ., na.strings=NULL))
  names(sheets) <- sheetNames
  return(sheets)
}
GetAliasnameAndFieldIdAndLabel <- function(fieldItems) {
  res <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> map_df( ~ c(fields=.$name, fields.label=.$label))
    res$alias_name <- aliasName
    return(res)
  }) |> bind_rows()
  return(res)
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
  } else {
    target_colname <- str_c("validate_", befAft)
    output_colname <-str_c(befAft, "_references")
  }
  for (i in 1:length(target[[target_colname]])) {
    if (!is.na(target[[target_colname]][[i]])) {
      temp <- target[[target_colname]][[i]] %>% gsub("f(\\d+)", "field\\1", .)
      if (str_detect(temp, "field[0-9]*")) {
          testList[[i]] <- temp |> str_extract_all("field[0-9]*")
      } else {
          testList[[i]] <- NA
      }
    } else {
      testList[[i]] <- NA
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
  }) |> keep( ~ length(.) > 0)
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
  nameAndAliasname <- jsonList |> map( ~ list(jpname=.$name, alias_name=.$alias_name)) |> keep( ~ !is.null(article[[.$alias_name]]))
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
  names(list_items) <- nameAndAliasname |> map_chr( ~ .$alias_name)
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
  sheet$groups.if_references <- ifelse(is.na(sheet$groups.if_references), "", sheet$groups.if_references)
  sheet$formula_field_references <- sheet$formula_field_references |> str_remove_all(" ")
  json$formula_field_references <- json$formula_field_references |> str_remove_all(" ")
  return(CheckTarget(sheet, json))
}
GetAllocationFromJson <- function(jsonList) {
  allocationColnames <- c("jpname", "alias_name", "is_zelen", "zelen_imbalance", "is_double_blinded", 
                          "double_blind_emails", "allocation_method", "groups.if", "groups.if_references", "groups.code", "groups.label", 
                          "groups.message", "formula_field", "formula_field_references")
  allocationList <- jsonList |> 
    keep(~ .$alias_name |> str_detect("(?i)^allocation([0-9]+)?$"))
  if (length(allocationList) == 0) {
    df <- tibble(!!!setNames(vector("list", length(allocationColnames)), allocationColnames))
  } else {
    aliasnameAndFieldIdAndLabel <- GetAliasnameAndFieldIdAndLabel(fieldItems)
    df <- allocationList |> map( ~ {
      name <- .$name
      aliasName <- .$alias_name
      allocation <- .$allocation
      groups <- allocation$groups |> map_df( ~ c(groups.code=.$code, groups.label=.$label, groups.if=.$`if`, groups.message=.$message))
      groups$alias_name <- aliasName
      groups$is_zelen <- allocation$is_zelen
      groups$zelen_imbalance <- allocation$zelen_imbalance |> as.numeric()
      groups$is_double_blinded <- allocation$is_double_blinded
      groups$double_blind_emails <- allocation$double_blind_emails
      groups$allocation_method <- allocation$allocation_method
      return(groups)
    }) |> bind_rows()
    temp_ref <- df$groups.if |> str_remove_all(" ") |> str_extract_all("ref\\('[a-zA-Z0-9]+',[0-9]+\\)")
    temp_ref2 <- temp_ref |> map( ~ {
      if (length(.) == 0) {
        return(c(groups.if_references=""))
      }
      temp <- . |> map_chr( ~ {
        temp <- . |> str_split(",") |> list_c()
        aliasName <- temp[1] |> str_remove("ref") |> str_extract("[a-zA-Z0-9]+") |> unlist()
        fieldName <- temp[2] |> str_extract("[0-9]+") %>% str_c("field", .)
        label <- aliasnameAndFieldIdAndLabel |> filter(alias_name == aliasName & fields == fieldName) %>% .[1, "fields.label"] |> unlist()
        res <- str_c("(", aliasName, ",", fieldName, ",", label, ")")
        return(res)
      })
      res <- temp |> unique() |> str_c(collapse="")
      return(c(groups.if_references=res))
    }) |> bind_rows()
    df <- df |> cbind(temp_ref2)
    temp_formula_fields <- allocationList |> map( ~ {
      myAliasName <- .$alias_name
      fieldItems <- .$field_items |> keep( ~ .$type == "FieldItem::Allocation")
      if (length(fieldItems) == 0) {
        return(NULL)
      }
      formula_field <- fieldItems |> map( ~ {
        input_text <- .$formula_field
        temp <- input_text |> str_remove_all(" ") |> str_extract_all("ref\\('[a-zA-Z0-9]+',[0-9]+\\)")|> list_c() 
        if (length(temp) == 0) {
          return(tibble(alias_name=myAliasName, formula_field=.$formula_field, formula_field_references=.$formula_field))
        }
        for (i in 1:length(temp)) {
          temp2 <- temp[[i]] |> str_split(",") |> list_c()
          aliasName <- temp2[1] |> str_remove("ref") |> str_extract("[a-zA-Z0-9]+") |> unlist()
          fieldName <- temp2[2] |> str_extract("[0-9]+") %>% str_c("field", .)
          label <- aliasnameAndFieldIdAndLabel |> filter(alias_name == aliasName & fields == fieldName) %>% .[1, "fields.label"] |> unlist()
          res <- str_c("(", aliasName, ",", fieldName, ",", label, ")")
          temp3 <- temp[i] |> str_replace(fixed("ref("), "ref\\(") |> str_replace(fixed(")"), "\\)")
          input_text <- input_text  |> str_remove_all(" ") |> str_replace(temp3, res)
        }
        return(tibble(alias_name=myAliasName, formula_field=.$formula_field, formula_field_references=input_text))
      }) |> bind_rows()
      if (nrow(formula_field) == 0) {
        return(NULL)
      }
      temp0 <- formula_field[1, "alias_name"] |> as.character() 
      temp1 <- formula_field$formula_field |> paste(collapse=", ")
      temp2 <- formula_field$formula_field_references |> paste(collapse=", ")
      res <- tibble(alias_name=temp0, formula_field=temp1, formula_field_references=temp2)
      return(res)
    }) |> bind_rows()
    df <- df |> left_join(temp_formula_fields, by="alias_name")
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
  action <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    flip_flops <- fieldItem |> map( ~ list(aliasName=aliasName, name=.$name, label=.$label, flip_flops=.$flip_flops)) |> keep( ~ length(.$flip_flops) > 0)
    if (length(flip_flops) == 0) {
      return(NULL)
    }
    res <- flip_flops |> map( ~ {
      flip_flop <- .$flip_flops
      field_item_id.name <- .$name
      field_item_id.label <- .$label
      alias_name <- .$aliasName
      res <- flip_flop |> map_df( ~ {
        temp <- crossing(codes=list_c(.$code), fields=list_c(.$fields)) |> tibble()
        temp$alias_name <- alias_name
        temp$id <- .$id
        temp$field_item_id <- .$field_item_id
        temp$field_item_id.name <- field_item_id.name
        temp$field_item_id.label <- field_item_id.label
        return(temp)
      })
      return(res)
    })
    return(res)
  }) |> keep( ~ !is.null(.)) |> bind_rows()
  aliasnameAndFieldIdAndLabel <- GetAliasnameAndFieldIdAndLabel(fieldItems)
  if (nrow(action) > 0) {
    df <- action |> inner_join(aliasnameAndFieldIdAndLabel, by=c("alias_name", "fields"))
  } else {
    df <- action
  }
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
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> map( ~ {
      temp <- .
      type <- temp$type
      is_invisible <- temp$is_invisible
      if (type == "FieldItem::Assigned" & !is_invisible) {
        res <- tibble(alias_name=aliasName, name=temp$name, label=temp$label)
        return(res)
      }
      if (type == "FieldItem::Article" & is_invisible) {
        res <- tibble(alias_name=aliasName, name=temp$name, label=temp$label)
        return(res)
      }
      return(NULL)
    }) |> keep( ~ !is.null(.))
    return(res)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label"))
  return(res)
}
# number
CheckNumber <- function(sheetList) {
  sheet <- sheetList[["number"]]
  sheet$default_value <- sheet$default_value |> as.character()
  json <- GetNumberFromJson()
  json$default_value <- json$default_value |> as.character()
  return(CheckTarget(sheet, json))
}
GetNumberFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> map( ~ {
      lessThan <- .$validators$numericality$validate_numericality_less_than_or_equal_to
      greaterThan <- .$validators$numericality$validate_numericality_greater_than_or_equal_to
      if (is.null(lessThan)) {
        lessThan <- ""
      }
      if (is.null(greaterThan)) {
        greaterThan <- ""
      }
      if (lessThan == "" & 
          greaterThan == "") {
        return(NULL)
      }
      defaultValue <- ifelse(is.null(.$default_value), NA, .$default_value)
      res <- list(alias_name=aliasName, name=.$name, label=.$label, default_value=defaultValue, 
                  validators.numericality.validate_numericality_less_than_or_equal_to=lessThan,
                  validators.numericality.validate_numericality_greater_than_or_equal_to=greaterThan)
      return(res)
    }) |> keep( ~ !is.null(.))
  }) |> keep( ~ length(.) > 0)
  df_number <- df |> map_df( ~ .)
  res <- GetItemsSelectColnames(df_number, c("jpname", "alias_name", "name", "label", "default_value", "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to"))
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
# explanation
CheckExplanation <- function(sheetList) {
  sheet <- sheetList[["explanation"]]
  json <- GetExplanationFromJson()
  return(CheckTarget(sheet, json))
}
GetExplanationFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ !is.null(.$description))
    explanation <- res |> map_df( ~ list(name=.$name, label=.$label, description=.$description))
    explanation$alias_name <- aliasName
    return(explanation)
  }) |> bind_rows() |> filter(description != "")
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "description"))
  return(res)
}
# presence
CheckPresence <- function(sheetList) {
  sheet <- sheetList[["presence"]]
  json <- GetPresenceFromJson()
  return(CheckTarget(sheet, json))
}
GetPresenceFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ .$type == "FieldItem::Article" & is.null(.$validators$presence))
    presence <- res |> map_df( ~ list(name=.$name, label=.$label))
    presence$alias_name <- aliasName
    return(presence)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label"))
  return(res)
}
# master
CheckMaster <- function(sheetList) {
  sheet <- sheetList[["master"]]
  json <- GetMasterFromJson()
  return(CheckTarget(sheet, json))
}
GetMasterFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ !is.null(.$link_type))
    master <- res |> map_df( ~ list(name=.$name, label=.$label, link_type=.$link_type))
    master$alias_name <- aliasName
    return(master)
  }) |> bind_rows() |> filter(link_type != "")
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "link_type"))
  return(res)
}
# visit
CheckVisit <- function(sheetList) {
  sheet <- sheetList[["visit"]]
  json <- GetVisitFromJson()
  return(CheckTarget(sheet, json))
}
GetVisitFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ .$label == "Visit Number")
    visit <- res |> map_df( ~ list(name=.$name, default_value=.$default_value))
    visit$alias_name <- aliasName
    return(visit)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "default_value"))
  return(res)
}
# alert
CheckAlert <- function(sheetList) {
  sheet <- sheetList[["alert"]]
  json <- GetAlertFromJson()
  return(CheckTarget(sheet, json))
}
GetAlertFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |>
      keep( ~ !is.null(.$normal_range$less_than_or_equal_to) | !is.null(.$normal_range$greater_than_or_equal_to))
    if (length(res) > 0) {
      for (i in 1:length(res)) {
        temp <- res[[i]]
        remove_flag <- T
        if (!is.null(temp$normal_range$less_than_or_equal_to)) {
          if (temp$normal_range$less_than_or_equal_to != "") {
            remove_flag <- F
          }
        }
        if (!is.null(temp$normal_range$greater_than_or_equal_to)) {
          if (temp$normal_range$greater_than_or_equal_to != "") {
            remove_flag <- F
          }
        }
        if (remove_flag) {
          res[[i]] <- NULL
        }
      }
    }
    alert <- res |> map_df( ~ {
      temp_alert <- .
      normal_range <- temp_alert$normal_range
      less_than <- ifelse(!is.null(normal_range$less_than_or_equal_to), normal_range$less_than_or_equal_to, NA_real_)
      greater_than <- ifelse(!is.null(normal_range$greater_than_or_equal_to), normal_range$greater_than_or_equal_to, NA_real_)
      res <- list(name=.$name, 
                  label=.$label,
                  normal_range.less_than_or_equal_to=less_than,
                  normal_range.greater_than_or_equal_to=greater_than)
      return(res)
    })
    alert$alias_name <- aliasName
    return(alert)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to"))
  return(res)
}
# title
CheckTitle <- function(sheetList) {
  sheet <- sheetList[["title"]]
  json <- GetTitleFromJson()
  return(CheckTarget(sheet, json))
}
GetTitleFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ .$type == "FieldItem::Heading")
    title <- res |> map_df( ~ list(name=.$name, label=.$label, level=.$level))
    title$alias_name <- aliasName
    return(title)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "level"))
  res <- res |> mutate(level = as.numeric(level))
  return(res)
}
# assigned
CheckAssigned <- function(sheetList) {
  sheet <- sheetList[["assigned"]]
  json <- GetAssignedFromJson()
  return(CheckTarget(sheet, json))
}
GetAssignedFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x 
    aliasName <- .y
    res <- fieldItem |> keep( ~ .$type == "FieldItem::Assigned")
    assigned <- res |> map_df( ~ list(name=.$name, label=.$label, default_value=.$default_value))
    assigned$alias_name <- aliasName
    return(assigned)
  }) |> bind_rows()
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "default_value"))
  return(res)
}
# ------ main ------