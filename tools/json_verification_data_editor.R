#' title
#' description
#' @file json_verification_data_editor.R
#' @author Mariko Ohtsuka
#' @date 2024.9.11
# ------ libraries ------
library(here)
source(here("tools", "by_sheet_excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
kTestTarget <- c("Field_Items", "Flip_Flops", "Cdisc_Sheet_Configs_Pivot")
#kTestTarget <- c("Field_Items", "Option", "Flip_Flops", "Cdisc_Sheet_Configs_Pivot")
# ------ functions ------
CompareJsonToSheet <- function(jsonTarget, sheetTarget) {
  temp <- identical(nrow(jsonTarget), nrow(sheetTarget))
  if (!temp) {
    stop(str_c("行数不一致：json=", nrow(jsonTarget), ", sheet=", nrow(sheetTarget)))
  }
  identical(ncol(jsonTarget), ncol(sheetTarget))
  if (!temp) {
    stop(str_c("列数不一致：json=", ncol(jsonTarget), ", sheet=", ncol(sheetTarget)))
  }
  jsonTarget <- jsonTarget |> select(colnames(sheetTarget)) |> ConvertToCharacter() |> as.data.frame()
  for (i in 1:nrow(jsonTarget)) {
    if (!identical(jsonTarget[i, ], sheetTarget[i, ])) {
      for (j in 1:ncol(jsonTarget)) {
        if (!identical(jsonTarget[i, j], sheetTarget[i, j])) {
          print("値不一致")
          print(str_c("列名：", colnames(jsonTarget)[j]))
          print(str(jsonTarget[i, j]))
          print(str(sheetTarget[i, j]))
          stop()
        }
      }
    }
  }
  return("check ok.")
}
EditJsonVerificationData <- function(json, sheet) {
  jpname <- json$name
  alias_name <- json$alias_name
  # cdisc_sheet_configs
  jsonCdisc <- json$cdisc_sheet_configs %>% map_df( ~ {
    temp <- .
    df_base <- data.frame()
    df_base[1, "id"] <- temp$id |> as.character()
    df_base[1, "sheet_id"] <- temp$sheet_id |> as.character()
    df_base[1, "prefix"] <- temp$prefix
    df_base[1, "label"] <- temp$label
    df_base$jpname <- jpname
    df_base$alias_name <-alias_name
    df_table <- temp$table |> enframe()
    colnames(df_table) <- c("table.field", "table.field.value")
    df_table$table.field.value <- df_table$table.field.value |> unlist()
    res <- df_base |> merge(df_table, by=NULL) |> select(colnames(sheet$Cdisc_Sheet_Configs_Pivot))
    return(res)
  }) 
  # flip_flops
  jsonFlipFlops <- json$field_items |> map( ~ {
    flipFlops <- .$flip_flops
    if (length(flipFlops) == 0) {
      return(NULL)
    }
    df_codes <- flipFlops |> map( ~ {
      res <- .$codes |> as.data.frame()
      colnames(res) <- c("codes")
      return(res)
    })
    df_fields <- flipFlops |> map_df( ~ {
      res <- .$fields |> map( ~ as.data.frame(.)) |> bind_rows()
      colnames(res) <- c("fields")
      return(res)
    })
    res <- df_fields[[1]] |> merge(df_codes[[1]], by=NULL)
    colnames(res)<- c("fields", "codes")
    temp <- flipFlops |> map(~ discard(., is.list)) |> map_df( ~ .)
    res <- res |> merge(temp, by=NULL)
    return(res)
  }) |> discard( ~ is.null(.)) |> bind_rows()
  jsonFlipFlops$jpname <- jpname
  jsonFlipFlops$alias_name <- alias_name
  
  # fieldItems
  jsonFieldItems <- json$field_items |> map( ~ discard(., is.list)) |> map_df( ~ .)
  jsonValidators <- json$field_items |> map( ~ list(id=.$id, validators=.$validators)) |> discard( ~ is.null(.$validators))
  jsonValidators <- jsonValidators |> map( ~ {
    if (length(.$validators) == 0) {
      target <- .
      target$validators <- NULL
      res <- as.data.frame(target)
      res$validators.presence <- F
      return(res)
    }
    target <- .
    df_date <- .$validators$date |> data.frame()
    df_formula <- .$validators$formula |> data.frame()
    df_presence <- .$validators$presence |> data.frame()
    res <- NULL
    if (length(df_date) > 0) {
      tempColnamees <- df_date |> colnames() %>% str_c("validators.date.", .)
      colnames(df_date) <- tempColnamees
      if (is.null(res)) {
        res <- df_date
      } else {
        res <- res |> merge(df_date, by=NULL)
      }
    }
    if (length(df_formula) > 0) {
      tempColnamees <- df_formula |> colnames() %>% str_c("validators.formula.", .)
      colnames(df_formula) <- tempColnamees
      if (is.null(res)) {
        res <- df_formula
      } else {
        res <- res |> merge(df_formula, by=NULL)
      }
    }
    if (length(df_presence) > 0) {
      tempColnamees <- df_presence |> colnames() %>% str_c("validators.presence.", .)
      colnames(df_presence) <- tempColnamees
      if (is.null(res)) {
        res <- df_presence
      } else {
        res <- res |> merge(df_presence, by=NULL)
      }
    } else {
    }
    res$validators.presence <- !is.null(.$validators$presence)
    res$id <- target$id
    return(res)
  }) |> bind_rows()
  jsonFieldItems <- jsonFieldItems |> left_join(jsonValidators, by="id")
  jsonOptionName <- json$field_items |> map( ~ list(id=.$id, option=.$option)) |> discard( ~ is.null(.$option))
  jsonOptionName <- jsonOptionName |> map_df( ~ list(id=.$id, option.name=.$option$name))
  jsonFieldItems <- jsonFieldItems |> left_join(jsonOptionName, by="id")
  jsonFieldItems$jpname <- jpname
  jsonFieldItems$alias_name <- alias_name
  jsonFieldItems$argument_type <- ""
  jsonFieldItems$auto_calc_field <- ""
  jsonFieldItems$flip_flops <- ""
  jsonFieldItems$formula_field <- ""
  jsonFieldItems$reference_field <- ""
  jsonFieldItems$reference_type <- ""
  jsonFieldItems$term_code <- ""
  jsonFieldItems$option_id <- NULL
  jsonFieldItems$validators.presence <- ifelse(is.na(jsonFieldItems$validators.presence), F, jsonFieldItems$validators.presence)
  
  return(list(Cdisc_Sheet_Configs_Pivot=jsonCdisc,
              Flip_Flops=jsonFlipFlops,
              Field_Items=jsonFieldItems))
  
}
# ------ main ------
#save(blin_b_all, file = "C:\\Users\\MarikoOhtsuka\\Downloads\\blin_b_all.RData")
#load("C:\\Users\\MarikoOhtsuka\\Downloads\\blin_b_all.RData")
targetSheet <- blin_b_all$sheet$ie_100
test <- EditJsonVerificationData(blin_b_all$json$ie_100, targetSheet)
res <- kTestTarget |> map( ~ CompareJsonToSheet(test[[.]], targetSheet[[.]]))
