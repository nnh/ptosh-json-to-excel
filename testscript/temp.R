json <- blin_b_all$json$ie_100
sheet <- blin_b_all$sheet$ie_100
# cdisc_sheet_configs
jsonIeCdisc <- json$cdisc_sheet_configs %>% map_df( ~ {
  temp <- .
  df_base <- data.frame()
  df_base[1, "id"] <- temp$id |> as.character()
  df_base[1, "sheet_id"] <- temp$sheet_id |> as.character()
  df_base[1, "prefix"] <- temp$prefix
  df_base[1, "label"] <- temp$label
  df_base$jpname <- "選択除外基準確認"
  df_base$alias_name <-"ie_100"
  df_table <- temp$table |> enframe()
  colnames(df_table) <- c("table.field", "table.field.value")
  df_table$table.field.value <- df_table$table.field.value |> unlist()
  res <- df_base |> merge(df_table, by=NULL) |> select(colnames(sheetIe$Cdisc_Sheet_Configs_Pivot))
  return(res)
}) 
sheetIeCdisc <- sheet$Cdisc_Sheet_Configs_Pivot
sheet$Cdisc_Sheet_Configs_Pivot <- NULL
sheet$Cdisc_Sheet_Configs <- NULL
for (i in 1:130) {
  if (!identical(jsonIeCdisc[i, ], sheetIeCdisc[i, ])) {
    break
    stop("")
  }
}
for (j in 1:6) {
  if (!identical(jsonIeCdisc[i, j], sheetIeCdisc[i, j])) {
    break
    stop("")
  }
}
jsonIeCdisc[i, j]
sheetIeCdisc[i, j]
# issue #45 option.values_codeがNAの時に空白に置き換わっている
testOptionValuesCode <- jsonIe$field_items |> map( ~ .$option) |> discard( ~ is.null(.))
testOptionValuesCode <- testOptionValuesCode |> map_df( ~ .$values)
# Options
jsonIeOptions <- json$field_items |> map(~ {
  options <- .$option
  if (is.null(options)) {
    return(NULL)
  }
  controlled_terminology_data <- options$controlled_terminology_data
  if (!is.null(controlled_terminology_data)) {
    controlled_terminology_data <- controlled_terminology_data |> map_df( ~ .) 
    tempColnames <- controlled_terminology_data |> colnames() %>% str_c("option.controlled_terminology_data.", .)
    colnames(controlled_terminology_data) <- tempColnames
    options$controlled_terminology_data <- NULL
  }
  values <- options$values
  if (!is.null(values)) {
    values <- values |> map( ~ . |> map_df( ~ .))|> bind_rows()
    tempColnames <- values |> colnames() %>% str_c("option.values_", .)
    colnames(values) <- tempColnames
    options$values <- NULL
  }
  df_base <- options |> discard( ~ is.null(.)) |> as.data.frame()
  tempColnamees <- df_base |> colnames() %>% str_c("option.", .)
  colnames(df_base) <- tempColnamees
  res <- df_base
  if (!is.null(controlled_terminology_data)) {
    res <- res |> merge(controlled_terminology_data, by=NULL)
  }
  if (!is.null(values)) {
    res <- res |> merge(values, by=NULL)
  }
  return(res)
}) |> discard( ~ is.null(.))|> bind_rows() |> distinct()
jsonIeOptions$jpname <- "選択除外基準確認"
jsonIeOptions$alias_name <-"ie_100"
jsonIeOptions$option.odm_id <- ""
#jsonIeOptions$option.parity <- ""
jsonIeOptions$option.source_id <- ""
jsonIeOptions$option.id <- jsonIeOptions$option.id |> as.character()
jsonIeOptions$option_id <- jsonIeOptions$option.id
jsonIeOptions$option.is_extensible <- jsonIeOptions$option.is_extensible |> as.character()
jsonIeOptions$option.trial_id <- jsonIeOptions$option.trial_id |> as.character()
jsonIeOptions$option.values_id <- jsonIeOptions$option.values_id |> as.character() 
jsonIeOptions$option.values_is_usable <- jsonIeOptions$option.values_is_usable |> as.character()
jsonIeOptions$option.values_option_id <- jsonIeOptions$option.values_option_id |> as.character()
jsonIeOptions$option.values_seq <- jsonIeOptions$option.values_seq |> as.character()
jsonIeOptions$option.controlled_terminology_data.cdisc_code <- jsonIeOptions$option.controlled_terminology_data.cdisc_code |> as.character()
jsonIeOptions$option.controlled_terminology_data.cdisc_code <- ifelse(is.na(jsonIeOptions$option.controlled_terminology_data.cdisc_code), "", jsonIeOptions$option.controlled_terminology_data.cdisc_code)
jsonIeOptions$option.controlled_terminology_data.cdisc_name <- ifelse(is.na(jsonIeOptions$option.controlled_terminology_data.cdisc_name), "", jsonIeOptions$option.controlled_terminology_data.cdisc_name)
jsonIeOptions$option.controlled_terminology_data.uuid <- ifelse(is.na(jsonIeOptions$option.controlled_terminology_data.uuid), "", jsonIeOptions$option.controlled_terminology_data.uuid)
jsonIeOptions$option.values_code <- ifelse(is.na(jsonIeOptions$option.values_code), "", jsonIeOptions$option.values_code)
jsonIeOptions$option.values_code <- ifelse(jsonIeOptions$option.values_code=="NA", "", jsonIeOptions$option.values_code)
sheetIeOptions <- sheetIe$Option
colnames(jsonIeOptions) |> sort()
colnames(sheetIeOptions) |> sort()
identical(colnames(jsonIeOptions) |> sort(), colnames(sheetIeOptions) |> sort())
jsonIeOptions <- jsonIeOptions |> select(colnames(jsonIeOptions) |> sort())
sheetIeOptions <- sheetIeOptions |> select(colnames(sheetIeOptions) |> sort())
identical(jsonIeOptions, sheetIeOptions)
identical(nrow(jsonIeOptions), nrow(sheetIeOptions))
identical(ncol(jsonIeOptions), ncol(sheetIeOptions))
for (i in 1:nrow(jsonIeOptions)) {
  if (!identical(jsonIeOptions[i, ], sheetIeOptions[i, ])) {
    stop()
    break
  }
}
for (j in 1:ncol(jsonIeOptions)) {
  if (!identical(jsonIeOptions[i, j], sheetIeOptions[i, j])) {
    stop()
    break
  }
}
jsonIeOptions[i, j]
sheetIeOptions[i, j]
colnames(jsonIeOptions)[j]
sheet$Option <-NULL
# flip_flops
jsonIeFlipFlops <- jsonIe$field_items |> map( ~ {
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
jsonIeFlipFlops$jpname <- "選択除外基準確認"
jsonIeFlipFlops$alias_name <-"ie_100"
sheetIeFlipFlops <- sheetIe$Flip_Flops
identical(nrow(jsonIeFlipFlops), nrow(sheetIeFlipFlops))
identical(ncol(jsonIeFlipFlops), ncol(sheetIeFlipFlops))
jsonIeFlipFlops <- jsonIeFlipFlops |> select(colnames(sheetIeFlipFlops)) |> ConvertToCharacter()
identical(jsonIeFlipFlops, sheetIeFlipFlops)
for (i in 1:nrow(jsonIeFlipFlops)) {
  if (!identical(jsonIeFlipFlops[i, ], sheetIeFlipFlops[i, ])) {
    stop()
    break
  }
}
for (j in 1:ncol(jsonIeFlipFlops)) {
  if (!identical(jsonIeFlipFlops[i, j], sheetIeFlipFlops[i, j])) {
    stop()
    break
  }
}
colnames(jsonIeFlipFlops)
colnames(sheetIeFlipFlops)
jsonIeFlipFlops[i, j]
sheetIeFlipFlops[i, j]
sheet$Flip_Flops <- NULL

# fieldItems
jsonIeFieldItems <- json$field_items |> map( ~ discard(., is.list)) |> map_df( ~ .)
jsonIeValidators <- json$field_items |> map( ~ list(id=.$id, validators=.$validators, validators.presence=T)) |> discard( ~ is.null(.$validators))
jsonIeValidators <- jsonIeValidators |> map( ~ {
  if (length(.$validators) == 0) {
    target <- .
    target$validators <- NULL
    return(as.data.frame(target))
  }
  target <- .
  df_date <- .$validators$date |> data.frame()
  df_formula <- .$validators$formula |> data.frame()
  df_presence <- .$validators$presence |> data.frame()
  res <- NULL
  if (length(df_date) > 0) {
    if (is.null(res)) {
      res <- df_date
    } else {
      res <- res |> merge(df_date, by=NULL)
    }
  }
  if (length(df_formula) > 0) {
    if (is.null(res)) {
      res <- df_formula
    } else {
      res <- res |> merge(df_formula, by=NULL)
    }
  }
  if (length(df_presence) > 0) {
    if (is.null(res)) {
      res <- df_presence
    } else {
      res <- res |> merge(df_presence, by=NULL)
    }
  }
  res$id <- target$id
  res$validators.presence <- target$validators.presence
  return(res)
}) |> bind_rows()

sheetIeFieldItems <- sheet$Field_Items
colnames(jsonIeFieldItems)
colnames(sheetIeFieldItems)
