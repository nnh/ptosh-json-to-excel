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
  df_base <- options |> discard( ~ is.null(.))
  return(df_base)
})
sheetIeOptions <- sheetIe$Option
# fieldItems
jsonIeFieldItems <- json$field_items |> map(~ discard(., is.list)) |> map_df( ~ .)
sheetIeFieldItems <- sheet$Field_Items
