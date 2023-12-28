#' title
#' description
#' @file edit_checklist_function.R
#' @author Mariko Ohtsuka
#' @date 2023.12.28
# ------ constants ------
# ------ functions ------
EditCheckList <- function(){
  json_filenames <- list.files(here(kInputFolderName), pattern="*.json", full.names=F)
  if (length(json_filenames) == 0){
    stop("No JSON files found.")
    return(NULL)
  }
  json_files <- ReadJsonFiles(json_filenames, kInputFolderName)
  return(json_files)
}
ReplaceText <- function(x){
  if (is.null(x)){
    return(NA)
  }
  return(x)
}
GetTargetColumns <- function(){
  res <- list()
  res$name <- c("jpname", "alias_name", "stylesheet", "fax_stylesheet", "javascript", "category", "images_count", "lab_department_id")
  res$item <- c("name", "label", "option.name", "default_value", "validators.presence.validate_presence_if", "presence_if_references", "validators.formula.validate_formula_if", "formula_if_references", "validators.formula.validate_formula_message", "validators.date.validate_date_after_or_equal_to", "references_after", "validators.date.validate_date_before_or_equal_to", "references_before")
  res$option <- c("option.name", "option.id")
  res$visit <- c("jpname", "alias_name", "name", "default_value")
  res$number <- c("jpname", "alias_name", "name", "label", "default_value", "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to")
  res$master <- c("jpname", "alias_name", "label", "link_type")
  res$alert <- c("jpname", "alias_name", "name", "label", "normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to")
  return(res)
}
GetDfOptions <- function(json_files){
  df_option <- json_files %>% map_df( ~ {
    field_items <<- .$flattenJson$field_items
    if (!is.data.frame(field_items)){
      return(NULL)
    }
    df_option <- field_items %>% select(any_of(c("sheet_id", "option.id", "option.name")))
    if (length(df_option) == 1){
      return(NULL)
    }
    option_values <- field_items$option.values %>% list_rbind() %>% distinct() %>%
      rename_with(~ paste0("option.values_", .x), everything())
    df_option <- df_option %>% filter(!is.na(option.id)) %>% distinct() %>%
      inner_join(option_values, by=c("option.id"="option.values_option_id"))
    return(df_option)
  })
  return(df_option)
}
GetDfFieldItems <- function(json_files){
  res <- json_files %>% map_df( ~ .$flattenJson$field_items)
  return(res)
}
GetDfSheetItems <- function(json_files){
  res <- json_files %>% map_df( ~ .$flattenJson %>% RemoveNestedLists() %>% map( ~ ReplaceText(.))) %>%
    rename(jpname=name)
  return(res)
}
GetDfCdiscSheetConfig <- function(json_files){
  res <- json_files %>% map_df( ~ .$flattenJson$cdisc_sheet_config)
  return(res)
}
OutputChecklistSheet <- function(df_output, wb, sheet_name){
  output_colnames <- df_output %>% colnames()
  wordwrap_colnames <- c("stylesheet", "fax_stylesheet")
  wordwrap_colnames_index <- which(output_colnames %in% wordwrap_colnames)
  addWorksheet(wb=wb, sheet=sheet_name)
  writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                 startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                 tableStyle=kTableStyle, keepNA=F)
  if (length(wordwrap_colnames_index) > 0){
    wordwrap_colnames_index %>% map( ~ addStyle(wb=wb, sheet=sheet_name,
                                                style=createStyle(wrapText=T), rows=1:nrow(df_output), cols=.))
  }
  setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
  return(wb)
}
OutputChecklistXlsx <- function(output_list){
  wb <- createWorkbook()
  for (i in 1:length(output_list)){
    wb <- OutputChecklistSheet(df_output=output_list[[i]], wb=wb, sheet_name=names(output_list)[i])
  }
  saveWorkbook(wb=wb, file="/Users/mariko/Downloads/test.xlsx", overwrite=T)
}
RemoveNestedLists <- function(input_list){
  # リスト内の要素がリストでない場合に抽出
  filtered_list <- input_list %>% purrr::discard( ~ is.list(.))
  return(filtered_list)
}
JoinJpnameAndAliasName <- function(target, sheet_items){
  res <- target %>% inner_join(sheet_items %>% select("jpname", "alias_name", "id"), ., by=c("id"="sheet_id"))
  return(res)
}
EditOutputColumns <- function(df_target, target_column){
  res <- df_target %>%
    JoinJpnameAndAliasName(input_list$df_sheet_items) %>%
    select(any_of(target_column))
  return(res)
}
