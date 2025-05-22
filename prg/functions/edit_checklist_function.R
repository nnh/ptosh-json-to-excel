#' Functions for checklist output.
#'
#' @file edit_checklist_function.R
#' @author Mariko Ohtsuka
#' @date 2025.5.22
# ------ constants ------
# ------ functions ------
OutputChecklistSheet <- function(df_output, wb, sheet_name) {
  output_colnames <- df_output %>% colnames()
  addWorksheet(wb = wb, sheet = sheet_name)
  writeDataTable(
    wb = wb, sheet = sheet_name, x = df_output,
    startRow = 1, startCol = 1, colNames = T, rowNames = F, withFilter = T,
    tableStyle = kTableStyle, keepNA = F
  )
  setColWidths(wb = wb, sheet = sheet_name, cols = 1:ncol(df_output), widths = "auto")
  fontStyle <- setFontStyle()
  addStyle(wb = wb, sheet = sheet_name, style = fontStyle, rows = 1:(nrow(df_output) + 1), cols = 1:ncol(df_output), gridExpand = TRUE)

  return(wb)
}
OutputChecklistXlsx <- function(output_list, output_checklist_path) {
  wb <- createWorkbook()
  for (i in 1:length(output_list)) {
    wb <- OutputChecklistSheet(df_output = output_list[[i]], wb = wb, sheet_name = names(output_list)[i])
  }
  saveWorkbook(wb = wb, file = str_c(output_checklist_path, "/", kOutputChecklistName), overwrite = T)
}
CreatedummyDf <- function(target_columns) {
  df <- data.frame(matrix(ncol = length(target_columns), nrow = 0))
  colnames(df) <- target_columns
  return(df)
}
JoinJpnameAndAliasName <- function(df, json_file) {
  df$jpname <- json_file$name
  df$alias_name <- json_file$alias_name
  return(df)
}
SelectColumns <- function(df, target_columns) {
  df <- df %>%
    select(all_of(target_columns))
  return(df)
}
JoinJpnameAndAliasNameAndSelectColumns <- function(df_name, json_file) {
  df <- get(df_name, envir = parent.frame())
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  df <- JoinJpnameAndAliasName(df, json_file)
  df <- SelectColumns(df, kEngColumnNames[[df_name]])
  return(df)
}
GetJsonFile <- function(json_file) {
  json_file <- json_file$rawJson
  return(json_file)
}
GetFieldItems <- function(json_file) {
  return(json_file$field_items)
}

source(here("prg", "functions", "edit_common.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_allocation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_action.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_display.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_option.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_comment.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_presence.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_title.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_assigned.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_limitation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_convert_column_name.R"), encoding = "UTF-8")
