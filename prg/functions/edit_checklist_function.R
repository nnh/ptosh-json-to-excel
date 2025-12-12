#' Functions for checklist output.
#'
#' @file edit_checklist_function.R
#' @author Mariko Ohtsuka
#' @date 2025.12.11
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
  if (sheet_name == kItemVisit_old || sheet_name == kItemVisit) {
    setColumnConditionalFormatting(
      wb = wb, sheetName = sheet_name,
      targetColName = kItemVisitConditionalFormattingColumnName,
      rows = 2:(nrow(df_output) + 1)
    )
  }

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
  df[["jpname"]] <- json_file[["name"]]
  df[["alias_name"]] <- json_file[["alias_name"]]
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
GetFieldItems <- function(json_file) {
  return(json_file[["field_items"]])
}
CheckExistenceOfVisitGroup <- function(sheet_alias_name, visit_groups) {
  if (is.null(visit_groups)) {
    return(FALSE)
  }
  return(sheet_alias_name %in% visit_groups[["alias_name"]])
}

EditGroupVisit <- function(sheets) {
  visit_json_files <- sheets %>%
    keep(~ CheckExistenceOfVisitGroup(.x[["alias_name"]], visit_groups))
  no_visit_json_files <- sheets %>%
    discard(~ CheckExistenceOfVisitGroup(.x[["alias_name"]], visit_groups))
  # グループ名ごとに最小の数値を持つ要素だけを残す
  visit_names <- names(visit_json_files)
  group_info <- tibble::tibble(
    name = visit_names,
    group = str_replace(visit_names, "_\\d+$", ""),
    num = as.integer(str_extract(visit_names, "\\d+$"))
  )
  min_num_per_group <- group_info %>%
    group_by(group) %>%
    filter(num == min(num)) %>%
    ungroup()
  visit_json_files_group_group <- visit_json_files[min_num_per_group$name]
  # visit_json_files_group_groupとno_visit_json_filesを結合
  visit_json_files_group <- c(visit_json_files_group_group, no_visit_json_files)
  return(visit_json_files_group)
}


source(here("prg", "functions", "edit_common.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item_visit_old.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item_visit.R"), encoding = "UTF-8")
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
source(here("prg", "functions", "edit_date.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_convert_column_name.R"),
  encoding = "UTF-8"
)
source(here("prg", "functions", "set_items_sheet_settings.R"), encoding = "UTF-8")
source(here("prg", "functions", "summarize_by_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "replace_ref_text.R"), encoding = "UTF-8")
