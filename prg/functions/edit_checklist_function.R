#' Functions for checklist output.
#'
#' @file edit_checklist_function.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
OutputChecklistSheet <- function(df_output, wb, sheet_name) {
  output_colnames <- df_output %>% colnames()
  addWorksheet(wb = wb, sheet = sheet_name)
    if (sheet_name == .const[["kSheetGroupsVisit"]] || sheet_name == .const[["kSheetGroupsNonVisit"]]) {
    writeDataTable(
      wb = wb, sheet = sheet_name, x = df_output,
      startRow = 1, startCol = 1, colNames = T, rowNames = F, withFilter = F,
      tableStyle = kTableStyle, keepNA = F
    )
  } else {
    writeDataTable(
      wb = wb, sheet = sheet_name, x = df_output,
      startRow = 1, startCol = 1, colNames = T, rowNames = F, withFilter = T,
      tableStyle = kTableStyle, keepNA = F
    )
  }
  setColWidths(wb = wb, sheet = sheet_name, cols = 1:ncol(df_output), widths = "auto")
  fontStyle <- setFontStyle()
  addStyle(wb = wb, sheet = sheet_name, style = fontStyle, rows = 1:(nrow(df_output) + 1), cols = 1:ncol(df_output), gridExpand = TRUE)
  if (sheet_name == .const[["kItemVisit_old"]] || sheet_name == .const[["kItemVisit"]]) {
    setColumnConditionalFormatting(
      wb = wb, sheetName = sheet_name,
      targetColName = .const[["kItemVisitConditionalFormattingColumnName"]],
      rows = 2:(nrow(df_output) + 1)
    )
  }
  # sheet_groupsの場合、一行目を非表示にして二行目、三行目をヘッダーとして扱う
  if (sheet_name == .const[["kSheetGroupsVisit"]] || sheet_name == .const[["kSheetGroupsNonVisit"]]) {
    # 一行目を非表示にする
    setRowHeights(wb = wb, sheet = sheet_name, rows = 1, heights = 0)
    # 二行目、三行目をヘッダーとして扱う
    headerStyle <- createStyle(
      fontName = "Meiryo", 
      fontSize = 11, 
      fontColour = "#FFFFFF", 
      halign = "left",            
      textDecoration = "bold",    
      fgFill = "#4F81BD", 
      border = "TopBottomLeftRight", 
      borderColour = "#000000"
    )
    addStyle(wb = wb, sheet = sheet_name, style = headerStyle, rows = 2:3, cols = 1:ncol(df_output), gridExpand = TRUE)
  }
  return(wb)
}
OutputChecklistXlsx <- function(output_list, output_checklist_path) {
  wb <- createWorkbook()
  for (i in seq_along(output_list)) {
    wb <- OutputChecklistSheet(df_output = output_list[[i]], wb = wb, sheet_name = names(output_list)[i])
  }
  saveWorkbook(wb = wb, file = str_c(output_checklist_path, "/", kOutputChecklistName), overwrite = T)
}
JoinJpnameAndAliasName <- function(df, json_file) {
  df[[.const[["kOutputJapanaseNameEnglish"]]]] <- json_file[[.const[["kSheetJapaneseName"]]]]
  df[[.const[["kAliasName"]]]] <- json_file[[.const[["kAliasName"]]]]
  return(df)
}
SelectColumns <- function(df, target_columns) {
  df <- df %>%
    select(all_of(target_columns))
  return(df)
}
JoinJpnameAndAliasNameAndSelectColumns <- function(df, df_name, json_file) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  df <- JoinJpnameAndAliasName(df, json_file)
  df <- SelectColumns(df, .const[["kEngColumnNames"]][[df_name]])
  return(df)
}
GetFieldItems <- function(json_file) {
  return(json_file[[.const[["kFieldItems"]]]])
}
CheckExistenceOfVisitGroup <- function(sheet_alias_name, visit_groups) {
  if (is.null(visit_groups)) {
    return(FALSE)
  }
  return(sheet_alias_name %in% visit_groups[[.const[["kAliasName"]]]])
}
# ------ load other function files ------
source(here("prg", "functions", "edit_common.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_allocation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_option.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_master.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_assigned.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_limitation.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_date.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_convert_column_name.R"),
  encoding = "UTF-8"
)
# edit_checklist_convert_column_name.R で GetEngToJpnColumnMappings() が定義された後に初期化する
.const[["kEngToJpnColumnMappings"]] <- GetEngToJpnColumnMappings()
.const[["kEngColumnNames"]] <- .const[["kEngToJpnColumnMappings"]] %>% map(names)
source(here("prg", "functions", "set_items_sheet_settings.R"), encoding = "UTF-8")
source(here("prg", "functions", "summarize_by_visit.R"), encoding = "UTF-8")
source(here("prg", "functions", "replace_ref_text.R"), encoding = "UTF-8")
source(here("prg", "functions", "sort_sheets.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_sheet_groups.R"), encoding = "UTF-8")
source(here("prg", "functions", "build_sheet_data.R"), encoding = "UTF-8")
