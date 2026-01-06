#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2025.12.19
rm(list = ls())
# ------ functions ------
#' Install and Load R Package
#'
#' This function installs and loads an R package if it is not already installed.
#'
#' @param package_name The name of the R package to be installed and loaded.
#'
#' @details
#' If the specified package is not already installed, this function installs it
#' using install.packages(). After installation, the function loads the package
#' into the R session using library().
#'
#' @examples
#' InstallAndLoadPackage("tidyverse")
#'
#' @export
InstallAndLoadPackage <- function(package_name) {
  if (!requireNamespace(package_name, quietly = T)) {
    install.packages(package_name, dependencies = T, type = "binary")
  }
  library(package_name, character.only = T, warn.conflicts = F)
}
# ------ libraries ------
InstallAndLoadPackage("tidyverse")
InstallAndLoadPackage("here")
InstallAndLoadPackage("jsonlite")
InstallAndLoadPackage("openxlsx")
InstallAndLoadPackage("rlang")
source(here("prg", "functions", "common_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_function.R"), encoding = "UTF-8")
# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputPath <- here(kOutputFolderName)
kAliasNameJapaneseColumnName <- "シート名英数字別名"
kItemVisitConditionalFormattingColumnName <- "数値チェック・アラート条件の有無"
kReferenceColnames <- c("条件の参照先情報", "論理式の参照先情報", "最小値の参照先情報", "最大値の参照先情報")
kEngToJpnColumnMappings <- GetEngToJpnColumnMappings()
kEngColumnNames <- kEngToJpnColumnMappings %>%
  map(names)
kOptions <- "options"
kItemVisit <- "item_visit"
kItemVisit_old <- "item_visit_old"
kVisit <- "visit"
kVisits <- "visits"
kTargetSheetNames <- c(kItemVisit, kItemVisit_old, "item_nonvisit", "visit", "allocation", "limitation", "date", "option", "master", "assigned")
kSortOrderSheetNames <- c(kItemVisit, "item_nonvisit", "visit", "allocation", "limitation", "date", "option", "name", "master", "assigned")
# ------ main ------
temp <- ExecReadJsonFiles()
for (name in names(temp)) {
  assign(name, temp[[name]])
}
rm(temp)

field_list <- GetFieldList(sheets)

sheet_data_list_group <- sheets %>% map(~ {
  sheet <- .x
  sheet_name <- sheet[["alias_name"]]
  field_items <- sheet %>% GetFieldItems()
  temp <- EditItemAndItemVisit(field_items, sheet_name)
  item_nonvisit <- temp$item
  item_visit_old <- temp$item_visit
  allocation <- sheet %>% GetAllocation()
  master <- field_items %>% GetComment("link_type", sheet)
  if (!is_visit) {
    visit <- field_items %>% GetVisit(sheet)
  } else {
    visit <- NULL
  }
  name <- tibble(name = sheet[["name"]], alias_name = sheet_name, images_count = sheet[["images_count"]])
  option <- field_items %>% GetOptions(sheet)
  assigned <- field_items %>% EditAssigned(sheet)
  limitation <- field_items %>% EditLimitation(sheet)
  date <- field_items %>% EditDate(sheet)
  item_nonvisit <- JoinJpnameAndAliasNameAndSelectColumns("item_nonvisit", sheet)
  item_visit_old <- JoinJpnameAndAliasNameAndSelectColumns("item_visit_old", sheet)
  return(list(
    name = name,
    item_nonvisit = item_nonvisit,
    allocation = allocation,
    master = master,
    visit = visit,
    item_visit_old = item_visit_old,
    option = option,
    assigned = assigned,
    limitation = limitation,
    date = date
  ))
})
# シートデータを結合し、空データを補完する
sheet_data_combine <- CombineSheetSafety(sheet_data_list_group)
# VISIT情報を集約する
summary_sheet_data <- SummarizeByVisit(sheet_data_combine)
# VISIT対応シートを使用している試験のVISIT情報を格納する
if (is_visit) {
  summary_sheet_data[[kVisit]] <- GetVisitIsVisit()
}
# 日本語列名に変換する
output_checklist <- convertSheetColumnsToJapanese(summary_sheet_data)
# item_visit、同一グループでシート情報以外がidenticalなものはまとめる
output_checklist[[kItemVisit]] <- EditItemVisit(output_checklist[[kItemVisit_old]])
# remove item_visit_old sheet
output_checklist[[kItemVisit_old]] <- NULL
# シート出力順、各シートの行順の変更
sort_output_checklist <- SortSheetsMain(output_checklist)

# create output folder.
output_folder_name <- Sys.time() %>%
  format("%Y%m%d%H%M%S") %>%
  str_c("output_", .)
output_folder_path <- CreateOutputFolder(output_folder_name, kOutputPath)
output_file_ymd <- Sys.time() %>%
  format("%Y%m%d")
kOutputChecklistName <- str_c(trialName, " eCRF Spec ", output_file_ymd, ".xlsx")
output_checklist_path <- CreateOutputFolder("list", output_folder_path)
cat(str_c("フォルダ", output_checklist_path, "を作成しました\n"))
OutputChecklistXlsx(sort_output_checklist, output_checklist_path)
cat("処理が終了しました。")
