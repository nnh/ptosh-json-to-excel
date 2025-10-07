#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2025.10.7
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
kTargetSheetNames <- c(kItemVisit, kItemVisit_old, "item", "allocation", "action", "display", "option", "comment", "explanation", "presence", "master", "visit", "title", "assigned", "limitation", "date")
# ------ main ------
temp <- ExecReadJsonFiles()
trialName <- temp[["trialName"]]
json_files <- temp[["json_files"]]
options_flag <- kOptions %in% names(json_files)
if (options_flag) {
  options_json <- GetListSetName(json_files, kOptions, "name")
  json_files <- json_files[names(json_files) != kOptions]
}
rm(temp)
sheets <- GetListSetName(json_files, "sheets", "alias_name")

field_list <- sheets %>%
  map(~ {
    json_file <- .
    field_items <- json_file %>% GetFieldItems()
    fields <- field_items %>%
      map(~ {
        res <- tibble::tibble(
          name = .x[["name"]],
          field_number = .x[["name"]] %>% str_extract("\\d+") %>% as.numeric(),
          label = .x[["label"]]
        )
        return(res)
      }) %>%
      bind_rows()
    fields[["jpname"]] <- json_file[["name"]]
    fields[["alias_name"]] <- json_file[["alias_name"]]
    return(fields)
  }) %>%
  bind_rows()
# VISIT対応シートかどうか判定する
visits <- json_files[["visits"]]
is_visit <- !is.null(visits) && length(visits) > 0
if (is_visit) {
  visits_name_num <- tibble::tibble(
    name = map_chr(visits, "name"),
    num = map_chr(visits, "num")
  )
  temp_visit_groups <- GetListSetName(json_files, "visit_groups", "alias_name") %>%
    map_dfr(function(group) {
      map_dfr(group$visit_sheets, function(sheet) {
        tibble(
          name = group$name,
          visit = group$alias_name,
          alias_name = sheet$sheet_alias_name,
          visitnum = sheet$visit_num %>% as.numeric()
        )
      })
    })
  visit_groups <- temp_visit_groups # %>%
  #    left_join(visits_name_num, by = c("visitnum" = "num"))
  visit_json_files_group <- EditGroupVisit(sheets)
} else {
  visit_json_files_group <- json_files
  visits_name_num <- NULL
  visit_groups <- NULL
}
GetIsInVisitGroup <- function(sheet_alias_name) {
  if (is_visit) {
    # sheet[["alias_name"]]がvisit_groups[["sheet_alias_name"]]に存在するか判定
    is_in_visit_group <- CheckExistenceOfVisitGroup(sheet_alias_name, visit_groups)
  } else {
    is_in_visit_group <- FALSE
  }
  return(is_in_visit_group)
}
# VISITまとめ対象外のシート
# name
# item
# allocation
# display
# master
# visit
sheet_data_list_no_group <- sheets %>% map(~ {
  sheet <- .
  field_items <- sheet %>% GetFieldItems()
  is_in_visit_group <- GetIsInVisitGroup(sheet[["alias_name"]])
  if (is_in_visit_group) {
    item <- NULL
  } else {
    item <- EditItem(field_items, sheet[["alias_name"]])
  }
  allocation <- sheet %>% GetAllocation()
  display <- field_items %>% GetDisplay()
  master <- field_items %>% GetComment("link_type")
  if (!is_visit) {
    visit <- field_items %>% GetVisit()
  } else {
    visit <- NULL
  }
  name <- tibble(name = sheet[["name"]], alias_name = sheet[["alias_name"]], images_count = sheet[["images_count"]])
  item <- JoinJpnameAndAliasNameAndSelectColumns("item", sheet)
  allocation <- JoinJpnameAndAliasNameAndSelectColumns("allocation", sheet)
  display <- JoinJpnameAndAliasNameAndSelectColumns("display", sheet)
  master <- JoinJpnameAndAliasNameAndSelectColumns("master", sheet)
  visit <- JoinJpnameAndAliasNameAndSelectColumns("visit", sheet)
  res <- list(
    name = name,
    item = item,
    allocation = allocation,
    display = display,
    master = master,
    visit = visit,
    item_visit = "item_visit"
  )
})
# VISITまとめ対象のシート
sheet_data_list_group <- map2(sheets, names(sheets), ~ {
  sheet <- .x
  sheet_name <- .y
  field_items <- sheet %>% GetFieldItems()
  is_in_visit_group <- GetIsInVisitGroup(sheet[["alias_name"]])
  if (is_in_visit_group) {
    item_visit_old <- EditItem(field_items, sheet_name)
  } else {
    item_visit_old <- NULL
  }
  action <- field_items %>% GetAction(sheet_name)
  option <- field_items %>% GetOptions()
  comment <- field_items %>% GetComment("content")
  explanation <- field_items %>% GetComment("description")
  presence <- field_items %>% GetPresence(sheet)
  title <- field_items %>%
    GetTargetByType("FieldItem::Heading") %>%
    EditTitle()
  assigned <- field_items %>%
    GetTargetByType("FieldItem::Assigned") %>%
    EditAssigned()
  limitation <- field_items %>%
    GetLimitation() %>%
    EditLimitation()
  date <- field_items %>%
    GetDate() %>%
    EditDate(sheet[["alias_name"]])
  item_visit_old <- JoinJpnameAndAliasNameAndSelectColumns("item_visit_old", sheet)
  action <- JoinJpnameAndAliasNameAndSelectColumns("action", sheet)
  option <- JoinJpnameAndAliasNameAndSelectColumns("option", sheet)
  comment <- JoinJpnameAndAliasNameAndSelectColumns("comment", sheet)
  explanation <- JoinJpnameAndAliasNameAndSelectColumns("explanation", sheet)
  presence <- JoinJpnameAndAliasNameAndSelectColumns("presence", sheet)
  title <- JoinJpnameAndAliasNameAndSelectColumns("title", sheet)
  assigned <- JoinJpnameAndAliasNameAndSelectColumns("assigned", sheet)
  limitation <- JoinJpnameAndAliasNameAndSelectColumns("limitation", sheet)
  date <- JoinJpnameAndAliasNameAndSelectColumns("date", sheet)
  return(list(
    item_visit_old = item_visit_old,
    action = action,
    option = option,
    comment = comment,
    explanation = explanation,
    presence = presence,
    title = title,
    assigned = assigned,
    limitation = limitation,
    date = date,
    sheet_name = sheet_name,
    sheet = sheet
  ))
})

all_names <- names(sheet_data_list_no_group)
sheet_data_list_group_fixed <- set_names(
  map(all_names, ~ sheet_data_list_group[[.x]] %||% NULL),
  all_names
)
sheet_data_list <- map2(sheet_data_list_no_group, sheet_data_list_group_fixed, ~ c(.x, .y))

targetSheetNames <- kTargetSheetNames %>% append("name", .)
sheet_data_combine <- targetSheetNames %>%
  map(~ map(sheet_data_list, pluck, .x) %>%
    compact() %>%
    bind_rows()) %>%
  set_names(targetSheetNames)
# 0行0列のデータフレームを補完
for (nm in names(sheet_data_combine)) {
  df <- sheet_data_combine[[nm]]
  if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
    if (!is.null(kEngColumnNames[[nm]])) {
      sheet_data_combine[[nm]] <- data.frame(matrix(ncol = length(kEngColumnNames[[nm]]), nrow = 0)) %>%
        setNames(kEngColumnNames[[nm]])
    }
  }
}

# VISIT対応シート
if (is_visit) {
  sheet_data_combine[[kVisit]] <- CreateVisitToVisitSheetData()
}
# 日本語列名に変換する
output_checklist <- convertSheetColumnsToJapanese(sheet_data_combine)
# item_visit、同一グループでシート情報以外がidenticalなものはまとめる
output_checklist[[kItemVisit]] <- EditItemVisit(output_checklist[[kItemVisit_old]])
item_visit_old <- EditItemVisitOld(output_checklist[[kItemVisit_old]])
output_checklist[[kItemVisit_old]] <- item_visit_old

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
OutputChecklistXlsx(output_checklist, output_checklist_path)
cat("処理が終了しました。")
