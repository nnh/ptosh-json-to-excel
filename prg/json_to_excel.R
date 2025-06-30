#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2025.6.26
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
kEngToJpnColumnMappings <- GetEngToJpnColumnMappings()
kEngColumnNames <- kEngToJpnColumnMappings %>%
  map(names)
kTargetSheetNames <- c("item_old", "item_visit", "item", "allocation", "action", "display", "option", "comment", "explanation", "presence", "master", "visit", "title", "assigned", "limitation", "date")
# ------ main ------
temp <- ExecReadJsonFiles()
trialName <- temp$trialName
json_files <- temp$json_files
rm(temp)

field_list <- json_files %>%
  map(~ {
    json_file <- GetJsonFile(.)
    field_items <- json_file %>% GetFieldItems()
    fields <- field_items %>%
      map(~ {
        res <- tibble::tibble(
          name = .x$name,
          field_number = .x$name %>% str_extract("\\d+") %>% as.numeric(),
          label = .x$label
        )
        return(res)
      }) %>%
      bind_rows()
    fields$jpname <- json_file$name
    fields$alias_name <- json_file$alias_name
    return(fields)
  }) %>%
  bind_rows()

sheet_data_list <- json_files %>% map(~ {
  json_file <- GetJsonFile(.)
  field_items <- json_file %>% GetFieldItems()
  if (json_file$category == "visit") {
    item_visit <- EditItem(field_items, json_file$alias_name)
    item <- NULL
  } else {
    item_visit <- NULL
    item <- EditItem(field_items, json_file$alias_name)
  }
  item_old <- field_items %>%
    GetTargetByType("FieldItem::Article") %>%
    EditItem_old(json_file$alias_name)
  allocation <- json_file %>% GetAllocation()
  action <- field_items %>% GetAction(json_file$alias_name)
  display <- field_items %>% GetDisplay()
  option <- field_items %>% GetOptions()
  comment <- field_items %>% GetComment("content")
  explanation <- field_items %>% GetComment("description")
  presence <- field_items %>% GetPresence(json_file)
  master <- field_items %>% GetComment("link_type")
  visit <- field_items %>% GetVisit()
  title <- field_items %>%
    GetTargetByType("FieldItem::Heading") %>%
    EditTitle()
  assigned <- field_items %>%
    GetTargetByType("FieldItem::Assigned") %>%
    EditAssigned()
  name <- tibble(name = json_file$name, alias_name = json_file$alias_name, images_count = json_file$images_count)
  limitation <- field_items %>%
    GetLimitation() %>%
    EditLimitation()
  date <- field_items %>%
    GetDate() %>%
    EditDate(json_file$alias_name)
  res <- kTargetSheetNames %>% map(~ JoinJpnameAndAliasNameAndSelectColumns(.x, json_file))
  names(res) <- kTargetSheetNames
  res$name <- name
  return(res)
})

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
GetGroupSheetNames <- function(targetColumn) {
  return(str_remove(targetColumn, "_[0-9]+$"))
}
output_checklist <- convertSheetColumnsToJapanese(sheet_data_combine)
# item_visit、同一グループでシート情報以外がidenticalなものはまとめる
item_visit <- output_checklist$item_visit
item_visit <- item_visit %>%
  mutate(group = GetGroupSheetNames(`シート名英数字別名`)) %>%
  select(-`シート名`)
item_visit_groups <- item_visit$group %>% unique()
item_visit_by_group <- item_visit %>%
  group_by(group) %>%
  arrange(`シート名英数字別名`)
View(item_visit_by_group)
item_visit_group <- list()
unique_item_visit <- list()
unique_item_visit_count <- 0
reference_colnames <- c("条件の参照先情報", "論理式の参照先情報", "最小値の参照先情報", "最大値の参照先情報")
for (i in seq_along(item_visit_groups)) {
  item_visit_group[[i]] <- item_visit %>%
    filter(group == !!item_visit_groups[[i]])
  check_alias_name <- item_visit_group[[i]]$`シート名英数字別名` %>%
    unique()
  check_group <- list()
  for (j in seq_along(check_alias_name)) {
    check_group[[j]] <- item_visit_group[[i]] %>%
      filter(`シート名英数字別名` == !!check_alias_name[[j]])
    # 参照先情報内の自シート名をgroupに置き換える
    for (col in reference_colnames) {
      for (row in 1:nrow(check_group[[j]])) {
        if (!is.na(check_group[[j]][row, col, drop = TRUE])) {
          check_group[[j]][row, col] <- str_replace_all(check_group[[j]][row, col, drop = TRUE], check_group[[j]][row, "シート名英数字別名", drop = TRUE], item_visit_groups[[i]])
        }
      }
    }
    if (j > 1) {
      temp1 <- unique_item_visit[[unique_item_visit_count]] %>% select(-`シート名英数字別名`)
      temp2 <- check_group[[j]] %>% select(-`シート名英数字別名`)
      if (!identical(temp1, temp2)) {
        cat(str_c("グループ", item_visit_groups[[i]], "のシート", check_alias_name[[j]], "は、前のシートと異なる項目があります。\n"))
        for (k in seq_along(temp1)) {
          if (!identical(temp1[[k]], temp2[[k]])) {
            cat(str_c("項目", names(temp1)[[k]], "が異なります。\n"))
          }
        }
        unique_item_visit_count <- unique_item_visit_count + 1
        unique_item_visit[[unique_item_visit_count]] <- check_group[[j]]
      } else {
        unique_item_visit[[unique_item_visit_count]][["シート名英数字別名"]] <- str_c(unique_item_visit[[unique_item_visit_count]][["シート名英数字別名"]], ", ", check_group[[j]][["シート名英数字別名"]])
      }
    } else {
      unique_item_visit_count <- unique_item_visit_count + 1
      unique_item_visit[[unique_item_visit_count]] <- check_group[[j]]
    }
  }
}
item_visit_tibble <- unique_item_visit %>% bind_rows()

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
