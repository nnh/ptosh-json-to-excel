#' Edit the contents of the JSON file and output to Excel.
#'
#' @file json_to_excel.R
#' @author Mariko Ohtsuka
#' @date 2025.5.19
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
source(here("prg", "functions", "edit_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "io_functions.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_function.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_item.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_action.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_display.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_option.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_comment.R"), encoding = "UTF-8")
source(here("prg", "functions", "edit_checklist_convert_column_name.R"), encoding = "UTF-8")

# ------ constants ------
kInputFolderName <- "input"
kOutputFolderName <- "output"
kOutputPath <- here(kOutputFolderName)
kAlertTargetColnames <- c("normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to")
kEngColumnNames <- kEngToJpnColumnMappings %>%
  map(names)
# ------ main ------
temp <- ExecReadJsonFiles()
trialName <- temp$trialName
json_files <- temp$json_files
rm(temp)
GetAllocation <- function(json_file) {
  allocation <- json_file$allocation
  allocation <- allocation %>% map(~ {
    if (is.null(.x)) {
      return(NA)
    } else {
      return(.x)
    }
  })
  return(allocation)
}
GetExplanation <- function(json_file) {
  target <- json_file$field_items %>%
    map(~ {
      if (is.null(.x$description) || .x$description == "") {
        return(NULL)
      }
      return(tibble::tibble(
        name = .x$name %||% NA,
        label = .x$label %||% NA,
        description = .x$description
          %||% NA
      ))
    }) %>%
    bind_rows()
  if (nrow(target) == 0) {
    return(NULL)
  }
  target$jpname <- json_file$name
  target$alias_name <- json_file$alias_name
  res <- target %>%
    select(kEngColumnNames$explanation) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
  return(res)
}

CreatedummyDf <- function(target_columns) {
  df <- data.frame(matrix(ncol = length(target_columns), nrow = 0))
  colnames(df) <- target_columns
  return(df)
}


test <- json_files %>% map(~ {
  json_file <- .$rawJson
  item <- json_file %>% GetItem()
  allocation <- json_file %>% GetAllocation()
  action <- json_file %>% GetAction()
  display <- json_file %>% GetDisplay()
  options <- json_file %>% GetOptions()
  comment <- json_file %>% GetComment()
  explanation <- json_file %>% GetExplanation()
})
for (i in seq_along(json_files)) {
  json_file <- json_files[[i]]$rawJson
  item_list <- json_file %>% GetItem()
}


# input_list <- EditInputDataList(json_files)
# target_columns <- GetTargetColumns(input_list)
# df_reference <- GetSheetnameAndFieldForReference(json_files)
# temp_output_checklist <- EditOutputDataList(input_list)
# output_checklist <- convertSheetColumnsToJapanese(temp_output_checklist)
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
