#' title
#' テスト用インプットファイルでのプログラム実行
#' @file copy_folders_and_run_program.R
#' @author Mariko Ohtsuka
#' @date 2025.7.30
rm(list = ls())
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
# ------ constants ------
# ------ functions ------
# ------ main ------
targetPath <- here() |>
  list.dirs(recursive = FALSE, full.names = TRUE) |>
  str_extract("^.*/forTest_input_.+$") |>
  na.omit()
if (length(targetPath) != 11) {
  stop("テストファイルが増えてるので↓のソースを修正")
}
targetPath |> write.table(here("temp", "targetPath"), sep = ",", col.names = F, row.names = F)
# ****************************************
# ファイル1
# ****************************************
# ****************************************
# テストファイルが増えたらここからコピー
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 1 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# テストファイルが増えたらここまでコピー
# ****************************************
# ****************************************
# ファイル2
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 2 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル3
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 3 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル4
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 4 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル5
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 5 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル6
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 6 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル7
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 7 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル8
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 8 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル9
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 9 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル10
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 10 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)
# ****************************************
# ファイル11
# ****************************************
source(here("tools", "copy_folders_and_run_program", "functions", "copy_folders_and_run_functions.R"), encoding = "UTF-8")
targetRow <- 11 # <- ここの番号を変える
copyFoldersAndRunProgramMain(targetRow)


# テストファイルが増えたら↑にペースト
# 終了処理
inputDirPath <- here() |>
  list.dirs() |>
  str_extract("^.*/input$") |>
  na.omit()
fs::dir_delete(inputDirPath)
dir.create(inputDirPath)
dummyPath <- here() |>
  list.dirs() |>
  str_extract("^.*/input_dummy$") |>
  na.omit()
dummyfile <- dummyPath |> list.files(full.names = T, all.files = T, no.. = T)
file.copy(dummyfile, inputDirPath)
