#' title
#' テスト用インプットファイルでのプログラム実行
#' @file copy_folders_and_run_program.R
#' @author Mariko Ohtsuka
#' @date 2024.8.29
rm(list=ls())
# ------ libraries ------
library(tidyverse, warn.conflicts=F)
library(here, warn.conflicts=F)
# ------ constants ------
# ------ functions ------
# ------ main ------
targetPath <- here() |> list.dirs() |> str_extract("^.*/input_.+$") |> na.omit()
dummyPath <- here() |> list.dirs() |> str_extract("^.*/input_dummy$") |> na.omit()
targetPath <- targetPath %>% ifelse(. == dummyPath, NA, .) |> na.omit()
if (length(targetPath) != 5) {
  stop("テストファイルが増えてるので↓のソースを修正")
}
targetPath |> write.table(here("temp", "targetPath"), sep=",", col.names=F, row.names=F)
# ファイル1
# テストファイルが増えたらここからコピー
# ****************************************
targetRow <- 1 # <- ここの番号を変える
temp <- read.table(here("temp", "targetPath"), sep=",")
targetPath <- temp[targetRow, 1]
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
if (length(inputDirPath) == 1) {
  fs::dir_delete(inputDirPath)
}
dir.create(inputDirPath)
targetTrialName <- basename(targetPath) |> str_remove("input")
targetTrialName |> write.table(here("temp", "targetTrialName"), sep=",", col.names=F, row.names=F)
targetFiles <- targetPath |> list.files(full.names=T)
outputDirBef <- here("output") |> list.dirs(recursive=F)
outputDirBef |> write.table(here("temp", "outputDirBef"), sep=",", col.names=F, row.names=F)
for (i in 1:length(targetFiles)) {
  file.copy(targetFiles[i], inputDirPath)
}
source(here("prg", "json_to_excel.R"), encoding="UTF-8")
outputDirBef <- read.table(here("temp", "outputDirBef"), sep=",") %>% .[, 1, drop=T]
outputDirAft <- here("output") |> list.dirs(recursive=F)
newDirPath <- setdiff(outputDirAft, outputDirBef)
newDir <- newDirPath |> dirname()
targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
file.rename(newDirPath, file.path(newDir, newDirRename))
# ****************************************
# テストファイルが増えたらここまでコピー
# ファイル2
# ****************************************
targetRow <- 2 # <- ここの番号を変える
temp <- read.table(here("temp", "targetPath"), sep=",")
targetPath <- temp[targetRow, 1]
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
if (length(inputDirPath) == 1) {
  fs::dir_delete(inputDirPath)
}
dir.create(inputDirPath)
targetTrialName <- basename(targetPath) |> str_remove("input")
targetTrialName |> write.table(here("temp", "targetTrialName"), sep=",", col.names=F, row.names=F)
targetFiles <- targetPath |> list.files(full.names=T)
outputDirBef <- here("output") |> list.dirs(recursive=F)
outputDirBef |> write.table(here("temp", "outputDirBef"), sep=",", col.names=F, row.names=F)
for (i in 1:length(targetFiles)) {
  file.copy(targetFiles[i], inputDirPath)
}
source(here("prg", "json_to_excel.R"), encoding="UTF-8")
outputDirBef <- read.table(here("temp", "outputDirBef"), sep=",") %>% .[, 1, drop=T]
outputDirAft <- here("output") |> list.dirs(recursive=F)
newDirPath <- setdiff(outputDirAft, outputDirBef)
newDir <- newDirPath |> dirname()
targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
file.rename(newDirPath, file.path(newDir, newDirRename))
# ****************************************
# ファイル3
# ****************************************
targetRow <- 3 # <- ここの番号を変える
temp <- read.table(here("temp", "targetPath"), sep=",")
targetPath <- temp[targetRow, 1]
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
if (length(inputDirPath) == 1) {
  fs::dir_delete(inputDirPath)
}
dir.create(inputDirPath)
targetTrialName <- basename(targetPath) |> str_remove("input")
targetTrialName |> write.table(here("temp", "targetTrialName"), sep=",", col.names=F, row.names=F)
targetFiles <- targetPath |> list.files(full.names=T)
outputDirBef <- here("output") |> list.dirs(recursive=F)
outputDirBef |> write.table(here("temp", "outputDirBef"), sep=",", col.names=F, row.names=F)
for (i in 1:length(targetFiles)) {
  file.copy(targetFiles[i], inputDirPath)
}
source(here("prg", "json_to_excel.R"), encoding="UTF-8")
outputDirBef <- read.table(here("temp", "outputDirBef"), sep=",") %>% .[, 1, drop=T]
outputDirAft <- here("output") |> list.dirs(recursive=F)
newDirPath <- setdiff(outputDirAft, outputDirBef)
newDir <- newDirPath |> dirname()
targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
file.rename(newDirPath, file.path(newDir, newDirRename))
# ****************************************
# ファイル4
# ****************************************
targetRow <- 4 # <- ここの番号を変える
temp <- read.table(here("temp", "targetPath"), sep=",")
targetPath <- temp[targetRow, 1]
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
if (length(inputDirPath) == 1) {
  fs::dir_delete(inputDirPath)
}
dir.create(inputDirPath)
targetTrialName <- basename(targetPath) |> str_remove("input")
targetTrialName |> write.table(here("temp", "targetTrialName"), sep=",", col.names=F, row.names=F)
targetFiles <- targetPath |> list.files(full.names=T)
outputDirBef <- here("output") |> list.dirs(recursive=F)
outputDirBef |> write.table(here("temp", "outputDirBef"), sep=",", col.names=F, row.names=F)
for (i in 1:length(targetFiles)) {
  file.copy(targetFiles[i], inputDirPath)
}
source(here("prg", "json_to_excel.R"), encoding="UTF-8")
outputDirBef <- read.table(here("temp", "outputDirBef"), sep=",") %>% .[, 1, drop=T]
outputDirAft <- here("output") |> list.dirs(recursive=F)
newDirPath <- setdiff(outputDirAft, outputDirBef)
newDir <- newDirPath |> dirname()
targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
file.rename(newDirPath, file.path(newDir, newDirRename))
# ****************************************
# ファイル5
# テストファイルが増えたらここからコピー
# ****************************************
targetRow <- 5 # <- ここの番号を変える
temp <- read.table(here("temp", "targetPath"), sep=",")
targetPath <- temp[targetRow, 1]
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
if (length(inputDirPath) == 1) {
  fs::dir_delete(inputDirPath)
}
dir.create(inputDirPath)
targetTrialName <- basename(targetPath) |> str_remove("input")
targetTrialName |> write.table(here("temp", "targetTrialName"), sep=",", col.names=F, row.names=F)
targetFiles <- targetPath |> list.files(full.names=T)
outputDirBef <- here("output") |> list.dirs(recursive=F)
outputDirBef |> write.table(here("temp", "outputDirBef"), sep=",", col.names=F, row.names=F)
for (i in 1:length(targetFiles)) {
  file.copy(targetFiles[i], inputDirPath)
}
source(here("prg", "json_to_excel.R"), encoding="UTF-8")
outputDirBef <- read.table(here("temp", "outputDirBef"), sep=",") %>% .[, 1, drop=T]
outputDirAft <- here("output") |> list.dirs(recursive=F)
newDirPath <- setdiff(outputDirAft, outputDirBef)
newDir <- newDirPath |> dirname()
targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
file.rename(newDirPath, file.path(newDir, newDirRename))
# ****************************************
# テストファイルが増えたらここまでコピー


# テストファイルが増えたら↑にペースト
# 終了処理
inputDirPath <- here() |> list.dirs() |> str_extract("^.*/input$") |> na.omit()
fs::dir_delete(inputDirPath)
dir.create(inputDirPath)
dummyPath <- here() |> list.dirs() |> str_extract("^.*/input_dummy$") |> na.omit()
dummyfile <- dummyPath |> list.files(full.names=T)
file.copy(dummyfile, inputDirPath)
