#' title
#' description
#' @file copy_folders_and_run_functions.R
#' @author Mariko Ohtsuka
#' @date 2025.5.9
rm(list=ls())
# ------ libraries ------
library(tidyverse, warn.conflicts=F)
library(here, warn.conflicts=F)
# ------ constants ------
# ------ functions ------
copyFoldersAndRunProgramMain <- function(targetRow) {
  temp <- read.table(here("temp", "targetPath"), sep=",")
  targetPath <- temp[targetRow, 1]
  print("対象入力ファイル")
  print(targetPath)
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
  
  if (file.size(here("temp", "outputDirBef")) > 0) {
    outputDirBef <- read.table(here("temp", "outputDirBef"), sep = ",") %>% .[, 1, drop=T]
  } else {
    outputDirBef <- NULL
  }
  outputDirAft <- here("output") |> list.dirs(recursive=F)
  if (!is.null(outputDirBef)) {
    newDirPath <- setdiff(outputDirAft, outputDirBef)
  } else {
    newDirPath <- outputDirAft
  }
  newDir <- newDirPath |> dirname()
  targetTrialName <- read.table(here("temp", "targetTrialName"), sep=",") %>% .[1, 1, drop=T]
  newDirRename <- newDirPath |> basename() |> str_c(targetTrialName)
  file.rename(newDirPath, file.path(newDir, newDirRename))  
}
# ------ main ------