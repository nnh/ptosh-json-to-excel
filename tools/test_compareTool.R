#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(testthat)
# ------ constants ------
source(here("tools", "compareTool_functions.R"), encoding="UTF-8")
compare_output_path <- NULL
kNormalfilesAndSheets <- list(
  test1.xlsx=c("item", "name", "options"),
  test1の.xlsx=c("Cdisc_Sheet_Configs", "Cdisc_Sheet_Configs_Pivot", "Field_Items"),
  test3.xlsx=c("Allocation", "Cdisc_Sheet_Configs")
)
df_test_GetColnamesForCompare <- data.frame("test2"=c(10, 22, 33),
                                            "test1"=c(1, 2, 3),
                                            "jpname"=c(100, 200, 300),
                                            "alias_name"=c(4, 5, 6),
                                            "name"=c("d", "e", "f"),
                                            "alias_name_2"=c("a", "b", "c"))

# ------ main ------
# 正常系
test_that("CheckExcelFilesConsistency_1:ファイル数、ファイル名が一致している", {
  result <- CheckExcelFilesConsistency(
    here("tools", "test_compareTool", "normalCase", "dir1"),
    here("tools", "test_compareTool", "normalCase", "dir2")
  )
  expect_equal(result, names(kNormalfilesAndSheets))
})
test_that("CheckExcelSheetsConsistency_1:シート数、シート名が一致している", {
  result <- CheckExcelSheetsConsistency(
    "test1.xlsx",
    here("tools", "test_compareTool", "normalCase", "dir1"),
    here("tools", "test_compareTool", "normalCase", "dir2")
  )
  expect_equal(result, kNormalfilesAndSheets$test1.xlsx)
})
test_that("ExecCompare_fileNameAndSheetName_1:ファイル数、ファイル名、シート数、シート名が一致している", {
  result <- ExecCompare_fileNameAndSheetName(
    here("tools", "test_compareTool", "normalCase", "dir1"),
    here("tools", "test_compareTool", "normalCase", "dir2")
  )
  expect_equal(result, kNormalfilesAndSheets)
})
test_that("GetColnamesForCompare_1:シート名がnameであればname, alias_name、それ以外の列がソートされた結果が返る", {
  result <- GetColnamesForCompare(df_test_GetColnamesForCompare, "name")
  expect_equal(result, c("name", "alias_name", "alias_name_2", "jpname", "test1", "test2"))
})
test_that("GetColnamesForCompare_1:シート名がname以外であればjpname, alias_name、それ以外の列がソートされた結果が返る", {
  result <- GetColnamesForCompare(df_test_GetColnamesForCompare, "item")
  expect_equal(result, c("jpname", "alias_name", "alias_name_2", "name", "test1", "test2"))
})
test_that("CheckExcelValuesConsistency:指定したシートの列名、値が一致している", {
  result <- CheckExcelValuesConsistency(
    read.xlsx(here("tools", "test_compareTool", "normalCase", "dir1", "test1.xlsx"), "name"),
    read.xlsx(here("tools", "test_compareTool", "normalCase", "dir2", "test1.xlsx"), "name"),
    "name",
    "test1.xlsx",
    NULL
  )
  expect_equal(result, T)
})
test_that("ExecCompare_values:指定したブックのすべてのシートの列名、値が一致している", {
  result <- ExecCompare_values(
    here("tools", "test_compareTool", "normalCase", "dir1"),
    here("tools", "test_compareTool", "normalCase", "dir2"),
    kNormalfilesAndSheets,
    NULL
  )
  expect_equal(result, T)
})
# 異常系
testname <- "CheckExcelFilesConsistency_2"
test_that(testname %>% str_c(":ファイル数が異なる"), {
  result <- CheckExcelFilesConsistency(
    here("tools", "test_compareTool", testname, "dir1"),
    here("tools", "test_compareTool", testname, "dir2")
  )
  expect_equal(result, NULL)
})
testname <- "CheckExcelFilesConsistency_3"
test_that(testname %>% str_c(":ファイル数が同じでファイル名が異なる"), {
  result <- CheckExcelFilesConsistency(
    here("tools", "test_compareTool", testname, "dir1"),
    here("tools", "test_compareTool", testname, "dir2")
  )
  expect_equal(result, NULL)
})
testname <- "CheckExcelSheetsConsistency_2"
test_that(testname %>% str_c(":指定したブックのシート名が異なる"), {
  result <- CheckExcelSheetsConsistency(
    "test1.xlsx",
    here("tools", "test_compareTool", testname, "dir1"),
    here("tools", "test_compareTool", testname, "dir2")
  )
  expect_equal(result, NULL)
})
testname <- "ExecCompare_fileNameAndSheetName_2"
test_that(testname %>% str_c(":複数のブックにおいてシート名が異なるものがある"), {
  result <- ExecCompare_fileNameAndSheetName(
    here("tools", "test_compareTool", "CheckExcelSheetsConsistency_2", "dir1"),
    here("tools", "test_compareTool", "CheckExcelSheetsConsistency_2", "dir2")
  )
  expect_equal(result, NULL)
})
testname <- "CheckExcelValuesConsistency_2"
test_that(testname %>% str_c(":指定したシートの列名が異なる"), {
  result <- CheckExcelValuesConsistency(
    read.xlsx(here("tools", "test_compareTool", testname, "dir1", "test1.xlsx"), "name"),
    read.xlsx(here("tools", "test_compareTool", testname, "dir2", "test1.xlsx"), "name"),
    "name",
    "test1.xlsx",
    NULL
  )
  expect_equal(result, F)
})
testname <- "ExecCompare_values_2"
test_that(testname %>% str_c("列名が異なるブックが存在する"), {
  result <- ExecCompare_values(
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_2", "dir1"),
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_2", "dir2"),
    kNormalfilesAndSheets,
    NULL
  )
  expect_equal(result, F)
})
testname <- "CheckExcelValuesConsistency_3"
test_that(testname %>% str_c(":指定したシートの列数が異なる"), {
  result <- CheckExcelValuesConsistency(
    read.xlsx(here("tools", "test_compareTool", testname, "dir1", "test1.xlsx"), "name"),
    read.xlsx(here("tools", "test_compareTool", testname, "dir2", "test1.xlsx"), "name"),
    "name",
    "test1.xlsx",
    NULL
  )
  expect_equal(result, F)
})
testname <- "ExecCompare_values_3"
test_that(testname %>% str_c("列数が異なるブックが存在する"), {
  result <- ExecCompare_values(
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_3", "dir1"),
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_3", "dir2"),
    kNormalfilesAndSheets,
    NULL
  )
  expect_equal(result, F)
})
testname <- "CheckExcelValuesConsistency_4"
test_that(testname %>% str_c(":指定したシートの値が異なる"), {
  result <- CheckExcelValuesConsistency(
    read.xlsx(here("tools", "test_compareTool", testname, "dir1", "test1.xlsx"), "name"),
    read.xlsx(here("tools", "test_compareTool", testname, "dir2", "test1.xlsx"), "name"),
    "name",
    "test1.xlsx",
    NULL
  )
  expect_equal(result, F)
})
testname <- "ExecCompare_values_4"
test_that(testname %>% str_c("値が異なるブックが存在する"), {
  result <- ExecCompare_values(
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_4", "dir1"),
    here("tools", "test_compareTool", "CheckExcelValuesConsistency_4", "dir2"),
    kNormalfilesAndSheets,
    NULL
  )
  expect_equal(result, F)
})
