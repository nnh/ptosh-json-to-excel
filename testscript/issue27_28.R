#' title
#' description
#' @file issue27_28.R
#' @author Mariko Ohtsuka
#' @date 2024.8.22
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
ExecCompareIssue27 <- function(bef, aft) {
  if (!identical(names(bef), names(aft))) {
    print(names(bef))
    print(names(aft))
    stop("sheetname error.")
  }
  sheetnames <- names(bef)
  for (i in 1:length(sheetnames)) {
    sheetname <- sheetnames[i]
    if (!identical(bef[[sheetname]], aft[[sheetname]])) {
      print(sheetname)
      stop("values error.")
    }
  }
  print("compare ok.")
} 

IsExceptionItem <- function(rowCount, colCount, test2) {
  (rowCount == 135 & colCount == 10 & test2 == "(primary_diag_100,field87,プロトコール)(primary_diag_100,field85,アーム)") ||
    ((rowCount == 158 | rowCount == 159) & colCount == 15 & test2 == "(prior_medications_110,field2,造血細胞移植歴の有無)(prior_medications_110,field4,ドナー情報)")
}

# ------ main ------
beforeSheets <- "output_20240820095805_gpower" |> ReadChecklist()
afterSheets <- "output_20240822165938_gpower" |> ReadChecklist()
print("gpower")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095158_allb19" |> ReadChecklist()
afterSheets <- "output_20240822165425_allb19" |> ReadChecklist()
print("allb19")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095547_bev" |> ReadChecklist()
afterSheets <- "output_20240822165821_bev" |> ReadChecklist()
print("bev")
ExecCompareIssue27(beforeSheets, afterSheets)

beforeSheets <- "output_20240820095931_tran" |> ReadChecklist()
afterSheets <- "output_20240822170047_tran" |> ReadChecklist()
print("tran")
ExecCompareIssue27(beforeSheets, afterSheets)

allr23Sheets <- "output_20240822165505_allr23" |> ReadChecklist()


#allr23inputPath <- here("input_allr23") |> list.files(pattern="*.json", full.names=T)
#allr23jsonList <- allr23inputPath |> map( ~ read_json(.))
#names(allr23jsonList) <- allr23inputPath |> basename() |> str_remove(".json")
allr23jsonList <- here("input_allr23") |> LoadJsonList()
fieldItems <- allr23jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- allr23jsonList |>  GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(allr23Sheets, allr23jsonList)
df_item <- jsonSheetItemList$json
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 3\\)"), 
                                          "(registration,field3,性別)", 
                                          df_item$presence_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 3\\)"), 
                                         "(registration,field3,性別)", 
                                         df_item$formula_if_references)
df_item[177, 8] <- ifelse(df_item[177, 8] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
df_item[176, 10] <- ifelse(df_item[176, 10] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
df_item[177, 10] <- ifelse(df_item[177, 10] == "(registration,field3,性別)", "(radiotherapy_500,field46,本研究登録後から再発/観察終了までに放射線療法を実施しましたか)(registration,field3,性別)", NA)
df_item_json <- df_item |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- allr23Sheets |> CheckAllocation(allr23jsonList)
##########
# action #
##########
checkChecklist$action <- allr23Sheets |> CheckAction()
###########
# display #
###########
checkChecklist$display <- allr23Sheets |> CheckDisplay()
##########
# number #
##########
checkChecklist$number <- allr23Sheets |> CheckNumber()
########
# name #
########
checkChecklist$name <- allr23Sheets |> CheckName(allr23jsonList)
##########
# option #
##########
checkChecklist$option <- allr23Sheets |> CheckOption()
###########
# comment #
###########
checkChecklist$content <- allr23Sheets |> CheckContent()

#########
# alert #
#########
normalRanges <- fieldItems |> map( ~ {
  fieldItem <- .
  res <- fieldItem |> map( ~ .$normal_range) |> list_flatten()
  return(res)
}) |> list_flatten()
