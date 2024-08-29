#' title
#' description
#' @file excel_json_validator_gpower.R
#' @author Mariko Ohtsuka
#' @date 2024.8.29
if (exists("keep_objects")) {
  rm(list=setdiff(ls(), keep_objects))
}
# ------ libraries ------
library(tidyverse, warn.conflicts=F)
library(here, warn.conflicts=F)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
tempJsonList <- jsonList
removeJsonList <- names(tempJsonList) %>% .[2:10]
jsonList <- tempJsonList
for (i in 1:length(removeJsonList)) {
  jsonList[[removeJsonList[i]]] <- NULL
}
temp <- names(jsonList)
temp[1] <- temp[1] |> str_replace("[0-9]+$", "xxx")
names(jsonList) <- temp
jsonList[[1]]$alias_name <- temp[1]

fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |>  GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item <- jsonSheetItemList$json
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f8=='NOTDONE'&&f16=='NOTDONE'&&f24=='NOTDONE'&&f32=='NOTDONE'&&f40=='NOTDONE'&&f48=='NOTDONE'&&f56=='NOTDONE'&&f64=='NOTDONE'&&f72=='NOTDONE'&&f80=='NOTDONE'&&f88=='NOTDONE'&&f96=='NOTDONE'&&f104=='NOTDONE'&&f112=='NOTDONE'&&f120=='NOTDONE'&&f128=='NOTDONE'&&f136=='NOTDONE'&&f144=='NOTDONE'&&f152=='NOTDONE'&&f160=='NOTDONE'&&f168=='NOTDONE'", 
                                        "(ga_dasc21_100,field8,)(ga_dasc21_100,field16,)(ga_dasc21_100,field24,)(ga_dasc21_100,field32,)(ga_dasc21_100,field40,)(ga_dasc21_100,field48,)(ga_dasc21_100,field56,)(ga_dasc21_100,field64,)(ga_dasc21_100,field72,)(ga_dasc21_100,field80,)(ga_dasc21_100,field88,)(ga_dasc21_100,field96,)(ga_dasc21_100,field104,)(ga_dasc21_100,field112,)(ga_dasc21_100,field120,)(ga_dasc21_100,field128,)(ga_dasc21_100,field136,)(ga_dasc21_100,field144,)(ga_dasc21_100,field152,)(ga_dasc21_100,field160,)(ga_dasc21_100,field168,)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f108=='NOTDONE'&&f115=='NOTDONE'&&f122=='NOTDONE'&&f129=='NOTDONE'&&f136=='NOTDONE'&&f143=='NOTDONE'&&f150=='NOTDONE'&&f157=='NOTDONE'&&f164=='NOTDONE'&&f171=='NOTDONE'&&f178=='NOTDONE'&&f185=='NOTDONE'&&f192=='NOTDONE'&&f199=='NOTDONE'&&f206=='NOTDONE'&&f213=='NOTDONE'&&f220=='NOTDONE'&&f227=='NOTDONE'", 
                                        "(ga_other_100,field108,)(ga_other_100,field115,)(ga_other_100,field122,)(ga_other_100,field129,)(ga_other_100,field136,)(ga_other_100,field143,)(ga_other_100,field150,)(ga_other_100,field157,)(ga_other_100,field164,)(ga_other_100,field171,)(ga_other_100,field178,)(ga_other_100,field185,)(ga_other_100,field192,)(ga_other_100,field199,)(ga_other_100,field206,)(ga_other_100,field213,)(ga_other_100,field220,)(ga_other_100,field227,)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f235=='NOTDONE'&&f249=='NOTDONE'&&f256=='NOTDONE'&&f263=='NOTDONE'&&f270=='NOTDONE'", 
                                        "(ga_other_100,field235,)(ga_other_100,field249,)(ga_other_100,field256,)(ga_other_100,field263,)(ga_other_100,field270,)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f293=='NOTDONE'&&f300=='NOTDONE'&&f307=='NOTDONE'&&f314=='NOTDONE'&&f321=='NOTDONE'&&f328=='NOTDONE'&&f335=='NOTDONE'&&f342=='NOTDONE'", 
                                        "(ga_other_100,field293,)(ga_other_100,field300,)(ga_other_100,field307,)(ga_other_100,field314,)(ga_other_100,field321,)(ga_other_100,field328,)(ga_other_100,field335,)(ga_other_100,field342,)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f51=='NOTDONE'&&f58=='NOTDONE'&&f65=='NOTDONE'&&f72=='NOTDONE'&&f79=='NOTDONE'&&f86=='NOTDONE'&&f93=='NOTDONE'&&f100=='NOTDONE'", 
                                        "(ga_other_100,field51,)(ga_other_100,field58,)(ga_other_100,field65,)(ga_other_100,field72,)(ga_other_100,field79,)(ga_other_100,field86,)(ga_other_100,field93,)(ga_other_100,field100,)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "f7=='NOTDONE'&&f15=='NOTDONE'&&f22=='NOTDONE'&&f29=='NOTDONE'&&f36=='NOTDONE'&&f43=='NOTDONE'", 
                                        "(ga_other_100,field7,)(ga_other_100,field15,)(ga_other_100,field22,)(ga_other_100,field29,)(ga_other_100,field36,)(ga_other_100,field43,)", 
                                        df_item$formula_if_references)

df_item_json <- df_item |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList)
##########
# action #
##########
sheetList$action$id <- sheetList$action$id |> as.integer()
sheetList$action$field_item_id <- sheetList$action$field_item_id |> as.integer()
checkChecklist$action <- sheetList |> CheckAction()
###########
# display #
###########
checkChecklist$display <- sheetList |> CheckDisplay()
##########
# number #
##########
sheetList$number$validators.numericality.validate_numericality_less_than_or_equal_to <- ifelse(
  is.na(sheetList$number$validators.numericality.validate_numericality_less_than_or_equal_to), 
  "", 
  sheetList$number$validators.numericality.validate_numericality_less_than_or_equal_to
)
checkChecklist$number <- sheetList |> CheckNumber()
########
# name #
########
checkChecklist$name <- sheetList |> CheckName(jsonList)
##########
# option #
##########
checkChecklist$option <- sheetList |> CheckOption()
###########
# comment #
###########
checkChecklist$content <- sheetList |> CheckContent()
###############
# explanation #
###############
checkChecklist$explanation <- sheetList |> CheckExplanation()
############
# presence #
############
checkChecklist$presence <- sheetList |> CheckPresence()
##########
# master #
##########
checkChecklist$master <- sheetList |> CheckMaster()
#########
# visit #
#########
checkChecklist$visit <- sheetList |> CheckVisit()
#########
# alert #
#########
sheetList$alert$normal_range.less_than_or_equal_to <- sheetList$alert$normal_range.less_than_or_equal_to |> as.integer()
checkChecklist$alert <- sheetList |> CheckAlert()
#########
# title #
#########
checkChecklist$title <- sheetList |> CheckTitle()
############
# assigned #
############
checkChecklist$assigned <- sheetList |> CheckAssigned()
