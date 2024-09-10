#' title
#' description
#' @file excel_json_validator_blin_b_all.R
#' @author Mariko Ohtsuka
#' @date 2024.9.9
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
removeJsonList <- names(tempJsonList) %>% .[29]
jsonList <- tempJsonList
for (i in 1:length(removeJsonList)) {
  jsonList[[removeJsonList[i]]] <- NULL
}
temp <- names(jsonList)
temp[29] <- temp[29] |> str_replace("[0-9]+$", "xxx")
names(jsonList) <- temp
jsonList[[29]]$alias_name <- temp[29]

fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |>  GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item <- jsonSheetItemList$json
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "((age(ref('registration',11), ref('registration', 2)) < 10) && f9 < 50000 && f16 == 'SR') || ((age(ref('registration',11), ref('registration', 2)) < 10) && f9 >= 50000 && f16 == 'HR')  || ((age(ref('registration',11), ref('registration', 2)) >= 10) && f16 == 'HR')", 
                                        "(registration,field11,初発診断日)(registration,field2,生年月日)(allocationfac_100,field9,診断時白血球数（/uL）)(allocationfac_100,field16,NCI/Rome 分類)",
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 3\\)=='M'"), 
                                         "(registration,field3,性別)", 
                                         df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration',3\\)=='M'"), 
                                        "(registration,field3,性別)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 3\\)=='F'"), 
                                        "(registration,field3,性別)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration',3\\)=='F'"), 
                                        "(registration,field3,性別)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration',3\\)=='M'"), 
                                        "(registration,field3,性別)", 
                                        df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(ref('registration',3)=='M'&&f2=='N')||ref('registration',3)=='F'", 
                                         "(registration,field3,性別)(lab_3000,field2,妊娠可能な被験者である)", 
                                         df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(ref('registration',3)=='M'&&f200=='N')||ref('registration',3)=='F'", 
                                        "(registration,field3,性別)(screening_100,field200,妊娠可能な被験者である)", 
                                        df_item$formula_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 3\\)=='M'"), 
                                         "(registration,field3,性別)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(ref('registration',3)=='M'&&f2=='N')||ref('registration',3)=='F'", 
                                         "(registration,field3,性別)(lab_3000,field2,妊娠可能な被験者である)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration',3\\)=='M'"), 
                                         "(registration,field3,性別)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration',3\\)=='F'"), 
                                         "(registration,field3,性別)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 3\\)=='F'"), 
                                         "(registration,field3,性別)", 
                                         df_item$presence_if_references)


df_item_json <- df_item |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |> as.data.frame() %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
##############
# allocation #
##############
checkChecklist$allocation <- sheetList |> CheckAllocation(jsonList)
checkChecklist$allocation$json$formula_field_references <- ifelse(
  checkChecklist$allocation$json$formula_field == "ref('allocationfac_100', 16) =='SR'",
  "(allocationfac_100,field16,NCI/Rome分類)=='SR'", 
  checkChecklist$allocation$json$formula_field_references)
if (identical(checkChecklist$allocation$sheet, checkChecklist$allocation$json)) {
  checkChecklist$allocation <- NULL
}
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
sheetList$comment[10, "content"] <- sheetList$comment[10, "content"] |> 
  str_replace("意思がある。\\n", "意思がある。\\\n\\\n") |> 
  str_replace("同意すること\\n女性", "同意すること\\\n\\\n女性") |> 
  str_replace("\\n6\\. 同意", "\\\n\\\n6\\. 同意")
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
