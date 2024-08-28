#' title
#' description
#' @file excel_json_validator_tran.R
#' @author Mariko Ohtsuka
#' @date 2024.8.27
keep_objects <- c("jsonList", "sheetList")
rm(list = setdiff(ls(), keep_objects))
# ------ libraries ------
library(tidyverse)
library(here)
source(here("tools", "excel_json_validator_common.R"), encoding="UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |>  GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item <- jsonSheetItemList$json
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',12\\)"), 
                                        "(registration,field12,症例登録日)", 
                                        df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration', 12\\)"), 
                                   "(registration,field12,症例登録日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',3\\)"), 
                                        "(registration,field3,生年月日)", 
                                        df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('evaluationtp2', 21\\)"), 
                                   "(evaluationtp2,field21,判定日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('blin1', 820\\)"), 
                                   "(blin1,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr1fisrt', 820\\)"), 
                                   "(hr1fisrt,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('prephase',825\\)"), 
                                   "(prephase,field825,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('induction', 820\\)"), 
                                   "(induction,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('inductionlab', 109\\)"), 
                                   "(inductionlab,field109,検査日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('evaluationtp1', 119\\)"), 
                                   "(evaluationtp1,field119,判定日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr2fisrt', 820\\)"), 
                                   "(hr2fisrt,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr2second', 820\\)"), 
                                   "(hr2second,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr3second', 820\\)"), 
                                   "(hr3second,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('prephase', 825\\)"), 
                                   "(prephase,field825,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('maitenance',829\\)"), 
                                   "(maitenance,field829,維持療法終了日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('sct1', 16\\)"), 
                                   "(sct1,field16,移植日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',26\\)"), 
                                   "(registration,field26,初発診断日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration', 26\\)"), 
                                   "(registration,field26,初発診断日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hdm', 820\\)"), 
                                   "(hdm,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('reinduction2', 820\\)"), 
                                   "(reinduction2,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('blin2', 820\\)"), 
                                   "(blin2,field820,投与開始日)", 
                                   df_item$references_after)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('discon', 6\\)"), 
                                   "(discon,field6,完了／中止理由発生日)", 
                                   df_item$references_after)
df_item$references_before <- ifelse(df_item$validate_date_before_or_equal_to |> str_detect("ref\\('evaluationtp2', 21\\)"), 
                                    "(evaluationtp2,field21,判定日)", 
                                   df_item$references_before)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 4\\)"), 
                                   "(registration,field4,性別)", 
                                   df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)", 
                                         "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)", 
                                         df_item$formula_if_references)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(f741=='Y'&&(f744=='Dose not reduced'||f744=='Dose reduction for Adverse Event'||f744=='Dose reduction for Other'))||(f741== 'N'&&(((ref('baseline', 119)=='CNS-1 WITHOUT TRAUMATIC TAP')||(ref('baseline', 119)=='CNS-1 WITH TRAUMATIC TAP'))&&f744=='Not Administration with CNS-1'))||f744=='Dosage Mistake'||f744=='Drug Interrupted for Adverse Event'||f744=='Drug Interrupted for Other'", 
                                        "(reinduction2,field741,髄注（TIT）の有無)(reinduction2,field744,用量調整理由/減量変更理由)(baseline,field119,CNS status分類)", 
                                        df_item$formula_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 4\\)"), 
                                        "(registration,field4,性別)", 
                                        df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)", 
                                         "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "field90 == 'NOT DONE'", 
                                         "(evaluationtp1,field90,)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(age(ref('registration', 26), ref('registration', 3)) > 39)", 
                                         "(registration,field26,初発診断日)(registration,field3,生年月日)", 
                                         df_item$presence_if_references)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "f6=='NOT DONE'", 
                                         "(pcrmrdtp2,field6,)", 
                                         df_item$presence_if_references)
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
checkChecklist$alert <- sheetList |> CheckAlert()
#########
# title #
#########
checkChecklist$title <- sheetList |> CheckTitle()
############
# assigned #
############
checkChecklist$assigned <- sheetList |> CheckAssigned()
