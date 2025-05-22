#' title
#' description
#' @file excel_json_validator_allb19.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
if (exists("keep_objects")) {
  rm(list = setdiff(ls(), keep_objects))
}
# ------ libraries ------
library(tidyverse, warn.conflicts = F)
library(here, warn.conflicts = F)
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
# ------ constants ------
# ------ functions ------
# ------ main ------
fieldItems <- jsonList |> GetFieldItemsByJsonList()
jpNameAndAliasName <- jsonList |> GetNameAndAliasNameByJson()
checkChecklist <- list()
##############
# item sheet #
##############
jsonSheetItemList <- GetItemFromJson(sheetList, jsonList)
df_item <- jsonSheetItemList$json
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',12\\)"),
  "(registration,field12,症例登録日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration', 12\\)"),
  "(registration,field12,症例登録日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',3\\)"),
  "(registration,field3,生年月日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('evaluationtp2', 21\\)"),
  "(evaluationtp2,field21,判定日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('blin1', 820\\)"),
  "(blin1,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr1fisrt', 820\\)"),
  "(hr1fisrt,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('prephase',825\\)"),
  "(prephase,field825,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('induction', 820\\)"),
  "(induction,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('inductionlab', 109\\)"),
  "(inductionlab,field109,検査日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('evaluationtp1', 119\\)"),
  "(evaluationtp1,field119,判定日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr2fisrt', 820\\)"),
  "(hr2fisrt,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr2second', 820\\)"),
  "(hr2second,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hr3second', 820\\)"),
  "(hr3second,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('prephase', 825\\)"),
  "(prephase,field825,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('maitenance',829\\)"),
  "(maitenance,field829,維持療法終了日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('sct1', 16\\)"),
  "(sct1,field16,移植日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration',26\\)"),
  "(registration,field26,初発診断日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('registration', 26\\)"),
  "(registration,field26,初発診断日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('hdm', 820\\)"),
  "(hdm,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('reinduction2', 820\\)"),
  "(reinduction2,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('blin2', 820\\)"),
  "(blin2,field820,投与開始日)",
  df_item$references_after
)
df_item$references_after <- ifelse(df_item$validate_date_after_or_equal_to |> str_detect("ref\\('discon', 6\\)"),
  "(discon,field6,完了／中止理由発生日)",
  df_item$references_after
)
df_item$references_before <- ifelse(df_item$validate_date_before_or_equal_to |> str_detect("ref\\('evaluationtp2', 21\\)"),
  "(evaluationtp2,field21,判定日)",
  df_item$references_before
)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if |> str_detect("ref\\('registration', 4\\)"),
  "(registration,field4,性別)",
  df_item$formula_if_references
)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)",
  "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)",
  df_item$formula_if_references
)
df_item$formula_if_references <- ifelse(df_item$validate_formula_if == "(f741=='Y'&&(f744=='Dose not reduced'||f744=='Dose reduction for Adverse Event'||f744=='Dose reduction for Other'))||(f741== 'N'&&(((ref('baseline', 119)=='CNS-1 WITHOUT TRAUMATIC TAP')||(ref('baseline', 119)=='CNS-1 WITH TRAUMATIC TAP'))&&f744=='Not Administration with CNS-1'))||f744=='Dosage Mistake'||f744=='Drug Interrupted for Adverse Event'||f744=='Drug Interrupted for Other'",
  "(reinduction2,field741,髄注（TIT）の有無)(reinduction2,field744,用量調整理由/減量変更理由)(baseline,field119,CNS status分類)",
  df_item$formula_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if |> str_detect("ref\\('registration', 4\\)"),
  "(registration,field4,性別)",
  df_item$presence_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(f1==1&&ref('hdm',820)!=0)||((f1==2||f1==5)&&ref('hdm2',820)!=0)||(f1==3&&ref('hr3second',820)!=0)||(f1==4&&ref('blin3',820)!=0)||(f1==6&&ref('hdm5',820)!=0)",
  "(consoliarm,field1,\t 強化療法のランダム化割り付け群)(hdm,field820,投与開始日)(hdm2,field820,投与開始日)(hr3second,field820,投与開始日)(blin3,field820,投与開始日)(hdm5,field820,投与開始日)",
  df_item$presence_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "field90 == 'NOT DONE'",
  "(evaluationtp1,field90,)",
  df_item$presence_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "(age(ref('registration', 26), ref('registration', 3)) > 39)",
  "(registration,field26,初発診断日)(registration,field3,生年月日)",
  df_item$presence_if_references
)
df_item$presence_if_references <- ifelse(df_item$validate_presence_if == "f6=='NOT DONE'",
  "(pcrmrdtp2,field6,)",
  df_item$presence_if_references
)
df_item_json <- df_item |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet <- jsonSheetItemList$sheet |>
  as.data.frame() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
df_item_sheet[2022, 10] <- ifelse(df_item_sheet[2022, 10] == "(baseline,field119,CNS status分類)(reinduction2,field741,髄注（TIT）の有無)(reinduction2,field744,用量調整理由/減量変更理由)",
  "(reinduction2,field741,髄注（TIT）の有無)(reinduction2,field744,用量調整理由/減量変更理由)(baseline,field119,CNS status分類)",
  NA
)
checkChecklist$item <- CheckTarget(df_item_sheet, df_item_json)
for (row in 1:nrow(df_item_sheet)) {
  for (col in 1:ncol(df_item_sheet)) {
    if (df_item_sheet[row, col] != df_item_json[row, col]) {
      print(row)
      print(col)
      print(df_item_sheet[row, col])
      print(df_item_json[row, col])
      print(df_item_sheet[row, col - 1])
      print(df_item_json[row, col - 1])
      stop()
    }
  }
}
##############
# allocation #
##############
allocationJsonAndSheet <- sheetList |> GetAllocation(jsonList)
allocationJsonAndSheet$sheet[9, 9] <- ifelse(allocationJsonAndSheet$sheet[9, 9] == "(baseline,field119,CNS status分類)(pcrmrdtp2,field6,)(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field87,白血球数（/uL）)(inductionlab,field81,PSL反応性)(inductionlab,field102,骨髄血腫瘍芽球（%）)(evaluationtp1,field89,PCR-MRD測定（定量カテゴリ選択）)(baseline,field327,キメラ遺伝子)(baseline,field159,染色体本数)(baseline,field199,DNA Index)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解判定)",
  "(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field87,白血球数（/uL）)(inductionlab,field81,PSL反応性)(inductionlab,field102,骨髄血腫瘍芽球（%）)(evaluationtp1,field89,PCR-MRD測定（定量カテゴリ選択）)(baseline,field327,キメラ遺伝子)(baseline,field159,染色体本数)(baseline,field199,DNA Index)(baseline,field119,CNS status分類)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解 判定)",
  NA
)
temp <- allocationJsonAndSheet$sheet[10, 9] %>% str_remove_all(temp, " ")
temp2 <- "(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field119,CNS status分類)(baseline,field120,)(pcrmrdtp2,field6,)(baseline,field87,白血球数（/uL）)(inductionlab,field81,PSL反応性)(baseline,field327, キメラ遺伝子)(baseline,field159,染色体本数)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解判定)" %>% str_remove_all(" ")
temp == temp2
allocationJsonAndSheet$sheet[10, 9] <- ifelse(temp == temp2,
  "(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field87,白血球数（/uL）)(inductionlab,field81,PSL反応性)(baseline,field327,キメラ遺伝子)(baseline,field159,染色体本数)(baseline,field119,CNS status分類)(baseline,field120,)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解判定)",
  NA
)
allocationJsonAndSheet$sheet[11, 9] <- ifelse(allocationJsonAndSheet$sheet[11, 9] == "(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field119,CNS status分類)(baseline,field120,)(pcrmrdtp2,field6,)(baseline,field87,白血球数（/uL）)(inductionlab,field102,骨髄血腫瘍芽球（%）)(inductionlab,field81,PSL反応性)(baseline,field327,キメラ遺伝子)(baseline,field159,染色体本数)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解判定)",
  "(registration,field26,初発診断日)(registration,field3,生年月日)(baseline,field87,白血球数（/uL）)(inductionlab,field102,骨髄血腫瘍芽球（%）)(inductionlab,field81,PSL反応性)(baseline,field327,キメラ遺伝子)(baseline,field159,染色体本数)(baseline,field119,CNS status分類)(baseline,field120,)(pcrmrdtp2,field5,PCR-MRD測定（定量カテゴリ選択）)(pcrmrdtp2,field6,)(evaluationtp2,field18,寛解判定)", NA
)
allocationJsonAndSheet$sheet[13, 9] <- ifelse(allocationJsonAndSheet$sheet[13, 9] == "(baseline,field327,キメラ遺伝子)(baseline,field222,CD19)",
  "(baseline,field222,CD19)(baseline,field327,キメラ遺伝子)",
  NA
)
allocationJsonAndSheet$sheet[14, 9] <- ifelse(allocationJsonAndSheet$sheet[14, 9] == "(baseline,field327,キメラ遺伝子)(baseline,field222,CD19)(baseline,field223,)",
  "(baseline,field222,CD19)(baseline,field223,)(baseline,field327,キメラ遺伝子)",
  NA
)
checkChecklist$allocation <- CheckTarget(allocationJsonAndSheet$sheet, allocationJsonAndSheet$json)
for (row in 1:nrow(allocationJsonAndSheet$sheet)) {
  for (col in 1:ncol(allocationJsonAndSheet$sheet)) {
    if (is.na(allocationJsonAndSheet$sheet[row, col]) && is.na(allocationJsonAndSheet$json[row, col])) {
      next
    } else if (allocationJsonAndSheet$sheet[row, col] != allocationJsonAndSheet$json[row, col]) {
      if (row != 9) {
        print(row)
        print(col)
        print(allocationJsonAndSheet$sheet[row, col])
        print(allocationJsonAndSheet$json[row, col])
        print(allocationJsonAndSheet$sheet[row, col - 1])
        print(allocationJsonAndSheet$json[row, col - 1])
        stop()
      }
    }
  }
}
##########
# action #
##########
checkChecklist$action <- sheetList |> CheckAction()
###########
# display #
###########
checkChecklist$display <- sheetList |> CheckDisplay()
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
# title #
#########
checkChecklist$title <- sheetList |> CheckTitle()
############
# assigned #
############
checkChecklist$assigned <- sheetList |> CheckAssigned()
##############
# limitation #
##############
checkChecklist$limitation <- CheckLimitation(sheetList, jsonList)
