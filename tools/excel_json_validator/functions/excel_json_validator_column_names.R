#' 各シートの列名が想定通りかどうかを確認する
#'
#' @file excel_json_validator_column_names.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28

# 列が可変なため、列名チェックをスキップするシート
kSkipColumnCheckSheets <- c("item_visit", "sheet_groups_visit", "sheet_groups_nonvisit")

# 各シートの期待列名（リテラル定義）
# visit シートは isVisit によって異なるため visit / visit_to_visit の2キーで定義
kExpectedColumnNames <- list(
  name = c(
    "シート名", "シート名英数字別名", "画像登録欄の数"
  ),
  item_nonvisit = c(
    "シート名", "シート名英数字別名", "フィールドID", "ラベル", "オプション名", "デフォルト値",
    "バリデータ.必須がON.条件", "条件の参照先情報",
    "バリデータ.論理式.論理式", "論理式の参照先情報", "バリデータ.論理式.エラーメッセージ",
    "バリデータ.日付.最小値", "最小値の参照先情報",
    "バリデータ.日付.最大値", "最大値の参照先情報",
    "フィールドタイプ"
  ),
  visit = c(
    "シート名", "シート名英数字別名", "フィールドID", "デフォルト値"
  ),
  visit_to_visit = c(
    "VISITNUM", "VISIT"
  ),
  allocation = c(
    "シート名", "シート名英数字別名",
    "Zelenの施設内バランス化", "施設間の差", "二重盲検", "割付責任者メールアドレス", "割付方法",
    "割付グループ.論理式", "割付グループ.論理式の参照先情報",
    "割付グループ.コード", "割付グループ.ラベル", "割付グループ.エラーメッセージ",
    "調整因子フィールド.式", "調整因子フィールド.式の参照先情報"
  ),
  option = c(
    "シート名", "シート名英数字別名", "オプション名", "ラベル", "オプションの表示順", "コード", "表示"
  ),
  master = c(
    "シート名", "シート名英数字別名", "フィールドID", "ラベル", "保存先のマスタ"
  ),
  assigned = c(
    "シート名", "シート名英数字別名", "フィールドID", "ラベル", "デフォルト値"
  ),
  limitation = c(
    "シート名", "シート名英数字別名", "フィールドID", "ラベル", "デフォルト値",
    "アラート条件.超える場合", "アラート条件.未満の場合",
    "バリデータ.数値.最大値", "バリデータ.数値.最小値"
  ),
  date = c(
    "シート名", "シート名英数字別名", "フィールドID", "ラベル",
    "日付の最小値", "日付の最小値の参照先情報", "日付の最大値", "日付の最大値の参照先情報"
  )
)

#' シートの期待列名を返す
#' @param sheetName シート名
#' @param isVisit VISIT対応試験かどうか
#' @return 期待される列名ベクトル（対応するマッピングがない場合は NULL）
GetExpectedColumnNames <- function(sheetName, isVisit) {
  key <- if (sheetName == "visit" && isVisit) "visit_to_visit" else sheetName
  kExpectedColumnNames[[key]]
}

#' 全シートの列名が想定通りかを検証する
#' @param sheetList Excel から読み込んだシートリスト
#' @param isVisit VISIT対応試験かどうか
#' @param trialName 試験名（エラーメッセージ用）
CheckColumnNames <- function(sheetList, isVisit, trialName) {
  targetSheets <- names(sheetList) %>% discard(~ .x %in% kSkipColumnCheckSheets)

  errors <- list()
  for (sheetName in targetSheets) {
    expected <- GetExpectedColumnNames(sheetName, isVisit)
    if (is.null(expected)) next
    actual <- colnames(sheetList[[sheetName]])
    if (!identical(expected, actual)) {
      errors[[sheetName]] <- list(expected = expected, actual = actual)
    }
  }

  if (length(errors) > 0) {
    for (nm in names(errors)) {
      cat(str_c("[列名不一致] シート: ", nm, "\n"))
      cat(str_c("  期待: ", paste(errors[[nm]]$expected, collapse = ", "), "\n"))
      cat(str_c("  実際: ", paste(errors[[nm]]$actual, collapse = ", "), "\n"))
    }
    stop(str_c("列名の検証に失敗しました: ", trialName))
  }

  invisible(TRUE)
}
