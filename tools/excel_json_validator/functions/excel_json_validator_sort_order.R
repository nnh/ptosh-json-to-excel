#' ソート順の検証
#'
#' @file excel_json_validator_sort_order.R
#' @author Mariko Ohtsuka
#' @date 2026.5.29

kFieldIdJpn    <- "フィールドID"
kOptionNameJpn <- "オプション名"
kOptionSeqJpn  <- "オプションの表示順"
kVisitNumJpn   <- "VISITNUM"

#' シートのソート順不正を検出する
#' @param sheetName 検証対象のシート名
#' @param aliasCol alias_name に対応する列名（日本語）
#' @param fieldCol field_id に対応する列名（日本語）。NULL の場合はシート順のみ
#' @return ソート正常なら NULL、不正なら list(current=, expected=)
GetSortOrderMismatch <- function(sheetName, aliasCol, fieldCol = NULL) {
  sheet <- sheetList[[sheetName]]
  if (!is.null(fieldCol)) {
    current_key <- sheet %>%
      select(all_of(c(aliasCol, fieldCol)))
      if (nrow(current_key) == 0) {
        #  行なしの場合はスキップ
        return(NULL)
      }
      if (nrow(current_key) == 1) {
        # 行数1の場合はソート順不正の可能性がないためスキップ
        return(NULL)
      }
    ref  <- sheetAndFieldOrders %>% select(alias_name, field_id, seq, field_seq)
    # refのalias_nameの_[0-9]+$ を削除してマッチングする
    ref <- ref %>%
      mutate(alias_name = sub("_[0-9]+$", "", alias_name)) %>% arrange(seq, field_seq)
    expected_key <- ref %>% inner_join(sheet, by = c("alias_name" = aliasCol, "field_id" = fieldCol)) %>% select(all_of(c("alias_name", "field_id"))) %>% distinct()
    colnames(expected_key) <- c(aliasCol, fieldCol)
  } else {
    current_key <- sheet %>% select(all_of(aliasCol))
    expected_key <- sheetOrders %>% select(sheet, seq) %>% arrange(seq) %>% select(-seq)
    colnames(expected_key) <- aliasCol
  }
  if (nrow(current_key) != nrow(expected_key)) {
    print(str_c("シート: ", sheetName))
    print(str_c("期待される行数: ", nrow(expected_key)))
    print(str_c("現在の行数: ", nrow(current_key)))
    stop("ソート順不正を検出: 行数が異なります")
  }
  for (i in 1:nrow(current_key)) {
    for (col in colnames(current_key)) {
      if (is.na(current_key[i, col]) && is.na(expected_key[i, col])) next
      if (!is.na(current_key[i, col]) && !is.na(expected_key[i, col]) && current_key[i, col] == expected_key[i, col]) next
      print(str_c("シート: ", sheetName))
      print(str_c("行: ", i, ", 列: ", col))
      print(str_c("現在の値: ", current_key[i, col]))
      print(str_c("期待される値: ", expected_key[i, col]))
      stop("ソート順不正を検出")
    }
  }
}

#' option シートのソート順不正を検出する
#' option シートは (alias_name, オプション名, オプションの表示順) の3段階の階層を持つため専用関数でチェックする。
#' - alias_name が sheetOrders 順であること
#' - (alias_name, オプション名) の連続ブロック内で オプションの表示順 が昇順であること
#' （オプション名の並び順そのものは ContentCheck で担保されるためここではチェックしない）
#' @return ソート正常なら NULL、不正なら list(alias_error=bool, bad_rows=tibble or NULL)
GetOptionSortMismatch <- function(sheet) {
  alias_col   <- kAliasNameJapaneseColumnName
  opt_nm_col  <- kOptionNameJpn
  opt_seq_col <- kOptionSeqJpn

  # alias_name が sheetOrders 順かチェック
  alias_seqs  <- sheetOrders$seq[match(sheet[[alias_col]], sheetOrders$sheet)]
  alias_error <- is.unsorted(alias_seqs, na.rm = TRUE)

  # (alias_name, オプション名) の連続ブロックを識別し、ブロック内で option_seq が昇順かチェック
  block_key   <- paste(sheet[[alias_col]], sheet[[opt_nm_col]], sep = "\t")
  block_id    <- cumsum(c(1L, as.integer(block_key[-1] != block_key[-length(block_key)])))
  opt_seqs    <- as.numeric(sheet[[opt_seq_col]])
  bad_indices <- unlist(tapply(seq_along(opt_seqs), block_id, function(i) {
    if (is.unsorted(opt_seqs[i], na.rm = TRUE)) i else integer(0)
  }))

  if (!alias_error && length(bad_indices) == 0) return(NULL)

  list(
    alias_error = alias_error,
    bad_rows    = if (length(bad_indices) > 0)
                    sheet[bad_indices, c(alias_col, opt_nm_col, opt_seq_col), drop = FALSE]
                  else NULL
  )
}

#' 全シートのソート順を検証する
#' @param sheetList Excel から読み込んだシートリスト
#' @param isVisit VISIT対応試験かどうか
#' @param trialName 試験名（エラーメッセージ用）
CheckSortOrder <- function(sheetList, isVisit, trialName) {

  # name: シート順のみ
  sheetName <- "name"
  if (!is.null(sheetList[[sheetName]])) {
    mismatch <- GetSortOrderMismatch(sheetName, kAliasNameJapaneseColumnName)
  }

  # シート順 + フィールド順でソートされているべきシート
  for (sheetName in c("item_nonvisit", "limitation", "date", "master", "assigned")) {
    if (!is.null(sheetList[[sheetName]])) {
      mismatch <- GetSortOrderMismatch(sheetName, kAliasNameJapaneseColumnName, kFieldIdJpn)
    }
  }

  # option: alias_name のシート順 + (alias_name, オプション名) ブロック内の option_seq 昇順
  sheetName <- "option"
  if (!is.null(sheetList[[sheetName]])) {
    mismatch <- GetOptionSortMismatch(sheetList[[sheetName]])
  }

  # visit (isVisit 時のみ): VISITNUM 昇順
  sheetName <- "visit"
  if (isVisit && !is.null(sheetList[[sheetName]])) {
    visit_nums  <- as.numeric(sheetList[[sheetName]][[kVisitNumJpn]])
    sorted_nums <- sort(visit_nums)
    if (!identical(visit_nums, sorted_nums)) {
      cat(str_c("[ソート順不正] シート: ", sheetName, "\n"))
      cat(str_c("  現在: ", paste(visit_nums,  collapse = ", "), "\n"))
      cat(str_c("  期待: ", paste(sorted_nums, collapse = ", "), "\n"))
      stop("ソート順不正を検出")
    }
  }

  invisible(TRUE)
}
