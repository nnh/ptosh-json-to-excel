#' edit_checklist_convert_column_name
#' Convert column names from English to Japanese
#'
#' @file edit_checklist_convert_column_name.R
#' @author Mariko Ohtsuka
#' @date 2025.12.8
renameColumnsFromEnglishToJapanese <- function(df, nameMap) {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(nameMap), !is.null(names(nameMap)))

    names(df) <- vapply(names(df), function(nm) {
        if (nm %in% names(nameMap)) nameMap[[nm]] else nm
    }, FUN.VALUE = character(1))
    return(df)
}

GetEngToJpnColumnMappings <- function() {
    itemColumnName <- c(
        jpname = "シート名",
        alias_name = kAliasNameJapaneseColumnName,
        name = "フィールドID",
        label = "ラベル",
        option.name = "オプション名",
        default_value = "デフォルト値",
        validators.presence.validate_presence_if = "バリデータ.必須がON.条件",
        presence_if_references = "条件の参照先情報",
        validators.formula.validate_formula_if = "バリデータ.論理式.論理式",
        formula_if_references = "論理式の参照先情報",
        validators.formula.validate_formula_message = "バリデータ.論理式.エラーメッセージ",
        validators.date.validate_date_after_or_equal_to = "バリデータ.日付.最小値",
        references_after = "最小値の参照先情報",
        validators.date.validate_date_before_or_equal_to = "バリデータ.日付.最大値",
        references_before = "最大値の参照先情報"
    )
    engToJpnColumnMappings <- list(
        name = c(
            name = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            images_count = "画像登録欄の数"
        ),
        item_old = itemColumnName,
        item_visit_old = c(itemColumnName,
            numericality_normal_range_check = "数値チェック・アラート条件の有無"
        ),
        item = c(itemColumnName, field_type = "フィールドタイプ"),
        option = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            option.name = "オプション名",
            option.values_name = "ラベル",
            option.values_seq = "-",
            option.values_code = "コード",
            option.values_is_usable = "表示"
        ),
        visit = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            default_value = "デフォルト値"
        ),
        visit_to_visit = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "VISITNUM",
            default_value = "VISIT"
        ),
        master = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            link_type = "保存先のマスタ"
        ),
        action = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            field_item_id.name = "開閉のトリガーになるフィールドID",
            field_item_id.label = "開閉のトリガーになるラベル",
            codes = "開閉のトリガーになるコード",
            fields = "開閉するフィールドID",
            fields.label = "開閉するラベル"
        ),
        allocation = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            is_zelen = "Zelenの施設内バランス化",
            zelen_imbalance = "施設間の差",
            is_double_blinded = "二重盲検",
            double_blind_emails = "割付責任者メールアドレス",
            allocation_method = "割付方法",
            groups.if = "割付グループ.論理式",
            groups.if_references = "割付グループ.論理式の参照先情報",
            groups.code = "割付グループ.コード",
            groups.label = "割付グループ.ラベル",
            groups.message = "割付グループ.エラーメッセージ",
            formula_field = "調整因子フィールド.式",
            formula_field_references = "調整因子フィールド.式の参照先情報"
        ),
        comment = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            content = "フリーコメント"
        ),
        explanation = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            description = "説明"
        ),
        display = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル"
        ),
        presence = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル"
        ),
        title = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            level = "見出し"
        ),
        assigned = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            default_value = "デフォルト値"
        ),
        limitation = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            default_value = "デフォルト値",
            normal_range.less_than_or_equal_to = "アラート条件.超える場合",
            normal_range.greater_than_or_equal_to = "アラート条件.未満の場合",
            validators.numericality.validate_numericality_less_than_or_equal_to = "バリデータ.数値.最大値",
            validators.numericality.validate_numericality_greater_than_or_equal_to = "バリデータ.数値.最小値"
        ),
        date = c(
            jpname = "シート名",
            alias_name = kAliasNameJapaneseColumnName,
            name = "フィールドID",
            label = "ラベル",
            validators.date.validate_date_after_or_equal_to = "日付の最小値",
            references_after = "日付の最小値の参照先情報",
            validators.date.validate_date_before_or_equal_to = "日付の最大値",
            references_before = "日付の最大値の参照先情報"
        )
    )
    return(engToJpnColumnMappings)
}
convertSheetColumnsToJapanese <- function(output_checklist) {
    sheetNames <- names(output_checklist)
    engToJpnColumnMappings <- GetEngToJpnColumnMappings()
    res <- list()
    for (sheetName in sheetNames) {
        if (sheetName %in% names(engToJpnColumnMappings)) {
            if (sheetName == kVisit && is_visit) {
                df <- renameColumnsFromEnglishToJapanese(output_checklist[[sheetName]], engToJpnColumnMappings[["visit_to_visit"]])
            } else {
                df <- renameColumnsFromEnglishToJapanese(output_checklist[[sheetName]], engToJpnColumnMappings[[sheetName]])
            }
            res[[sheetName]] <- df
        } else if (sheetName == kItemVisit) {
            res[[sheetName]] <- output_checklist[[sheetName]]
        }
    }
    return(res)
}
