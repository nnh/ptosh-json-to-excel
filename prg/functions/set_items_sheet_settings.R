# 列番号をExcel列文字に変換するヘルパー関数
int2colForSetColumnConditionalFormatting <- function(n) {
    openxlsx::int2col(n)
}
setColumnConditionalFormatting <- function(wb, sheetName, targetColName, rows) {
    # 1行目のヘッダを読み込む
    col_names <- names(readWorkbook(wb, sheet = sheetName, rows = 1))

    # 列番号を取得
    target_col <- which(col_names == targetColName)

    if (length(target_col) == 0) {
        stop(paste0("シートに '", targetColName, "' 列が見つかりません。"))
    }
    cols_to_format <- 1:which(col_names == targetColName)
    # 条件付き書式の基準行（データ開始行）
    start_row <- min(rows)
    col_letter <- int2colForSetColumnConditionalFormatting(target_col)
    # スタイルを定義
    style_noCondition <- createStyle(
        fontName = "Meiryo",
        fontSize = 11,
        fontColour = "#000000",
        bgFill = "#ffffff"
    )
    style_numericality <- createStyle(
        fontName = "Meiryo",
        fontSize = 11,
        fontColour = "#000000",
        bgFill = "#ffffe0"
    )
    style_normal_range <- createStyle(
        fontName = "Meiryo",
        fontSize = 11,
        fontColour = "#000000",
        bgFill = "#d1ffd1"
    )
    style_numericality_normal_range <- createStyle(
        fontName = "Meiryo",
        fontSize = 11,
        fontColour = "#000000",
        bgFill = "#d1e8ff"
    )
    # 条件付き書式を設定
    conditionalFormatting(
        wb,
        sheet = sheetName,
        cols = cols_to_format, rows = rows,
        rule = paste0("$", col_letter, start_row, '="', .const[["kCheckNone"]], '"'),
        style = style_noCondition
    )

    conditionalFormatting(
        wb,
        sheet = sheetName,
        cols = cols_to_format, rows = rows,
        rule = paste0("$", col_letter, start_row, '="', .const[["kCheckNumericality"]], '"'),
        style = style_numericality
    )

    conditionalFormatting(
        wb,
        sheet = sheetName,
        cols = cols_to_format, rows = rows,
        rule = paste0("$", col_letter, start_row, '="', .const[["kCheckNormalRange"]], '"'),
        style = style_normal_range
    )

    conditionalFormatting(
        wb,
        sheet = sheetName,
        cols = cols_to_format, rows = rows,
        rule = paste0("$", col_letter, start_row, '="', .const[["kCheckBoth"]], '"'),
        style = style_numericality_normal_range
    )

    invisible(wb)
}
