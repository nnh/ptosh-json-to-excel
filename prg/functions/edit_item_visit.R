#' edit_item_visit.R
#'
#' @file edit_item.R
#' @author Mariko Ohtsuka
#' @date 2026.1.6
EditItemVisit <- function(item_visit) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
    # シートソート順の取得
    sheet_name_and_sort_order <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    # 同一 visit_group 内で最も若い visit を代表として採用する
    visit_info_for_item_visit <- visit_info %>%
        select(alias_name, visit_group, visit_group_name) %>%
        inner_join(
            sheet_name_and_sort_order,
            by = c("alias_name" = "alias_name")
        ) %>%
        arrange(visit_group, sort_order) %>%
        group_by(visit_group) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        arrange(sort_order)
    # 出力列順の取得
    target_colnames_visits <- visit_info_for_item_visit %>%
        pull(visit_group_name) %>%
        as.vector()
    output_colname_order <- target_colnames_visits %>%
        c("label", ., "数値チェック・アラート条件の有無", "seq")
    # visit_group毎にレコードをまとめる
    target_item_visit <- item_visit %>%
        inner_join(
            visit_info_for_item_visit,
            by = c("シート名英数字別名" = "alias_name")
        ) %>%
        select(visit_group_name, ラベル, `数値チェック・アラート条件の有無`)
    target_item_visit_distinct <- target_item_visit %>%
        distinct(.keep_all = TRUE)
    # 出力行順の取得
    # 同一のラベルが複数存在する場合最も若い項目のみを有効として残す
    target_field_items <- visit_info_for_item_visit %>%
        inner_join(
            field_list,
            by = c("alias_name" = "alias_name")
        ) %>%
        select(visit_group_name, label, sort_order, field_seq) %>%
        inner_join(
            target_item_visit_distinct,
            by = c("visit_group_name" = "visit_group_name", "label" = "ラベル")
        )
    item_visit_rownames <- target_field_items %>%
        arrange(sort_order, field_seq) %>%
        distinct(label, .keep_all = TRUE) %>%
        select("label")
    item_visit_rownames$seq <- seq(1, nrow(item_visit_rownames))
    # ラベル個数のカウント
    label_count_by_sheet <- target_item_visit %>%
        group_by(visit_group_name, ラベル, `数値チェック・アラート条件の有無`) %>%
        summarise(
            ラベルの個数 = n(),
            .groups = "drop"
        )
    # ピボット操作で visit_group_name ごとに列を分ける
    label_count_wide <- label_count_by_sheet %>%
        pivot_wider(
            names_from = visit_group_name,
            values_from = ラベルの個数,
            values_fill = list(ラベルの個数 = 0)
        )
    output_item_visit <- item_visit_rownames %>%
        inner_join(
            label_count_wide,
            by = c("label" = "ラベル")
        ) %>%
        select(all_of(output_colname_order)) %>%
        arrange(seq)
    output_item_visit$seq <- NULL
    return(output_item_visit)
}
