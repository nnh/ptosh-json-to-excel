#' edit_item.R
#'
#' @file edit_item.R
#' @author Mariko Ohtsuka
#' @date 2026.1.6
EditItemVisit <- function(item_visit) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
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
    # 同一のラベルが複数存在する場合最も若い項目のみを有効として残す
    field_label_and_sort_order <- field_list %>%
        select(alias_name, label, field_seq)
    sheet_and_field_sort_order <- sheet_name_and_sort_order %>%
        left_join(field_label_and_sort_order, by = c("alias_name" = "alias_name")) %>%
        arrange(sort_order, field_seq)
    sheet_and_field_sort_order_len <- seq(length(sheet_and_field_sort_order$alias_name), 1)
    kLowValue <- -999
    for (i in sheet_and_field_sort_order_len) {
        target_label <- sheet_and_field_sort_order$label[i]
        temp <- sheet_and_field_sort_order %>%
            filter(label == target_label & field_seq > kLowValue)
        if (nrow(temp) > 1) {
            sheet_and_field_sort_order[i, "field_seq"] <- kLowValue
        }
    }
    # item_visit のラベル順に並び替え用のデータフレームを作成
    item_visit_rownames <- sheet_and_field_sort_order %>%
        filter(field_seq != kLowValue) %>%
        select("label")
    item_visit_rownames$seq <- seq(1, nrow(item_visit_rownames))
    # item_visit を visit_group ごとに集約する
    item_visit_by_group <- item_visit %>%
        inner_join(visit_info_for_item_visit, by = c("シート名英数字別名" = "alias_name")) %>%
        select(visit_group_name, ラベル, `数値チェック・アラート条件の有無`)
    label_count_by_sheet <- item_visit_by_group %>%
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
    # 列順の生成
    output_colname_order <- visit_info_for_item_visit %>%
        pull(visit_group_name) %>%
        as.vector() %>%
        c("label", ., "数値チェック・アラート条件の有無", "seq")

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
