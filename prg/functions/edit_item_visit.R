#' edit_item_visit.R
#'
#' @file edit_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2026.5.27
EditItemVisit <- function(item_visit, field_list, visit_info, sheet_info) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
    temp_itemVisitSeq <- "seq"
    kLabelCountColname <- "ラベルの個数"
    # シートソート順の取得
    sheet_name_and_sort_order <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    # 同一 visit_group 内で最も若い visit を代表として採用する
    visit_info_for_item_visit <- visit_info %>%
        select(alias_name, visit_group, visit_group_name) %>%
        inner_join(
            sheet_name_and_sort_order,
            by = .const[["kAliasName"]]
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
        c("label", ., .const[["kItemVisitConditionalFormattingColumnName"]], temp_itemVisitSeq)
    # visit_group毎にレコードをまとめる
    target_item_visit <- item_visit %>%
        inner_join(
            visit_info_for_item_visit,
            by = setNames(.const[["kAliasName"]], .const[["kAliasNameJapaneseColumnName"]])
        ) %>%
        select(
            visit_group_name,
            all_of(.const[["kLabelJapaneseColumnName"]]),
            all_of(.const[["kItemVisitConditionalFormattingColumnName"]])
        )
    target_item_visit_distinct <- target_item_visit %>%
        distinct(.keep_all = TRUE)
    # 出力行順の取得
    # 同一のラベルが複数存在する場合最も若い項目のみを有効として残す
    target_field_items <- visit_info_for_item_visit %>%
        inner_join(
            field_list,
            by = .const[["kAliasName"]]
        ) %>%
        select(visit_group_name, label, sort_order, field_seq) %>%
        inner_join(
            target_item_visit_distinct,
            by = c("visit_group_name" = "visit_group_name",
                   setNames(.const[["kLabelJapaneseColumnName"]], "label"))
        )
    item_visit_rownames <- target_field_items %>%
        arrange(sort_order, field_seq) %>%
        distinct(label, .keep_all = TRUE) %>%
        select("label")
    item_visit_rownames[[temp_itemVisitSeq]] <- seq(1, nrow(item_visit_rownames))
    # ラベル個数のカウント
    label_count_by_sheet <- target_item_visit %>%
        group_by(
            visit_group_name,
            !!sym(.const[["kLabelJapaneseColumnName"]]),
            !!sym(.const[["kItemVisitConditionalFormattingColumnName"]])
        ) %>%
        summarise(
            !!sym(kLabelCountColname) := n(),
            .groups = "drop"
        )
    # ピボット操作で visit_group_name ごとに列を分ける
    label_count_wide <- label_count_by_sheet %>%
        pivot_wider(
            names_from = visit_group_name,
            values_from = all_of(kLabelCountColname),
            values_fill = setNames(list(0), kLabelCountColname)
        )
    output_item_visit <- item_visit_rownames %>%
        inner_join(
            label_count_wide,
            by = setNames(.const[["kLabelJapaneseColumnName"]], "label")
        ) %>%
        select(all_of(output_colname_order)) %>%
        arrange(temp_itemVisitSeq)
    output_item_visit[[temp_itemVisitSeq]] <- NULL
    return(output_item_visit)
}
