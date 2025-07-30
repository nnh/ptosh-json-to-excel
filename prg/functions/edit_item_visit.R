GetGroupBySheetNames <- function(targetColumn) {
    return(str_remove(targetColumn, "\\([^\\)]+\\)$"))
}
EditItemVisit <- function(item_visit) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
    item_visit_by_group <- item_visit %>%
        mutate(group = GetGroupBySheetNames(.data[["シート名"]]))

    label_count_by_sheet <- item_visit_by_group %>%
        group_by(group, ラベル, `数値チェック・アラート条件の有無`) %>%
        summarise(
            ラベルの個数 = n(),
            .groups = "drop"
        )
    label_count_wide <- label_count_by_sheet %>%
        pivot_wider(
            names_from = group,
            values_from = ラベルの個数,
            values_fill = list(ラベルの個数 = 0)
        )
    res <- label_count_wide %>%
        relocate(`数値チェック・アラート条件の有無`, .after = last_col())
    return(res)
}
