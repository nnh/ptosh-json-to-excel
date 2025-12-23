#' sort_sheets.R
#'
#' @file sort_sheets.R
#' @author Mariko Ohtsuka
#' @date 2025.12.19
SortSheetAndField <- function(df, sheet_sort_info, field_sort_info) {
    df %>%
        left_join(
            field_sort_info,
            by = setNames(
                c("alias_name", "name"),
                c(kAliasNameJapaneseColumnName, "フィールドID")
            )
        ) %>%
        arrange(sort_order, field_seq) %>%
        select(-sort_order, -field_seq)
}

SortSheetsMain <- function(output_checklist) {
    temp <- output_checklist
    sheet_sort_info <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    field_sort_info <- sheet_sort_info %>%
        left_join(field_list, by = "alias_name") %>%
        select(alias_name, sort_order, name, field_seq)
    # 各シートをソート
    # item_visit
    # item_visit作るとこでソートする

    # item_nonvisit
    target <- "item_nonvisit"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # limitation
    target <- "limitation"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # date
    target <- "date"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # option
    # field_itemsを再取得してソート、複数あれば最も小さいseqを採用

    # name
    sort_name <- output_checklist[["name"]] %>%
        left_join(
            sheet_sort_info,
            by = setNames("alias_name", kAliasNameJapaneseColumnName)
        ) %>%
        arrange(sort_order) %>%
        select(-sort_order)
    temp[["name"]] <- sort_name
    # master
    target <- "master"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # assigned
    target <- "assigned"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # シート出力順の変更
    res <- temp[kSortOrderSheetNames]
    return(res)
}
