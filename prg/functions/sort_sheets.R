#' sort_sheets.R
#'
#' @file sort_sheets.R
#' @author Mariko Ohtsuka
#' @date 2026.1.7
SortSheetAndField <- function(df, sheet_sort_info, field_sort_info) {
    if (nrow(df) == 0) {
        return(df)
    }
    res <- df %>%
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
SortSheetAndFieldByDfValues <- function(target, output_checklist) {
    res <- output_checklist[[target]] %>%
        arrange(sheet.seq, field_item.seq) %>%
        select(-sheet.seq, -field_item.seq) %>%
        distinct()
    return(res)
}

SortSheetsMain <- function(output_checklist) {
    temp <- output_checklist
    sheet_sort_info <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    temp_field_sort_info <- sheet_sort_info %>%
        left_join(field_list, by = "alias_name") %>%
        select(alias_name, sort_order, name, field_seq)
    field_sort_info <- temp_field_sort_info %>%
        left_join(visit_info, by = "alias_name")
    field_sort_info[["alias_name"]] <- ifelse(!is.na(field_sort_info[["visit_group"]]), field_sort_info[["visit_group"]], field_sort_info[["alias_name"]])
    field_sort_info <- field_sort_info %>%
        select(alias_name, name, sort_order, field_seq)
    field_sort_info <- field_sort_info %>%
        group_by(alias_name) %>%
        filter(sort_order == min(sort_order, na.rm = TRUE)) %>%
        ungroup()

    # 各シートをソート
    # item_nonvisit
    target <- "item_nonvisit"
    temp[[target]] <- SortSheetAndFieldByDfValues(target, output_checklist)
    # limitation
    target <- "limitation"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # date
    target <- "date"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # option
    target <- "option"
    temp[[target]] <- SortSheetAndFieldByDfValues(target, output_checklist)
    # visit
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
