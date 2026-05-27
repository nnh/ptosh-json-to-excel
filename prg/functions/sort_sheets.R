#' sort_sheets.R
#'
#' @file sort_sheets.R
#' @author Mariko Ohtsuka
#' @date 2026.5.27
SortSheetAndField <- function(df, sheet_sort_info, field_sort_info) {
    if (nrow(df) == 0) {
        return(df)
    }
    res <- df %>%
        left_join(
            field_sort_info,
            by = setNames(
                c(.const[["kAliasName"]], "name"),
                c(.const[["kAliasNameJapaneseColumnName"]], "フィールドID")
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
SortOptionSheet <- function(target, output_checklist) {
    optionSeqColname <- "-"
    res <- output_checklist[[target]] %>%
        arrange(sheet.seq, field_item.seq, !!sym(optionSeqColname)) %>%
        select(-sheet.seq, -field_item.seq) %>%
        distinct()
    return(res)
}

SortSheetsMain <- function(output_checklist, sheet_info, field_list, visit_info) {
    temp <- output_checklist
    sheet_sort_info <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    temp_field_sort_info <- sheet_sort_info %>%
        left_join(field_list, by = .const[["kAliasName"]]) %>%
        select(alias_name, sort_order, name, field_seq)
    field_sort_info <- temp_field_sort_info %>%
        left_join(visit_info, by = .const[["kAliasName"]])
    field_sort_info[[.const[["kAliasName"]]]] <- ifelse(!is.na(field_sort_info[["visit_group"]]), field_sort_info[["visit_group"]], field_sort_info[[.const[["kAliasName"]]]])
    field_sort_info <- field_sort_info %>%
        select(alias_name, name, sort_order, field_seq)
    field_sort_info <- field_sort_info %>%
        group_by(alias_name) %>%
        filter(sort_order == min(sort_order, na.rm = TRUE)) %>%
        ungroup()

    # 各シートをソート
    # item_nonvisit
    target <- .const[["kItemNonVisit"]]
    temp[[target]] <- SortSheetAndFieldByDfValues(target, output_checklist)
    # limitation
    target <- "limitation"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # date
    target <- "date"
    temp[[target]] <- output_checklist[[target]] %>% SortSheetAndField(., sheet_sort_info, field_sort_info)
    # option
    target <- "option"
    temp[[target]] <- SortOptionSheet(target, output_checklist)
    # visit
    # name
    sort_name <- output_checklist[["name"]] %>%
        left_join(
            sheet_sort_info,
            by = setNames(.const[["kAliasName"]], .const[["kAliasNameJapaneseColumnName"]])
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
    res <- temp[.const[["kSortOrderSheetNames"]]]
    return(res)
}
