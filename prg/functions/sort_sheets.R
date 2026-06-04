#' sort_sheets.R
#'
#' @file sort_sheets.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
SortSheetAndField <- function(df, sheet_sort_info, field_sort_info) {
    if (nrow(df) == 0) {
        return(df)
    }
    df %>%
        left_join(
            field_sort_info,
            by = c(.const[["kAliasName"]], .const[["kFieldItemsFieldId"]])
        ) %>%
        arrange(sort_order, field_seq) %>%
        select(-sort_order, -field_seq)
}
SortBySeqColumns <- function(target, sheet_data, extra_sort_cols = character(0)) {
    df <- sheet_data[[target]]
    if (nrow(df) == 0) {
        return(df)
    }
    df %>%
        arrange(sheet.seq, field_item.seq, !!!syms(extra_sort_cols)) %>%
        select(-sheet.seq, -field_item.seq) %>%
        distinct()
}

SortRowsMain <- function(sheet_data, sheet_info, field_list, visit_info) {
    temp <- sheet_data
    sheet_sort_info <- sheet_info %>%
        select(alias_name, sort_order) %>%
        distinct()
    field_sort_info <- sheet_sort_info %>%
        left_join(field_list, by = .const[["kAliasName"]]) %>%
        select(alias_name, sort_order, name, field_seq) %>%
        left_join(visit_info, by = .const[["kAliasName"]]) %>%
        mutate(!!.const[["kAliasName"]] := coalesce(visit_group, !!sym(.const[["kAliasName"]]))) %>%
        select(alias_name, name, sort_order, field_seq) %>%
        group_by(alias_name) %>%
        filter(sort_order == min(sort_order, na.rm = TRUE)) %>%
        ungroup()

    # 各シートの行順をソート（英語列名のまま処理）
    # item_nonvisit
    temp[[.const[["kItemNonVisit"]]]] <- SortBySeqColumns(.const[["kItemNonVisit"]], sheet_data)
    # option
    temp[[.const[["kOption"]]]] <- SortBySeqColumns(.const[["kOption"]], sheet_data, .const[["kOptionSeqColname"]])
    # name
    temp[[.const[["kName"]]]] <- sheet_data[[.const[["kName"]]]] %>%
        left_join(
            sheet_sort_info,
            by = .const[["kAliasName"]]
        ) %>%
        arrange(sort_order) %>%
        select(-sort_order)
    # limitation, date, master, assigned
    for (target in c(.const[["kLimitation"]], .const[["kDate"]], .const[["kMaster"]], .const[["kAssigned"]])) {
        temp[[target]] <- SortSheetAndField(sheet_data[[target]], sheet_sort_info, field_sort_info)
    }
    return(temp)
}
