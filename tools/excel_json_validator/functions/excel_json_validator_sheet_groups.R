#' test script
#'
#' @file excel_json_validator_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.4.17
CheckSheetGroups <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]]
    json <- targetJson
#    json <- GetOptionFromJson(fieldItems)

#    return(CheckTarget(sheet, json))
}
GetSheetGroupsRownames <- function(df) {
    sheet_groups_visit_rownames <- df %>% 
        select(seq, visit_num, sheet_groups_sheets_alias_name, sheet_groups_sheets_jpname) %>% rename(alias_name = sheet_groups_sheets_alias_name, jpname = sheet_groups_sheets_jpname) %>% arrange(seq)
    return(sheet_groups_visit_rownames)
} 
GetSheetGroupsFromJson <- function() {
    json_sheet_groups <- target_json[["sheet_groups"]]
    json_alias_name_and_category <- target_json[["sheets"]] %>% map(~ tibble(alias_name = .x[["alias_name"]], category = .x[["category"]])) %>% bind_rows()
    sheet_groups <- tibble()
    for (i in seq_along(json_sheet_groups)) {
        name <- json_sheet_groups[[i]][["name"]]
        alias_name <- json_sheet_groups[[i]][["alias_name"]]
        allocation_group <- json_sheet_groups[[i]][["allocation_group"]]
        is_default <- json_sheet_groups[[i]][["is_default"]]
        sheet_groups_sheets <- json_sheet_groups[[i]][["sheets"]]
        allocation_sheet <- pluck(json_sheet_groups, i, "allocation_sheet")
        # allocation_sheetが存在する場合リストから alias_name を抽出して1列の tibble に変換
        if (!is.null(allocation_sheet)) {
            allocation_sheet_df <- allocation_sheet %>% as_tibble() %>% rename(allocation_sheet_alias_name = alias_name)
            allocation_sheet_df <- allocation_sheet_df %>% left_join(jpNameAndAliasName, by = c("allocation_sheet_alias_name" = "alias_name")) %>%
                rename(allocation_sheet_jpname = jpname) 
        } else {
            allocation_sheet_df <- tibble(allocation_sheet_alias_name = NA_character_, allocation_sheet_jpname = NA_character_)
        }
        # リストから alias_name を抽出して1列の tibble に変換
        sheet_groups_sheets_df <- sheet_groups_sheets %>%
        # 各要素から alias_name を文字列として取り出す
            map_chr(~ .x$alias_name %||% NA_character_) %>%
        # 指定された列名で tibble を作成
            tibble(sheet_groups_sheets_alias_name = .)
        sheet_groups_sheets_df <- sheet_groups_sheets_df %>%
            mutate(allocation_group_name = alias_name, allocation_group_jpname = name, allocation_group = allocation_group, is_default = is_default)
        sheet_groups_sheets_df_2 <- sheet_groups_sheets_df %>% left_join(jpNameAndAliasName, by = c("sheet_groups_sheets_alias_name" = "alias_name")) %>%
            rename(sheet_groups_sheets_jpname = jpname)
        sheet_groups_sheets_df_3 <- sheet_groups_sheets_df_2 %>% left_join(sheetOrders, by = c("sheet_groups_sheets_alias_name" = "sheet"))
        sheet_groups_sheets_df_4 <- sheet_groups_sheets_df_3 %>% left_join(visitGroups %>% select(c(alias_name, visit_num)), by = c("sheet_groups_sheets_alias_name" = "alias_name")) 
        sheet_groups_sheets_df_5 <- sheet_groups_sheets_df_4 %>% left_join(json_alias_name_and_category, by = c("sheet_groups_sheets_alias_name" = "alias_name"))
        sheet_groups_sheets_df_6 <- sheet_groups_sheets_df_5 %>% bind_cols(allocation_sheet_df)
        sheet_groups <- bind_rows(sheet_groups, sheet_groups_sheets_df_6)
    }
    sheet_groups <- sheet_groups %>% arrange(seq)
    sheet_groups_visit <- sheet_groups %>% 
        filter(category == "visit" | category == "allocation") %>% select(c(sheet_groups_sheets_alias_name, allocation_group_jpname, allocation_sheet_jpname))
    sheet_groups_nonvisit <- sheet_groups %>% filter(category != "visit" & category != "allocation")
    #  横軸は共通
    sheet_groups_colnames_alias_name <- sheet_groups %>% select(allocation_sheet_jpname, allocation_group_name) %>% distinct()
    sheet_groups_colnames_allocation_group <- sheet_groups %>% select(seq, allocation_group_name, allocation_group_jpname) %>% distinct() %>% arrange(seq)
    # seqが一番小さい行を残す
    sheet_groups_colnames_allocation_group <- sheet_groups_colnames_allocation_group %>% group_by(allocation_group_name) %>% slice_min(seq) %>% ungroup() %>% arrange(seq)
    sheet_groups_colnames <- sheet_groups_colnames_alias_name %>% inner_join(sheet_groups_colnames_allocation_group, by = "allocation_group_name")
    sheet_groups_colnames <- sheet_groups_colnames %>% select(allocation_sheet_jpname, allocation_group_jpname) %>% rename(allocation_sheet = allocation_sheet_jpname, allocation_group = allocation_group_jpname)
    sheet_groups_colnames_allocation_sheet <- sheet_groups_colnames$allocation_sheet
    sheet_groups_colnames_allocation_group <- sheet_groups_colnames$allocation_group
    sheet_groups_header <- tibble()
    for (i in seq_along(sheet_groups_colnames_allocation_sheet)) {
        allocation_sheet <- sheet_groups_colnames_allocation_sheet[i]
        allocation_group <- sheet_groups_colnames_allocation_group[i]
        sheet_groups_header[1, i] <- allocation_sheet
        sheet_groups_header[2, i] <- allocation_group   
    }
    #  縦軸
    sheet_groups_visit_rownames <- GetSheetGroupsRownames(sheet_groups_visit)
    sheet_groups_nonvisit_rownames <- GetSheetGroupsRownames(sheet_groups_nonvisit)
    # visit
    res_visit <- as_tibble(matrix("-", 
                              nrow = nrow(sheet_groups_visit_rownames), 
                              ncol = ncol(sheet_groups_header)))
    for (row in 1:nrow(sheet_groups_visit_rownames)) {
        target_alias_name <- sheet_groups_visit_rownames$alias_name[row]
        target_rows <- sheet_groups_visit %>% filter(sheet_groups_sheets_alias_name == target_alias_name)
    
        if (nrow(target_rows) == 0) next
    
        for (col in 1:ncol(sheet_groups_header)) {
            target_allocation_sheet_name <- sheet_groups_header[1, col, drop=T]
            target_group <- sheet_groups_header[2, col, drop=T]
        
            # 判定
            is_match <- any(
              target_rows$allocation_group_jpname %in% target_group & 
                target_rows$allocation_sheet_jpname %in% target_allocation_sheet_name
            )
        
            if (is_match) {
                res_visit[row, col] <- "○"
            }
        }
    }

    # 最後に rownames と結合
    final_res_visit <- bind_cols(sheet_groups_visit_rownames, res_visit)
}
GetSheetGroupsFromJson()
