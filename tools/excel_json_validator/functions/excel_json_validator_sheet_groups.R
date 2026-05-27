#' test script
#'
#' @file excel_json_validator_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.5.27
ValidateSheetGroup <- function(target_sheet_df, reference_table, type_label, header_rows = 2, trial_name) {
    # 1. 行数のチェック
    if (nrow(target_sheet_df) != nrow(reference_table) + header_rows) {
        stop(str_c("The number of sheets in sheet_groups_", type_label, " does not match the number of ", type_label, " sheets: ", trial_name))
    }
    
    # 2. 必要な列の選択とヘッダー行（2行）の削除
    cleaned_df <- target_sheet_df %>% 
        select(name, sheet_name) %>% 
        slice(-(1:header_rows)) # Rのモダンな書き方（[-(1:2), ] と同等）
    
    # 3. 中身の値が一致するかチェック
    if (!all(as.matrix(reference_table) == as.matrix(cleaned_df))) {
        stop(str_c("The name column in sheet_groups_", type_label, " does not match the name of ", type_label, " sheets: ", trial_name))
    }
}
ValidateAllocationCols <- function(sheet_group_df, reference_allocations, type_label, trial_name) {
    # 1. 1〜2行目の3列目以降を抽出して転置
    extracted_cols <- sheet_group_df[1:2, 3:ncol(sheet_group_df)] %>% 
        as.matrix() %>% 
        t() %>% 
        as.data.frame()
    
    # 2. 最初の「デフォルト」列のヘッダーチェック
    if (extracted_cols[1, 1] != "デフォルト" || extracted_cols[1, 2] != "デフォルト") {
        stop(str_c("The header of sheet_groups_", type_label, " is incorrect in trial: ", trial_name))
    }
    
    # 3. 1行目（デフォルトの行）を削除
    cleaned_cols <- extracted_cols[-1, , drop = FALSE]
    
    # 4. マスターデータ（sheet_group_allocations）と中身を比較
    if (!all(as.matrix(reference_allocations) == as.matrix(cleaned_cols))) {
        stop(str_c("The allocation groups in sheet_groups_", type_label, " do not match the allocation groups in sheets: ", trial_name))
    }
}
ValidateAllocationMarks <- function(sheet_group_df, reference_sheet_groups, type_label, trial_name, header_rows = 2) {
    # データ行が存在しない場合（ヘッダー行のみ）はチェック不要
    if (nrow(sheet_group_df) <= header_rows) return(invisible(NULL))
    for (row_idx in seq(header_rows + 1, nrow(sheet_group_df))) {
        target_alias_name <- sheet_group_df[[row_idx, 1]]
        
        # 1. 参照データから該当する alias_name の情報を取得
        matched_group_info_list <- reference_sheet_groups %>% filter(alias_name == target_alias_name)
        
        # 2. 元のシート（sheet_group_df）から該当する行のデータを取得
        target_row_data <- sheet_group_df %>% filter(name == target_alias_name)
        if (nrow(target_row_data) != 1) {
            stop(str_c("alias_name in sheet_groups_", type_label, " should be unique: ", target_alias_name, " in trial: ", trial_name))
        }
        
        # 3. チェックする列の基本方針を決定
        if (nrow(matched_group_info_list) == 0) {
            # JSON側に定義がない場合：強制的に「default」列を参照するターゲットにする
            target_allocation_column <- "default"
            # ループを1回だけ回すためにダミーの1行tibbleを作成
            matched_group_info_list <- tibble(allocation_group = "default")
        } else {
            # JSON側に定義がある場合：後続のループ内で列名を動的に取得するため、一旦初期化
            target_allocation_column <- "dynamic"
        }
        
        # 4. 各グループ（またはデフォルト）に対して「〇」のチェックを行う
        for (info_idx in 1:nrow(matched_group_info_list)) {
            
            # 「default」固定でない場合は、JSONから取得したグループコードを列名にする
            if (target_allocation_column != "default") {
                target_allocation_column <- matched_group_info_list[[info_idx, "allocation_group"]]
            }
            
            # 指定された列から値を取得
            mark_value <- target_row_data[[1, target_allocation_column]]
            
            # 列自体が存在しない、または値がNULL/NAの場合は「default」列にフォールバック
            if (is.null(mark_value) || length(mark_value) == 0 || is.na(mark_value)) {
                mark_value <- target_row_data[[1, "default"]]
            }
            
            # それでも値が取得できない場合はエラー
            if (is.null(mark_value) || length(mark_value) == 0 || is.na(mark_value)) {
                stop(str_c("Allocation column '", target_allocation_column, "' or 'default' in sheet_groups_", type_label, " is not found: ", target_alias_name, " in trial: ", trial_name))
            }
            
            # 「〇」がついているかチェック
            if (mark_value != "〇") {
                stop(str_c("Allocation column [", target_allocation_column, "] in sheet_groups_", type_label, " is not marked with 〇: ", target_alias_name, " in trial: ", trial_name))
            }
        }
    }
}
CheckSheetGroups <- function(target_json, sheetList, trialName) {
    sheet_group_list <- target_json[["sheet_groups"]]
    sheet_group_visit_sheet <- sheetList[["sheet_groups_visit"]]
    sheet_group_nonvisit_sheet <- sheetList[["sheet_groups_nonvisit"]]
    visit_status_table <- target_json[["sheets"]] %>%
        map_df(~ tibble(
            alias_name = .x$alias_name,
            name       = .x$name,
            is_visit   = .x$category %in% c("visit", "allocation")
        ))
    visit_table <- visit_status_table %>% filter(is_visit) %>% select(-is_visit)
    non_visit_table <- visit_status_table %>% filter(!is_visit) %>% select(-is_visit)
    # 行名の一致を確認する
    kHeaderRowCount <- 2
    ValidateSheetGroup(
        target_sheet_df = sheet_group_visit_sheet,
        reference_table = visit_table,
        type_label      = "visit",
        header_rows     = kHeaderRowCount,
        trial_name      = trialName
    )

    ValidateSheetGroup(
        target_sheet_df = sheet_group_nonvisit_sheet,
        reference_table = non_visit_table,
        type_label      = "nonvisit",
        header_rows     = kHeaderRowCount,
        trial_name      = trialName
    )
    # 列名の一致を確認する
    sheet_group_allocations <- target_json[["sheets"]] %>%
        keep(~ !is.null(.x[["allocation"]])) %>%
        map_df(function(current_sheet) {
            current_sheet[["allocation"]][["groups"]] %>%
                map_df(function(current_group) {
                    matched_groups <- sheet_group_list %>%
                        keep(~ .x$allocation_group == current_group[["code"]])
                    group_label <- if (length(matched_groups) > 0) {
                        matched_groups[[1]][["name"]]
                    } else {
                        current_group[["label"]]
                    }
                    tibble(name = current_sheet[["name"]], group_label = group_label)
                })
        })
    ValidateAllocationCols(
        sheet_group_df        = sheet_group_visit_sheet,
        reference_allocations = sheet_group_allocations,
        type_label            = "visit",
        trial_name            = trialName
    )

    ValidateAllocationCols(
        sheet_group_df        = sheet_group_nonvisit_sheet,
        reference_allocations = sheet_group_allocations,
        type_label            = "nonvisit",
        trial_name            = trialName
    )   
    
    # 最終的な結果を蓄積するtibble
    accumulated_sheet_groups <- tibble(
        name = character(),
        allocation_group = character(),
        alias_name = character(),
        allocation_sheet_alias_name = character()
    )

    for (current_group in sheet_group_list) {
        # allocation_sheetの要素数が1より大きい場合はエラーを出す
        if (length(current_group[["allocation_sheet"]]) > 1) {
            stop(str_c("allocation_sheet should have only one element in trial: ", trialName))
        }
        
        if (length(current_group[["allocation_sheet"]]) == 0) {
            target_sheet_alias <- ""
        } else {
            target_sheet_alias <- current_group[["allocation_sheet"]][["alias_name"]]
        }
        
        temp_sheet_group <- current_group[["sheets"]] %>%
            map_df(~ tibble(
                name                        = current_group[["name"]],
                allocation_group            = current_group[["allocation_group"]],
                alias_name                  = .x[["alias_name"]],
                allocation_sheet_alias_name = target_sheet_alias
            ))
        
        # 一時データを全体の結果に結合
        accumulated_sheet_groups <- bind_rows(accumulated_sheet_groups, temp_sheet_group)
    }
    ValidateAllocationMarks(
        sheet_group_df         = sheet_group_visit_sheet,
        reference_sheet_groups = accumulated_sheet_groups,
        type_label             = "visit",
        trial_name             = trialName,
        header_rows            = kHeaderRowCount
    )
    ValidateAllocationMarks(
        sheet_group_df         = sheet_group_nonvisit_sheet,
        reference_sheet_groups = accumulated_sheet_groups,
        type_label             = "nonvisit",
        trial_name             = trialName,
        header_rows            = kHeaderRowCount
    )    
    return(TRUE)
}