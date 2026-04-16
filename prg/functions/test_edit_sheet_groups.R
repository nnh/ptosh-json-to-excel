#' シートグループと来院情報の紐付け・クロス集計の編集
editSheetGroupsVisit <- function() {
  kDefaultName <- "default"
  kSheetsAliasName <- "sheets_alias_name"
  
  # 1. データ抽出とフラット化
  test_sheets <- flattenSheetsList(sheets)
  test_sheet_groups <- flattenSheetGroupList(json_files$sheet_groups) %>% 
    select(sheets_alias_name, allocation_sheet_alias)
  
  # 2. データの結合と正規化
  join_key <- setNames(kSheetsAliasName, .const[["kAliasName"]])
  
  test_sheet_groups_combined <- test_sheets %>% 
    left_join(test_sheet_groups, by = join_key, relationship = "many-to-many") %>% 
    mutate(
      arm_code = if_else(is.na(sg_allocation_group) | sg_allocation_group == "", 
                         kDefaultName, sg_allocation_group)
    )
  
  # 3. クロス集計の実行と結果の返却
  result <- createArmSheetCrossTab(test_sheet_groups_combined, kDefaultName)
  return(result)
}

#' sheet_groupsリストの全情報を保持したままtibbleに変換する
flattenSheetGroupList <- function(sheet_groups) {
  map_df(sheet_groups, function(x) {
    base_info <- tibble(
      uuid = x$uuid %||% NA_character_,
      name = x$name %||% NA_character_,
      alias_name = x$alias_name %||% NA_character_,
      allocation_group = x$allocation_group %||% NA_character_,
      is_default = x$is_default %||% NA,
      created_at = x$created_at %||% NA_character_,
      updated_at = x$updated_at %||% NA_character_
    )
    
    alloc_alias <- x$allocation_sheet$alias_name %||% NA_character_
    sheet_aliases <- if (!is.null(x$sheets)) map_chr(x$sheets, ~ .x$alias_name %||% NA_character_) else NA_character_
    
    base_info %>%
      mutate(allocation_sheet_alias = alloc_alias, sheets_alias_name = list(sheet_aliases)) %>%
      unnest(sheets_alias_name)
  })
}

#' sheetsリストの全情報を保持したままtibbleに変換する
flattenSheetsList <- function(sheets_list) {
  map_df(sheets_list, function(x) {
    base_info <- tibble(
      name = x$name %||% NA_character_,
      alias_name = x$alias_name %||% NA_character_,
      category = x$category %||% NA_character_,
      sort_order = x$sort_order %||% NA_real_
    )
    
    sg_raw <- x$sheet_groups
    sg_data <- if (is.data.frame(sg_raw)) {
      rename_with(sg_raw, ~ paste0("sg_", .))
    } else {
      tibble(sg_alias_name = NA_character_, sg_group_name = NA_character_, 
             sg_group_alias_name = NA_character_, sg_allocation_group = NA_character_, 
             sg_is_default = NA)
    }
    bind_cols(base_info, sg_data)
  })
}

#' クロス集計表の作成とヘッダー行の付与
createArmSheetCrossTab <- function(input_df, default_name) {
  kHeaderCategory <- "header"
  kDefaultJapaneseName <- "デフォルト"
  
  # --- 1. クロス集計 (Pivot) ---
  df <- input_df %>%
    distinct(alias_name, name, allocation_sheet_alias, sort_order, category, arm_code) %>%
    mutate(exists = "○") %>%
    pivot_wider(
      id_cols = c(sort_order, alias_name, name, category),
      names_from = c(allocation_sheet_alias, arm_code),
      names_sep = "|",
      values_from = exists,
      values_fill = "-"
    ) %>%
    arrange(sort_order)
  
  # --- 2. マスタ情報の準備 ---
  allocation_info <- sheet_info %>% 
    filter(category == .const[["kAllocation"]]) %>% 
    select(alias_name, sheet_name)
  
  group_info <- sheet_info %>% 
    select(allocation_group, group_name) %>% 
    distinct() %>%
    mutate(group_name = if_else(group_name == default_name | is.na(group_name), 
                                kDefaultJapaneseName, group_name))
  
  # --- 3. ヘッダー行の作成 (すべて文字列型で統一) ---
  header <- df %>% 
    mutate(across(everything(), as.character)) %>%
    slice(0) %>% 
    add_row() %>% # 1行目: 割り付けシート名
    add_row()    # 2行目: 腕名
  
  temp_colnames <- colnames(df)
  
  for (i in seq_along(temp_colnames)) {
    col_name <- temp_colnames[i]
    
    if (str_detect(col_name, "\\|")) {
      parts <- str_split(col_name, "\\|")[[1]]
      alloc_alias <- parts[1]
      arm_code <- parts[2]
      
      # 1行目 (Allocation)
      header[1, i] <- if (alloc_alias %in% c("NA", default_name)) kDefaultJapaneseName 
      else (allocation_info %>% filter(alias_name == alloc_alias) %>% pull(sheet_name) %>% .[1] %||% alloc_alias)
      
      # 2行目 (Arm)
      header[2, i] <- if (arm_code %in% c("NA", default_name)) kDefaultJapaneseName 
      else (group_info %>% filter(allocation_group == arm_code) %>% pull(group_name) %>% .[1] %||% arm_code)
      
    } else {
      # 固定列の日本語化
      header[1, i] <- case_when(
        col_name == .const[["kSortOrder"]] ~ "-999",
        col_name == .const[["kCategory"]] ~ kHeaderCategory,
        TRUE ~ NA_character_
      )
      header[2, i] <- case_when(
        col_name == .const[["kAliasName"]] ~ .const[["kAliasNameJapaneseColumnName"]],
        col_name == .const[["kSheetJapaneseName"]] ~ .const[["kSheetNameJapanese"]],
        col_name == .const[["kSortOrder"]] ~ "-888",
        col_name == .const[["kCategory"]] ~ kHeaderCategory,
        TRUE ~ col_name
      )
    }
  }
  
  # --- 4. 結合と最終整理 ---
  header_df <- bind_rows(header, mutate(df, across(everything(), as.character)))
  
  # Visit情報の結合
  visitnum_map <- visit_info %>% select(all_of(c(.const[["kAliasName"]], "visitnum")))
  
  # 分離ロジック
  is_visit_row <- header_df$category %in% c(.const[["kVisit"]], .const[["kAllocation"]], kHeaderCategory)
  
  visit <- header_df %>% 
    filter(is_visit_row) %>%
    left_join(visitnum_map, by = .const[["kAliasName"]]) %>%
    relocate(visitnum, everything()) %>%
    arrange(as.numeric(sort_order)) %>%
    select(-c(sort_order, category))
  
  nonvisit <- header_df %>% 
    filter(!is_visit_row) %>%
    arrange(as.numeric(sort_order)) %>%
    select(-c(sort_order, category))
  
  list(visit = visit, nonvisit = nonvisit)
}
