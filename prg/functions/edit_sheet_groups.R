#' edit_sheet_groups.R
#' シートグループの編集とクロス集計表の作成
#'
#' @file edit_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.5.26
EditSheetGroups <- function(sheets, json_files, sheet_info) {
  sheet_orders <- sheet_info %>% 
    select(name=alias_name, seq=sort_order) %>% 
    distinct()

  sheet_group_mappings <- json_files[["sheet_groups"]] %>% 
    map_df(function(sg) {
      alloc_group <- sg[["allocation_group"]] %||% NA_character_
      
      alloc_sheets <- sg[["allocation_sheet"]]
      alloc_aliases <- if (is.null(alloc_sheets)) {
        NA_character_
      } else if (!is.null(alloc_sheets[["alias_name"]])) {
        alloc_sheets[["alias_name"]]
      } else {
        map_chr(alloc_sheets, ~ if(is.list(.x)) .x[["alias_name"]] %||% NA_character_ else as.character(.x))
      }
      
      sheets_list <- sg[["sheets"]]
      sheet_aliases <- if (is.null(sheets_list)) {
        NA_character_
      } else {
        map_chr(sheets_list, ~ if(is.list(.x)) .x[["alias_name"]] %||% NA_character_ else as.character(.x))
      }
      
      expand_grid(
        sheet_alias_name = sheet_aliases,
        allocation_group = alloc_group,
        alias_name = alloc_aliases
      )
    }) %>% 
    distinct()

  sheet_group_allocations <- json_files[["sheet_groups"]] %>% 
    keep(~ !is.null(.x[["allocation_group"]]) && !is.null(.x[["allocation_sheet"]])) %>% 
    map_df(function(sg) {
      alloc_sheets <- sg[["allocation_sheet"]]
      alloc_aliases <- if (!is.null(alloc_sheets[["alias_name"]])) {
        alloc_sheets[["alias_name"]] 
      } else {
        map_chr(alloc_sheets, ~ if(is.list(.x)) .x[["alias_name"]] %||% NA_character_ else as.character(.x))
      }
      
      tibble(
        alias_name = alloc_aliases,
        sheet_group_name = sg[["name"]] %||% NA_character_,
        group_code = sg[["allocation_group"]] %||% NA_character_
      )
    }) %>% 
    distinct()


  allocation_group_master <- json_files[["sheets"]] %>% 
    keep(~ !is.null(.x[["allocation"]][["groups"]])) %>% 
    map_df(function(s) {
      s_name <- s[["name"]] %||% NA_character_
      alias_name <- s[["alias_name"]] %||% NA_character_
      
      s[["allocation"]][["groups"]] %>% 
        map_df(~ tibble(
          sheet_name = s_name,
          alias_name = alias_name,  
          group_code = .x[["code"]] %||% NA_character_,
          group_label = .x[["label"]] %||% NA_character_
        ))
    }) %>% 
    distinct()

  column_information <- allocation_group_master %>%
    left_join(sheet_group_allocations, by = c("alias_name", "group_code")) %>%
    select(sheet_name, sheet_alias_name = alias_name, sheet_group_name, group_code, group_label) %>%
    mutate(sheet_group_name = if_else(is.na(sheet_group_name), group_label, sheet_group_name)) %>%
    select(-group_label) %>%
    distinct()

  resolved_mappings <- sheet_group_mappings %>% 
    left_join(column_information, by = c("alias_name" = "sheet_alias_name", "allocation_group" = "group_code")) 

  ordered_sheet_groups <- sheet_orders %>% 
    left_join(resolved_mappings, by = c("name" = "sheet_alias_name"))

  unique_columns <- column_information %>% 
    distinct(sheet_name, sheet_alias_name, sheet_group_name, group_code)
  
  match_data <- ordered_sheet_groups %>% 
    mutate(column_info_key = if_else(
      is.na(alias_name), 
      "default", 
      paste(alias_name, allocation_group, sep = "___")
    )) %>% 
    select(name, column_info_key) %>% 
    distinct() %>% 
    mutate(flag = "〇")
  
  expected_columns <- c(
    "default",
    paste(unique_columns$sheet_alias_name, unique_columns$group_code, sep = "___")
  )
  
  grid_base <- expand_grid(
    name = unique(ordered_sheet_groups$name),
    column_info_key = expected_columns
  )
  
  pivoted_body <- grid_base %>% 
    left_join(match_data, by = c("name", "column_info_key")) %>% 
    mutate(flag = if_else(is.na(flag), "-", flag)) %>% 
    pivot_wider(names_from = column_info_key, values_from = flag)
  
  alias_master <- json_files[["sheets"]] %>% 
    map_df(~ tibble(
      name_key = .x[["alias_name"]] %||% NA_character_,
      japanese_name = .x[["name"]] %||% NA_character_,
      category = .x[["category"]] %||% NA_character_
    )) %>% 
    distinct()
  
  alias_to_properties_master <- alias_master %>% 
    mutate(is_visit_or_allocation = if_else(category %in% c("visit", "allocation"), "T", "F")) %>% 
    mutate(is_visit_or_allocation = if_else(is.na(is_visit_or_allocation), "F", is_visit_or_allocation)) %>% 
    select(name_key, japanese_name, is_visit_or_allocation)
  
  order_seq_master <- sheet_orders %>% 
    select(name, seq) %>% 
    distinct() %>% 
    mutate(seq = as.character(seq))
  
  matrix_body <- pivoted_body %>% 
    left_join(alias_to_properties_master, by = c("name" = "name_key")) %>% 
    rename(sheet_name = japanese_name) %>% 
    mutate(sheet_name = if_else(is.na(sheet_name), "デフォルト", sheet_name)) %>% 
    mutate(is_visit_or_allocation = if_else(is.na(is_visit_or_allocation), "F", is_visit_or_allocation)) %>% 
    left_join(order_seq_master, by = "name") %>% 
    select(seq, name, sheet_name, is_visit_or_allocation, everything())
  
  header_2 <- c("seq", "name", "sheet_name", "is_visit_or_allocation", "default", unique_columns$group_code)
  header_3 <- c("seq", "name", "sheet_name", "is_visit_or_allocation", "デフォルト", unique_columns$sheet_name)
  header_4 <- c("seq", "name", "sheet_name", "is_visit_or_allocation", "デフォルト", unique_columns$sheet_group_name)
  
  dummy_colnames <- paste0("V", 1:ncol(matrix_body))
  colnames(matrix_body) <- dummy_colnames
  
  names(header_3) <- dummy_colnames
  names(header_4) <- dummy_colnames
  
  cross_tab <- bind_rows(
    as_tibble_row(header_3),
    as_tibble_row(header_4),
    matrix_body
  )
  
  colnames(cross_tab) <- header_2
  cross_tab[1, 1] <- "-999"
  cross_tab[2, 1] <- "-888"
  cross_tab[1:2, 2:3] <- ""
  cross_tab[2, 2] <- "シート名英数字別名"
  cross_tab[2, 3] <- "シート名"

  return(cross_tab)
}
#' split_cross_tab.R
#'
#' @param cross_tab 新しい仕様の edit_sheet_groups() の戻り値 (ヘッダー2行構造)
#' @return visit_cross_tab と non_visit_cross_tab を格納したリスト
SplitCrossTab <- function(cross_tab) {
  
  # --- 1. ヘッダー行（最初の2行）とデータ行の分離 ---
  header_rows <- cross_tab %>% slice(1:2)
  data_rows   <- cross_tab %>% slice(3:n())
  
  # --- 2. データ行の仕分けとソート ---
  # 列名が可変なため、インデックス（位置）で安全に処理します。
  # 1列目 = seq, 4列目 = is_visit_or_allocation
  
  # visit / allocation 対象のデータ行
  visit_data <- data_rows %>% 
    filter(.[[4]] == "T") %>%            
    arrange(as.numeric(.[[1]]))          
  
  # それ以外のデータ行
  non_visit_data <- data_rows %>% 
    filter(.[[4]] == "F") %>%            
    arrange(as.numeric(.[[1]]))          
  
  # --- 3. ヘッダーとソート済データを再結合 ---
  visit_cross_tab     <- bind_rows(header_rows, visit_data)
  non_visit_cross_tab <- bind_rows(header_rows, non_visit_data)
  
  # --- 4. 不要な列（is_visit_or_allocation と seq）の削除 ---
  # 1列目(seq) と 4列目(is_visit_or_allocation) を除外
  visit_cross_tab     <- visit_cross_tab     %>% select(-1, -4)
  non_visit_cross_tab <- non_visit_cross_tab %>% select(-1, -4)
  
  # 元々の列名（header_2 の内容）から、削除した1列目と4列目を除いて再設定
  colnames(visit_cross_tab)     <- colnames(header_rows)[c(-1, -4)]
  colnames(non_visit_cross_tab) <- colnames(header_rows)[c(-1, -4)]
  
  # --- 5. 成果物をリストにして返却 ---
  return(list(
    visit_cross_tab     = visit_cross_tab,
    non_visit_cross_tab = non_visit_cross_tab
  ))
}

EditSheetGroupsMain <- function(sheets, json_files, sheet_info) {
  sheet_groups <- EditSheetGroups(sheets, json_files, sheet_info)
  sheet_groups_table <- SplitCrossTab(sheet_groups)
  return(sheet_groups_table)
}