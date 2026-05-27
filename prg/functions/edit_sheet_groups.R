#' edit_sheet_groups.R
#' シートグループの編集とクロス集計表の作成
#'
#' @file edit_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.5.26
EditSheetGroups <- function(json_files, sheet_info) {
  kGroupCode <- "group_code"
  kSheetName <- "sheet_name"
  kSheetGroupName <- "sheet_group_name"
  kGroupLabel <- "group_label"
  kDefault <- "default"
  kDefaultLabel <- "デフォルト"
  kSeparator <- "___"
  kMarkHit <- "〇"
  kMarkMiss <- "-"
  kNameKey <- "name_key"
  kIsVisitOrAllocation <- "is_visit_or_allocation"
  kFlag <- "flag"
  kFlagTrue <- "T"
  kFlagFalse <- "F"
  kColumnInfoKey <- "column_info_key"
  kJapaneseName <- "japanese_name"
  sheet_orders <- sheet_info %>% 
    select(name=alias_name, seq=sort_order) %>% 
    distinct()

  sheet_group_mappings <- json_files[[.const[["kSheetGroups"]]]] %>% 
    map_df(function(sg) {
      alloc_group <- sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      
      alloc_sheets <- sg[[.const[["kAllocationSheet"]]]]
      alloc_aliases <- if (is.null(alloc_sheets)) {
        NA_character_
      } else if (!is.null(alloc_sheets[[.const[["kAliasName"]]]])) {
        alloc_sheets[[.const[["kAliasName"]]]]
      } else {
        map_chr(alloc_sheets, ~ if(is.list(.x)) .x[[.const[["kAliasName"]]]] %||% NA_character_ else as.character(.x))
      }
      
      sheets_list <- sg[[.const[["kSheets"]]]]
      sheet_aliases <- if (is.null(sheets_list)) {
        NA_character_
      } else {
        map_chr(sheets_list, ~ if(is.list(.x)) .x[[.const[["kAliasName"]]]] %||% NA_character_ else as.character(.x))
      }
      
      expand_grid(
        !!.const[["kSheetAliasName"]] := sheet_aliases,
        !!.const[["kSheetGroupAllocationGroup"]] := alloc_group,
        !!.const[["kAliasName"]] := alloc_aliases
      )
    }) %>% 
    distinct()

  sheet_group_allocations <- json_files[[.const[["kSheetGroups"]]]] %>% 
    keep(~ !is.null(.x[[.const[["kSheetGroupAllocationGroup"]]]]) && !is.null(.x[[.const[["kAllocationSheet"]]]])) %>% 
    map_df(function(sg) {
      alloc_sheets <- sg[[.const[["kAllocationSheet"]]]]
      alloc_aliases <- if (!is.null(alloc_sheets[[.const[["kAliasName"]]]])) {
        alloc_sheets[[.const[["kAliasName"]]]] 
      } else {
        map_chr(alloc_sheets, ~ if(is.list(.x)) .x[[.const[["kAliasName"]]]] %||% NA_character_ else as.character(.x))
      }
      
      tibble(
        !!.const[["kAliasName"]] := alloc_aliases,
        !!kSheetGroupName := sg[[.const[["kSheetGroupsName"]]]] %||% NA_character_,
        !!kGroupCode := sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      )
    }) %>% 
    distinct()

  allocation_group_master <- json_files[[.const[["kSheets"]]]] %>% 
    keep(~ !is.null(.x[[.const[["kAllocation"]]]][[.const[["kAllocationGroups"]]]])) %>% 
    map_df(function(s) {
      s_name <- s[[.const[["kSheetJapaneseName"]]]] %||% NA_character_
      alias_name <- s[[.const[["kAliasName"]]]] %||% NA_character_
      
      s[[.const[["kAllocation"]]]][[.const[["kAllocationGroups"]]]] %>% 
        map_df(~ tibble(
          !!kSheetName := s_name,
          !!.const[["kAliasName"]] := alias_name,  
          !!kGroupCode := .x[[.const[["kAllocationGroupsCode"]]]] %||% NA_character_,
          !!kGroupLabel := .x[[.const[["kAllocationGroupsLabel"]]]] %||% NA_character_
        ))
    }) %>% 
    distinct()

  column_information <- allocation_group_master %>%
    left_join(sheet_group_allocations, by = c(.const[["kAliasName"]], kGroupCode)) %>%
    select(!!kSheetName, !!.const[["kSheetAliasName"]] := !!.const[["kAliasName"]], !!kSheetGroupName, !!kGroupCode, !!kGroupLabel) %>%
    mutate(!!kSheetGroupName := if_else(is.na(!!sym(kSheetGroupName)), !!sym(kGroupLabel), !!sym(kSheetGroupName))) %>%
    select(-!!kGroupLabel) %>%
    distinct()

  resolved_mappings <- sheet_group_mappings %>%
    left_join(column_information,
              by = setNames(c(.const[["kSheetAliasName"]], kGroupCode),
                            c(.const[["kAliasName"]], .const[["kSheetGroupAllocationGroup"]])))

  ordered_sheet_groups <- sheet_orders %>%
    left_join(resolved_mappings,
              by = setNames(.const[["kSheetAliasName"]], .const[["kSheetJapaneseName"]]))

  unique_columns <- column_information %>% 
    distinct(!!sym(kSheetName), !!sym(.const[["kSheetAliasName"]]), !!sym(kSheetGroupName), !!sym(kGroupCode))
  
  match_data <- ordered_sheet_groups %>% 
    mutate(!!kColumnInfoKey := if_else(
      is.na(alias_name), 
      kDefault, 
      paste(alias_name, allocation_group, sep = kSeparator)
    )) %>% 
    select(name, !!sym(kColumnInfoKey)) %>% 
    distinct() %>% 
    mutate(!!kFlag := kMarkHit)
  
  expected_columns <- c(
    kDefault,
    paste(unique_columns[[.const[["kSheetAliasName"]]]], unique_columns[[kGroupCode]], sep = kSeparator)
  )
  
  grid_base <- expand_grid(
    !!.const[["kSheetGroupsName"]] := unique(ordered_sheet_groups[[.const[["kSheetGroupsName"]]]]),
    !!kColumnInfoKey := expected_columns
  )
  
  pivoted_body <- grid_base %>%
    left_join(match_data, by = c(.const[["kSheetGroupsName"]], kColumnInfoKey)) %>%
    mutate(!!kFlag := if_else(is.na(!!sym(kFlag)), kMarkMiss, !!sym(kFlag))) %>% 
    pivot_wider(names_from = !!sym(kColumnInfoKey), values_from = !!sym(kFlag))
  
  alias_master <- json_files[[.const[["kSheets"]]]] %>% 
    map_df(~ tibble(
      !!kNameKey := .x[[.const[["kAliasName"]]]] %||% NA_character_,
      !!kJapaneseName := .x[[.const[["kSheetJapaneseName"]]]] %||% NA_character_,
      !!.const[["kCategory"]] := .x[[.const[["kCategory"]]]] %||% NA_character_
    )) %>% 
    distinct()
  
  alias_to_properties_master <- alias_master %>% 
    mutate(!!kIsVisitOrAllocation := if_else(!!sym(.const[["kCategory"]]) %in% c(.const[["kCategoryVisit"]], .const[["kCategoryAllocation"]]), kFlagTrue, kFlagFalse)) %>% 
    mutate(!!kIsVisitOrAllocation := if_else(is.na(!!sym(kIsVisitOrAllocation)), kFlagFalse, !!sym(kIsVisitOrAllocation))) %>% 
    select(!!sym(kNameKey), !!sym(kJapaneseName), !!sym(kIsVisitOrAllocation))
  
  order_seq_master <- sheet_orders %>% 
    select(name, !!.const[["kSheetSeq"]]) %>% 
    distinct() %>% 
    mutate(!!.const[["kSheetSeq"]] := as.character(!!sym(.const[["kSheetSeq"]])))
  
  matrix_body <- pivoted_body %>%
    left_join(alias_to_properties_master,
              by = setNames(kNameKey, .const[["kSheetJapaneseName"]])) %>%
    rename(!!kSheetName := japanese_name) %>% 
    mutate(!!sym(kSheetName) := if_else(is.na(!!sym(kSheetName)), kDefaultLabel, !!sym(kSheetName))) %>% 
    mutate(!!sym(kIsVisitOrAllocation) := if_else(is.na(!!sym(kIsVisitOrAllocation)), kFlagFalse, !!sym(kIsVisitOrAllocation))) %>% 
    left_join(order_seq_master, by = .const[["kSheetJapaneseName"]]) %>% 
    select(!!sym(.const[["kSheetSeq"]]), !!sym(.const[["kSheetJapaneseName"]]), !!sym(kSheetName), !!sym(kIsVisitOrAllocation), everything())
  
  header_2 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefault, unique_columns[[kGroupCode]])
  header_3 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefaultLabel, unique_columns[[kSheetName]])
  header_4 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefaultLabel, unique_columns[[kSheetGroupName]])
  
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
  cross_tab[2, 2] <- .const[["kAliasNameJapaneseColumnName"]]
  cross_tab[2, 3] <- .const[["kNameJapaneseColumnName"]]

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
  # visit / allocation 対象のデータ行
  visit_data <- data_rows %>%
    filter(is_visit_or_allocation == "T") %>%
    arrange(as.numeric(.data[[.const[["kSheetSeq"]]]]))

  # それ以外のデータ行
  non_visit_data <- data_rows %>%
    filter(is_visit_or_allocation == "F") %>%
    arrange(as.numeric(.data[[.const[["kSheetSeq"]]]]))

  # --- 3. ヘッダーとソート済データを再結合 ---
  visit_cross_tab     <- bind_rows(header_rows, visit_data)
  non_visit_cross_tab <- bind_rows(header_rows, non_visit_data)

  # --- 4. 不要な列（is_visit_or_allocation と seq）の削除 ---
  drop_cols <- c(.const[["kSheetSeq"]], "is_visit_or_allocation")
  visit_cross_tab     <- visit_cross_tab     %>% select(-all_of(drop_cols))
  non_visit_cross_tab <- non_visit_cross_tab %>% select(-all_of(drop_cols))
  
  # --- 5. 成果物をリストにして返却 ---
  return(list(
    visit_cross_tab     = visit_cross_tab,
    non_visit_cross_tab = non_visit_cross_tab
  ))
}

EditSheetGroupsMain <- function(json_files, sheet_info) {
  sheet_groups <- EditSheetGroups(json_files, sheet_info)
  sheet_groups_table <- SplitCrossTab(sheet_groups)
  return(sheet_groups_table)
}