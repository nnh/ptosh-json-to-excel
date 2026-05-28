#' edit_sheet_groups.R
#' シートグループの編集とクロス集計表の作成
#'
#' @file edit_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.5.27
#' allocation_sheet から alias_name のベクトルを取得するヘルパー関数
#'
#' @param alloc_sheets sg[["allocation_sheet"]] の値（NULL / 単一オブジェクト / リスト）
#' @return alias_name の文字列ベクトル（NULL の場合は NA_character_）
GetAllocAliases <- function(alloc_sheets) {
  if (is.null(alloc_sheets)) {
    NA_character_
  } else if (!is.null(alloc_sheets[[.const[["kAliasName"]]]])) {
    alloc_sheets[[.const[["kAliasName"]]]]
  } else {
    map_chr(alloc_sheets, ~ if (is.list(.x)) .x[[.const[["kAliasName"]]]] %||% NA_character_ else as.character(.x))
  }
}
#' シートグループが割当グループと割当シートを両方持つか判定する述語
HasAllocationGroup <- function(x) {
  !is.null(x[[.const[["kSheetGroupAllocationGroup"]]]]) &&
      !is.null(x[[.const[["kAllocationSheet"]]]])
}
#' シートが allocation.groups を持つか判定する述語
HasAllocationGroupsDefined <- function(x) {
  !is.null(x[[.const[["kAllocation"]]]][[.const[["kAllocationGroups"]]]])
}

#' @param json_files JSONファイルのリスト
#' @param sheet_info シート情報のデータフレーム（alias_name, sort_order 列を含む）
#' @return 2行ヘッダー構造のクロス集計表（data.frame）
EditSheetGroups <- function(json_files, sheet_info) {
  # --- ローカル定数 ---
  kGroupCode <- "group_code"
  kSheetName <- "sheet_name"
  kSheetGroupName <- "sheet_group_name"
  kGroupLabel <- "group_label"
  kDefault <- .const[["kDefault"]]
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
  # --- 処理用データの準備 ---
  # sheet_info から alias_name（name列）と sort_order（seq列）を抽出
  sheet_orders <- sheet_info %>%
    select(name=alias_name, seq=sort_order) %>%
    distinct()

  # --- 1. シートグループ × シート × 割当グループ の全マッピングを構築 ---
  # sheet_groups の各グループについて、所属シートと割当シートの全組み合わせを展開する
  sheet_group_mappings <- json_files[[.const[["kSheetGroups"]]]] %>%
    map_df(function(sg) {
      alloc_group <- sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      
      alloc_aliases <- GetAllocAliases(sg[[.const[["kAllocationSheet"]]]])
      
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

  # --- 2. 割当グループを持つシートグループの情報を抽出 ---
  # allocation_group と allocation_sheet を両方持つグループのみを対象とする
  sheet_group_allocations <- json_files[[.const[["kSheetGroups"]]]] %>%
    keep(HasAllocationGroup) %>%
    map_df(function(sg) {
      alloc_aliases <- GetAllocAliases(sg[[.const[["kAllocationSheet"]]]])
      
      tibble(
        !!.const[["kAliasName"]] := alloc_aliases,
        !!kSheetGroupName := sg[[.const[["kSheetGroupsName"]]]] %||% NA_character_,
        !!kGroupCode := sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      )
    }) %>% 
    distinct()

  # --- 3. 各シートの割当グループ定義マスタを作成 ---
  # sheets.allocation.groups から group_code と group_label を展開する
  allocation_group_master <- json_files[[.const[["kSheets"]]]] %>%
    keep(HasAllocationGroupsDefined) %>%
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

  # --- 4. 列情報の構築（割当グループ → シートグループ名の対応表） ---
  # group_label は sheet_group_name が未設定の場合のフォールバック値として使用する
  column_information <- allocation_group_master %>%
    left_join(sheet_group_allocations, by = c(.const[["kAliasName"]], kGroupCode)) %>%
    select(!!kSheetName, !!.const[["kSheetAliasName"]] := !!.const[["kAliasName"]], !!kSheetGroupName, !!kGroupCode, !!kGroupLabel) %>%
    mutate(!!kSheetGroupName := if_else(is.na(!!sym(kSheetGroupName)), !!sym(kGroupLabel), !!sym(kSheetGroupName))) %>%
    select(-!!kGroupLabel) %>%
    distinct()

  # --- 5. グループ情報を結合し、シート順序を付与 ---
  # sheet_group_mappings に列情報を結合し、さらに sheet_orders の順序（seq）を付与する
  resolved_mappings <- sheet_group_mappings %>%
    left_join(column_information,
              by = setNames(c(.const[["kSheetAliasName"]], kGroupCode),
                            c(.const[["kAliasName"]], .const[["kSheetGroupAllocationGroup"]])))

  ordered_sheet_groups <- sheet_orders %>%
    left_join(resolved_mappings,
              by = setNames(.const[["kSheetAliasName"]], .const[["kSheetJapaneseName"]]))

  # --- 6. クロス集計表の構築 ---
  # unique_columns: クロス集計の列定義（シート × グループコードのユニーク一覧）
  unique_columns <- column_information %>%
    distinct(!!sym(kSheetName), !!sym(.const[["kSheetAliasName"]]), !!sym(kSheetGroupName), !!sym(kGroupCode))
  
  # match_data: 各シートが該当する列（column_info_key）に〇を立てるデータ
  # column_info_key = "default"（割当なし）または "alias_name___group_code"（割当あり）
  match_data <- ordered_sheet_groups %>%
    mutate(!!kColumnInfoKey := if_else(
      is.na(.data[[.const[["kAliasName"]]]]),
      kDefault,
      paste(.data[[.const[["kAliasName"]]]], .data[[.const[["kSheetGroupAllocationGroup"]]]], sep = kSeparator)
    )) %>%
    select(!!sym(.const[["kSheetJapaneseName"]]), !!sym(kColumnInfoKey)) %>%
    distinct() %>%
    mutate(!!kFlag := kMarkHit)
  
  # expected_columns: クロス集計の全列キー（default + 全グループ列）
  expected_columns <- c(
    kDefault,
    paste(unique_columns[[.const[["kSheetAliasName"]]]], unique_columns[[kGroupCode]], sep = kSeparator)
  )
  
  # grid_base: シート × 全列キー の全組み合わせグリッド（〇/-を埋める土台）
  grid_base <- expand_grid(
    !!.const[["kSheetGroupsName"]] := unique(ordered_sheet_groups[[.const[["kSheetGroupsName"]]]]),
    !!kColumnInfoKey := expected_columns
  )
  
  pivoted_body <- grid_base %>%
    left_join(match_data, by = c(.const[["kSheetGroupsName"]], kColumnInfoKey)) %>%
    mutate(!!kFlag := if_else(is.na(!!sym(kFlag)), kMarkMiss, !!sym(kFlag))) %>% 
    pivot_wider(names_from = !!sym(kColumnInfoKey), values_from = !!sym(kFlag))
  
  # --- 7. シートプロパティ（日本語名・カテゴリ）を付与してマトリクス本体を完成 ---
  alias_master <- json_files[[.const[["kSheets"]]]] %>%
    map_df(~ tibble(
      !!kNameKey := .x[[.const[["kAliasName"]]]] %||% NA_character_,
      !!kJapaneseName := .x[[.const[["kSheetJapaneseName"]]]] %||% NA_character_,
      !!.const[["kCategory"]] := .x[[.const[["kCategory"]]]] %||% NA_character_
    )) %>% 
    distinct()
  
  alias_to_properties_master <- alias_master %>%
    mutate(!!kIsVisitOrAllocation := if_else(
      !!sym(.const[["kCategory"]]) %in% c(.const[["kCategoryVisit"]], .const[["kCategoryAllocation"]]),
      kFlagTrue,
      kFlagFalse
    )) %>%
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
  
  # --- 8. ヘッダー行の組み立てとクロス集計表の完成 ---
  # header_2: 内部列名（英数字キー）。colnames として cross_tab に設定する
  # header_3: Excel 表示用 1行目ヘッダー（シート名日本語 / 割当シート名）
  # header_4: Excel 表示用 2行目ヘッダー（シート名日本語 / シートグループ名）
  header_2 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefault, unique_columns[[kGroupCode]])
  header_3 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefaultLabel, unique_columns[[kSheetName]])
  header_4 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSheetName, kIsVisitOrAllocation, kDefaultLabel, unique_columns[[kSheetGroupName]])
  
  # bind_rows で異なる列名のベクトルを行として結合するため、
  # 一旦ダミー列名で統一し、names() でヘッダー行と対応付けてから結合する
  dummy_colnames <- paste0("V", 1:ncol(matrix_body))
  colnames(matrix_body) <- dummy_colnames
  names(header_3) <- dummy_colnames
  names(header_4) <- dummy_colnames

  cross_tab <- bind_rows(
    as_tibble_row(header_3),
    as_tibble_row(header_4),
    matrix_body
  )

  # 列名を英数字キー（header_2）に設定し、ヘッダー行にセンチネル値と表示ラベルを書き込む
  # -999 / -888: SplitCrossTab でヘッダー行を通常データと区別するためのセンチネル値
  colnames(cross_tab) <- header_2
  cross_tab[1, 1] <- "-999"
  cross_tab[2, 1] <- "-888"
  cross_tab[1:2, 2:3] <- ""
  cross_tab[2, 2] <- .const[["kAliasNameJapaneseColumnName"]]
  cross_tab[2, 3] <- .const[["kNameJapaneseColumnName"]]

  return(cross_tab)
}
#' SplitCrossTab
#'
#' EditSheetGroups() が返す2行ヘッダー構造のクロス集計表を
#' visit/allocation シートと非 visit シートに分割して返す。
#' @param cross_tab EditSheetGroups() の戻り値（2行ヘッダー構造のクロス集計表）
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

#' EditSheetGroupsMain
#'
#' @param json_files JSONファイルのリスト
#' @param sheet_info シート情報のデータフレーム（alias_name, sort_order 列を含む）
#' @return visit_cross_tab と non_visit_cross_tab を格納したリスト
EditSheetGroupsMain <- function(json_files, sheet_info) {
  sheet_groups <- EditSheetGroups(json_files, sheet_info)
  sheet_groups_table <- SplitCrossTab(sheet_groups)
  return(sheet_groups_table)
}