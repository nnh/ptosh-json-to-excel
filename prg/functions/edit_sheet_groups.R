#' edit_sheet_groups.R
#' シートグループの編集とクロス集計表の作成
#'
#' @file edit_sheet_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
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

# --- EditSheetGroups 関連のモジュール内定数 ---
kSgGroupCode      <- "group_code"
kSgSheetName      <- "sheet_name"
kSgSheetGroupName <- "sheet_group_name"
kSgGroupLabel     <- "group_label"
kSgDefaultLabel   <- "デフォルト"
kSgSeparator      <- "___"
kSgMarkHit        <- "〇"
kSgMarkMiss       <- "-"
kSgNameKey        <- "name_key"
kSgIsVisitOrAlloc <- "is_visit_or_allocation"
kSgFlag           <- "flag"
kSgFlagTrue       <- "T"
kSgFlagFalse      <- "F"
kSgColumnInfoKey  <- "column_info_key"
kSgJapaneseName   <- "japanese_name"

# --- 内部ヘルパー関数 ---

#' シートグループ × シート × 割当グループ の全マッピングを構築する
BuildSheetGroupMappings <- function(json_files) {
  json_files[[.const[["kSheetGroups"]]]] %>%
    map_df(function(sg) {
      alloc_group <- sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      alloc_aliases <- GetAllocAliases(sg[[.const[["kAllocationSheet"]]]])
      sheets_list <- sg[[.const[["kSheets"]]]]
      sheet_aliases <- if (is.null(sheets_list)) {
        NA_character_
      } else {
        map_chr(sheets_list, ~ if (is.list(.x)) .x[[.const[["kAliasName"]]]] %||% NA_character_ else as.character(.x))
      }
      expand_grid(
        !!.const[["kSheetAliasName"]] := sheet_aliases,
        !!.const[["kSheetGroupAllocationGroup"]] := alloc_group,
        !!.const[["kAliasName"]] := alloc_aliases
      )
    }) %>%
    distinct()
}

#' 割当グループを持つシートグループの情報を抽出する
BuildSheetGroupAllocations <- function(json_files) {
  json_files[[.const[["kSheetGroups"]]]] %>%
    keep(HasAllocationGroup) %>%
    map_df(function(sg) {
      alloc_aliases <- GetAllocAliases(sg[[.const[["kAllocationSheet"]]]])
      tibble(
        !!.const[["kAliasName"]] := alloc_aliases,
        !!kSgSheetGroupName := sg[[.const[["kSheetGroupsName"]]]] %||% NA_character_,
        !!kSgGroupCode := sg[[.const[["kSheetGroupAllocationGroup"]]]] %||% NA_character_
      )
    }) %>%
    distinct()
}

#' 各シートの割当グループ定義マスタを作成する
BuildAllocationGroupMaster <- function(json_files) {
  json_files[[.const[["kSheets"]]]] %>%
    keep(HasAllocationGroupsDefined) %>%
    map_df(function(s) {
      s_alias <- s[[.const[["kAliasName"]]]] %||% NA_character_
      s[[.const[["kAllocation"]]]][[.const[["kAllocationGroups"]]]] %>%
        map_df(~ tibble(
          !!kSgSheetName := s[[.const[["kSheetJapaneseName"]]]] %||% NA_character_,
          !!.const[["kAliasName"]] := s_alias,
          !!kSgGroupCode := .x[[.const[["kAllocationGroupsCode"]]]] %||% NA_character_,
          !!kSgGroupLabel := .x[[.const[["kAllocationGroupsLabel"]]]] %||% NA_character_
        ))
    }) %>%
    distinct()
}

#' 列情報（割当グループ → シートグループ名の対応表）を構築する
BuildColumnInformation <- function(allocation_group_master, sheet_group_allocations) {
  allocation_group_master %>%
    left_join(sheet_group_allocations, by = c(.const[["kAliasName"]], kSgGroupCode)) %>%
    select(
      !!kSgSheetName,
      !!.const[["kSheetAliasName"]] := !!.const[["kAliasName"]],
      !!kSgSheetGroupName,
      !!kSgGroupCode,
      !!kSgGroupLabel
    ) %>%
    mutate(!!kSgSheetGroupName := if_else(
      is.na(!!sym(kSgSheetGroupName)),
      !!sym(kSgGroupLabel),
      !!sym(kSgSheetGroupName)
    )) %>%
    select(-!!kSgGroupLabel) %>%
    distinct()
}

#' グループ情報を結合し、シート順序を付与する
ResolveOrderedSheetGroups <- function(sheet_group_mappings, column_information, sheet_orders) {
  resolved_mappings <- sheet_group_mappings %>%
    left_join(
      column_information,
      by = setNames(
        c(.const[["kSheetAliasName"]], kSgGroupCode),
        c(.const[["kAliasName"]], .const[["kSheetGroupAllocationGroup"]])
      )
    )
  sheet_orders %>%
    left_join(
      resolved_mappings,
      by = setNames(.const[["kSheetAliasName"]], .const[["kSheetJapaneseName"]])
    )
}

#' クロス集計表の本体（pivot_wider 済みデータ）を構築する
PivotToCrossTabBody <- function(ordered_sheet_groups, unique_columns) {
  expected_columns <- c(
    .const[["kDefault"]],
    paste(unique_columns[[.const[["kSheetAliasName"]]]], unique_columns[[kSgGroupCode]], sep = kSgSeparator)
  )
  match_data <- ordered_sheet_groups %>%
    mutate(!!kSgColumnInfoKey := if_else(
      is.na(.data[[.const[["kAliasName"]]]]),
      .const[["kDefault"]],
      paste(.data[[.const[["kAliasName"]]]], .data[[.const[["kSheetGroupAllocationGroup"]]]], sep = kSgSeparator)
    )) %>%
    select(!!sym(.const[["kSheetJapaneseName"]]), !!sym(kSgColumnInfoKey)) %>%
    distinct() %>%
    mutate(!!kSgFlag := kSgMarkHit)

  expand_grid(
    !!.const[["kSheetGroupsName"]] := unique(ordered_sheet_groups[[.const[["kSheetGroupsName"]]]]),
    !!kSgColumnInfoKey := expected_columns
  ) %>%
    left_join(match_data, by = c(.const[["kSheetGroupsName"]], kSgColumnInfoKey)) %>%
    mutate(!!kSgFlag := if_else(is.na(!!sym(kSgFlag)), kSgMarkMiss, !!sym(kSgFlag))) %>%
    pivot_wider(names_from = !!sym(kSgColumnInfoKey), values_from = !!sym(kSgFlag))
}

#' シートプロパティ（日本語名・カテゴリ）を付与してマトリクス本体を完成させる
AddSheetPropertiesToMatrix <- function(pivoted_body, json_files, sheet_orders) {
  alias_master <- json_files[[.const[["kSheets"]]]] %>%
    map_df(~ tibble(
      !!kSgNameKey := .x[[.const[["kAliasName"]]]] %||% NA_character_,
      !!kSgJapaneseName := .x[[.const[["kSheetJapaneseName"]]]] %||% NA_character_,
      !!.const[["kCategory"]] := .x[[.const[["kCategory"]]]] %||% NA_character_
    )) %>%
    distinct()

  alias_to_props <- alias_master %>%
    mutate(!!kSgIsVisitOrAlloc := if_else(
      !!sym(.const[["kCategory"]]) %in% c(.const[["kCategoryVisit"]], .const[["kCategoryAllocation"]]),
      kSgFlagTrue,
      kSgFlagFalse
    )) %>%
    select(!!sym(kSgNameKey), !!sym(kSgJapaneseName), !!sym(kSgIsVisitOrAlloc))

  order_seq_master <- sheet_orders %>%
    select(name, !!.const[["kSheetSeq"]]) %>%
    distinct() %>%
    mutate(!!.const[["kSheetSeq"]] := as.character(!!sym(.const[["kSheetSeq"]])))

  pivoted_body %>%
    left_join(alias_to_props, by = setNames(kSgNameKey, .const[["kSheetJapaneseName"]])) %>%
    rename(!!kSgSheetName := !!sym(kSgJapaneseName)) %>%
    mutate(
      !!sym(kSgSheetName) := if_else(is.na(!!sym(kSgSheetName)), kSgDefaultLabel, !!sym(kSgSheetName)),
      !!sym(kSgIsVisitOrAlloc) := if_else(is.na(!!sym(kSgIsVisitOrAlloc)), kSgFlagFalse, !!sym(kSgIsVisitOrAlloc))
    ) %>%
    left_join(order_seq_master, by = .const[["kSheetJapaneseName"]]) %>%
    select(
      !!sym(.const[["kSheetSeq"]]),
      !!sym(.const[["kSheetJapaneseName"]]),
      !!sym(kSgSheetName),
      !!sym(kSgIsVisitOrAlloc),
      everything()
    )
}

#' ヘッダー行を組み立ててクロス集計表を完成させる
#' -999 / -888: SplitCrossTab でヘッダー行を通常データと区別するためのセンチネル値
AssembleCrossTabHeaders <- function(matrix_body, unique_columns) {
  # header_2: 内部列名（英数字キー）。colnames として cross_tab に設定する
  # header_3: Excel 表示用 1行目ヘッダー（シート名日本語 / 割当シート名）
  # header_4: Excel 表示用 2行目ヘッダー（シート名日本語 / シートグループ名）
  header_2 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSgSheetName, kSgIsVisitOrAlloc, .const[["kDefault"]], unique_columns[[kSgGroupCode]])
  header_3 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSgSheetName, kSgIsVisitOrAlloc, kSgDefaultLabel, unique_columns[[kSgSheetName]])
  header_4 <- c(.const[["kSheetSeq"]], .const[["kSheetJapaneseName"]], kSgSheetName, kSgIsVisitOrAlloc, kSgDefaultLabel, unique_columns[[kSgSheetGroupName]])

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

#' @param json_files JSONファイルのリスト
#' @param sheet_info シート情報のデータフレーム（alias_name, sort_order 列を含む）
#' @return 2行ヘッダー構造のクロス集計表（data.frame）
EditSheetGroups <- function(json_files, sheet_info) {
  sheet_orders <- sheet_info %>%
    select(name = alias_name, seq = sort_order) %>%
    distinct()

  sheet_group_mappings    <- BuildSheetGroupMappings(json_files)
  sheet_group_allocations <- BuildSheetGroupAllocations(json_files)
  allocation_group_master <- BuildAllocationGroupMaster(json_files)
  column_information      <- BuildColumnInformation(allocation_group_master, sheet_group_allocations)
  ordered_sheet_groups    <- ResolveOrderedSheetGroups(sheet_group_mappings, column_information, sheet_orders)

  unique_columns <- column_information %>%
    distinct(!!sym(kSgSheetName), !!sym(.const[["kSheetAliasName"]]), !!sym(kSgSheetGroupName), !!sym(kSgGroupCode))

  pivoted_body <- PivotToCrossTabBody(ordered_sheet_groups, unique_columns)
  matrix_body  <- AddSheetPropertiesToMatrix(pivoted_body, json_files, sheet_orders)
  cross_tab    <- AssembleCrossTabHeaders(matrix_body, unique_columns)

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
