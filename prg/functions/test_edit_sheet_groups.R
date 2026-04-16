#' sheet_groupsリストの全情報を保持したままtibbleに変換する
#'
#' @param sheet_groups 入力されたリストオブジェクト
#' @return 展開された情報のtibble
flattenSheetGroupList <- function(sheet_groups) {
  
  sheet_groups %>%
    # 各要素（リスト）を1行のデータフレームに変換
    map_df(function(x) {
      # 1. 第1階層の基本情報を抽出
      base_info <- tibble(
        uuid = x$uuid %||% NA_character_,
        name = x$name %||% NA_character_,
        alias_name = x$alias_name %||% NA_character_,
        allocation_group = x$allocation_group %||% NA_character_,
        is_default = x$is_default %||% NA,
        created_at = x$created_at %||% NA_character_,
        updated_at = x$updated_at %||% NA_character_
      )
      
      # 2. allocation_sheet$alias_name の抽出
      # 存在しない場合（リストの長さが足りない場合）は NA を入れる
      allocation_sheet_alias <- if (!is.null(x$allocation_sheet$alias_name)) {
        x$allocation_sheet$alias_name
      } else {
        NA_character_
      }
      
      # 3. sheets リスト内の alias_name をベクトルとして抽出
      sheet_aliases <- if (!is.null(x$sheets) && length(x$sheets) > 0) {
        map_chr(x$sheets, ~ .x$alias_name %||% NA_character_)
      } else {
        NA_character_
      }
      
      # 4. すべてを結合して tibble 化
      base_info %>%
        mutate(
          allocation_sheet_alias = allocation_sheet_alias,
          sheets_alias_name = list(sheet_aliases)
        ) %>%
        # sheets_alias_name（リスト列）を縦に展開して全情報を残す
        unnest(sheets_alias_name)
    })
}

#' sheetsリストの全情報を保持したままtibbleに変換する（型混在対応版）
#'
#' @param sheets_list 入力されたsheetsリストオブジェクト
#' @return 展開された情報のtibble
flattenSheetsList <- function(sheets_list) {
  
  sheets_list %>%
    map_df(function(x) {
      # 1. 第1階層の基本情報を抽出
      base_info <- tibble(
        name = x$name %||% NA_character_,
        alias_name = x$alias_name %||% NA_character_,
        category = x$category %||% NA_character_,
        images_count = x$images_count %||% NA_integer_,
        is_serious = x$is_serious %||% NA,
        is_closed = x$is_closed %||% NA,
        sort_order = x$sort_order %||% NA_real_
      )
      
      # 2. sheet_groups の処理
      sg_raw <- x$sheet_groups
      
      # 型を判定して、データフレーム（tibble）として扱える形に整える
      sg_data <- if (is.data.frame(sg_raw)) {
        # データフレームの場合は接頭辞をつけて衝突回避
        sg_raw %>% rename_with(~ paste0("sg_", .))
      } else {
        # NA (logical) や NULL の場合は、空の1行tibbleを作成
        # 列名はデータが存在する場合の構造に合わせる
        tibble(
          sg_alias_name = NA_character_,
          sg_group_name = NA_character_,
          sg_group_alias_name = NA_character_,
          sg_allocation_group = NA_character_,
          sg_is_default = NA
        )
      }
      
      # 3. 結合
      bind_cols(base_info, sg_data)
    })
}

editSheetGroupsVisit <- function() {
    test_sheets <- flattenSheetsList(sheets) %>% 
      select(-c(images_count, is_serious, is_closed))
    test_sheet_groups <- flattenSheetGroupList(json_files$sheet_groups) %>% 
      select(c(sheets_alias_name, allocation_sheet_alias))
    test_sheet_groups_2 <- test_sheets %>% 
      left_join(
        test_sheet_groups, 
          by = c("alias_name" = "sheets_alias_name"),
          relationship = "many-to-many"
      )

    test_sheet_groups_3 <- test_sheet_groups_2 
    test_sheet_groups_3$arm_sheets_name <- ifelse(
      test_sheet_groups_3$sg_is_default, 
      "デフォルト", 
      test_sheet_groups_3$sg_group_name
    )
    test_sheet_groups_3$arm_code <- ifelse(
      is.na(test_sheet_groups_3$sg_allocation_group) | test_sheet_groups_3$sg_allocation_group == "", 
      "default", 
      test_sheet_groups_3$sg_allocation_group
    )
    test_sheet_groups_3$arm_name <- ifelse(test_sheet_groups_3$sg_is_default, "デフォルト", test_sheet_groups_3$sg_group_alias_name)
    result_xtab <- createArmSheetCrossTab(test_sheet_groups_3)
    visit <- result_xtab$visit
    nonvisit <- result_xtab$nonvisit

}

#' arm_alias_nameを横軸、sheets_alias_nameを縦軸にしたクロス集計表を作成する
#'
#' @param df 入力データフレーム (test_sheet_groups_3)
#' @return クロス集計された tibble
createArmSheetCrossTab <- function(input_df) {
  kHeaderCategory <- "header"
  df <- input_df %>%
    # 1. 必要な列を残したまま重複を排除する
    # sheets_name も集計の軸(id_cols)に使うため、ここで含めておく必要があります
    distinct(alias_name, name, allocation_sheet_alias, sort_order, category, arm_code) %>%
    
    # 2. 存在を示すフラグ（記号）を作成
    mutate(exists = "○") %>%
    
    # 3. 縦持ちから横持ち（クロス形式）に変換
    pivot_wider(
      id_cols = c(sort_order, alias_name, name, category),    # 縦軸に日本語名も含める
      names_from = c(allocation_sheet_alias, arm_code),       # 横軸
      names_sep = "|",
      values_from = exists,
      values_fill = "-"                              # 存在しない場合はハイフン
    ) %>%
    
    # 4. シート名でソート
    arrange(sort_order)
  # allocation情報を取得
  allocation_info <- sheet_info %>% filter(category == "allocation") %>% 
    select(alias_name, sheet_name)
  group_info <- sheet_info %>% select(allocation_group, group_name) %>% distinct()
  group_info$group_name <- ifelse(
    group_info$group_name == "default" | is.na(group_info$group_name), 
    "デフォルト", 
    group_info$group_name
  )
  # 列名の行を作成する
  header <- df %>%
    # 構造（列名と型）だけ残して0行にする
    slice(0) %>%
    # 2行のNA行を追加
    add_row() %>%
    add_row()
  temp_colnames <- colnames(df)
  length(temp_colnames)
  for (i in 1:length(temp_colnames)) {
    if (str_detect(temp_colnames[i], "\\|")) {
      parts <- str_split(temp_colnames[i], "\\|")[[1]]
      if (length(parts) == 2) {
        allocation_alias <- parts[1]
        arm_code <- parts[2]
        if (allocation_alias == "NA" || allocation_alias == "default") {
          header[1, i] <- "デフォルト"
        } else {
          allocation_row <- allocation_info %>% filter(alias_name == allocation_alias)
          if (nrow(allocation_row) > 0) {
            header[1, i] <- allocation_row$sheet_name[1]
          } 
        }
        if (arm_code == "NA" || arm_code == "default") {
          header[2, i] <- "デフォルト"
        } else {
          group_row <- group_info %>% filter(allocation_group == arm_code)
          if (nrow(group_row) > 0) {
            header[2, i] <- group_row$group_name[1]
          }
        }
      }
    } else {
      if (temp_colnames[i] == "alias_name") {
        header[2, i] <- .const[["kAliasNameJapaneseColumnName"]]
      } else if (temp_colnames[i] == "name") {
        header[2, i] <- "シート名"
      } else if (temp_colnames[i] == "sort_order") {
        header[1, i] <- -999
        header[2, i] <- -888
      } else if (temp_colnames[i] == "category") {
        header[1, i] <- kHeaderCategory
        header[2, i] <- kHeaderCategory

      } else {
        header[2, i] <- temp_colnames[i]
      }
    }
  }
  # categoryがvisitまたはallocationならばvisitに、それ以外ならnonvisitに格納
  header_df <- df %>% bind_rows(header)
  temp_visit <- header_df %>% filter(category == "visit" | category == "allocation" | category == kHeaderCategory) %>% arrange(sort_order)
  visitnum_alias_name <- visit_info %>% select(c(.const[["kAliasName"]], visitnum))
  visit <- visitnum_alias_name %>% left_join(temp_visit, ., by = .const[["kAliasName"]])# visitnum と alias_name を先頭に移動し、残りの列順は維持する
  visit <- visit %>%
    relocate(visitnum, alias_name, .before = everything()) %>% arrange(sort_order)
  visit <- visit %>% select(-c(sort_order, category))
  nonvisit <- header_df %>% filter(!(category == "visit" | category == "allocation")) %>% arrange(sort_order) 
  nonvisit <- nonvisit %>% select(-c(sort_order, category))
  result <- list(
    visit = visit,
    nonvisit = nonvisit
  )
  return(result)
}
