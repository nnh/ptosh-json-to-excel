#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2026.1.8
CheckItemVisit <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]]
    json <- isVisit_json
    if (!isVisit & nrow(sheet) == 1 & length(json) == 0) {
        if (all(is.na(sheet[1, ]) | sheet[1, ] == "")) {
            return(NULL)
        }
    }
    non_visit_sheet_names <- isNonVisit_json %>% map_chr(~ .x[["alias_name"]])

    articles <- json %>% map(~ {
        alias_name <- .x[["alias_name"]]
        group <- visitGroups %>%
            filter(alias_name == !!alias_name) %>%
            pull(group)
        temp <- .x$field_items %>% keep(~ .x$type == "FieldItem::Article")
        if (length(temp) == 0) {
            return(NULL)
        }
        res <- list(field_items = temp, group = group)
        return(res)
    })
    article_groups <- articles
    for (i in seq_along(article_groups)) {
        groupName <- article_groups[[i]][["group"]]
        idx <- i + 1
        if (idx > length(article_groups)) {
            next
        }
        for (j in idx:length(article_groups)) {
            if (!is.null(article_groups[[j]]) && article_groups[[j]][["group"]] == groupName) {
                article_groups[[j]]$field_items <- NA
            }
        }
    }
    target_articles <- article_groups %>%
        discard(~ all(is.na(.x$field_items)))

    # field_itemsにnormal_rangeが存在するものだけ抽出
    normal_ranges <- target_articles %>%
        map(~ {
            field_items <- .x[["field_items"]]
            normal_range_items <- field_items %>%
                keep(~ !is.null(.x$normal_range) && length(.x$normal_range) > 0)
            if (length(normal_range_items) == 0) {
                return(NULL)
            }
            res <- list(field_items = normal_range_items, group = .x[["group"]])
            return(res)
        }) %>%
        discard(~ is.null(.x))
    if (length(normal_ranges) > 0) {
        df_normal_ranges <- normal_ranges %>%
            map_df(~ {
                group <- .x[["group"]]
                res <- .x[["field_items"]] %>% map_df(~ tibble(
                    group = group, label = .x$label, cat = "アラート設定あり"
                ))
                return(res)
            }) %>%
            distinct()
    } else {
        df_normal_ranges <- tibble(
            group = character(),
            label = character(),
            cat = character()
        )
    }
    # field_itemsにvaridators$numericalityが存在するものだけ抽出
    validators <- target_articles %>%
        map(~ {
            group <- .x[["group"]]
            field_items <- .x[["field_items"]]
            validator_items <- field_items %>%
                keep(~ !is.null(.x$validators$numericality))
            if (length(validator_items) == 0) {
                return(NULL)
            }
            res <- validator_items %>%
                map(~ {
                    numericality <- .x$validators$numericality
                    if (is.null(numericality)) {
                        return(NULL)
                    }
                    res <- tibble(
                        group = group,
                        label = .x$label, cat = "数値チェックあり"
                    )
                    return(res)
                }) %>%
                bind_rows()
            return(res)
        }) %>%
        discard(~ is.null(.x))
    if (length(validators) > 0) {
        df_validators <- validators %>%
            bind_rows() %>%
            distinct()
    } else {
        df_validators <- tibble(
            group = character(),
            label = character(),
            cat = character()
        )
    }
    df_target_articles <- target_articles %>%
        map(~ {
            group <- .x[["group"]]
            res <- .x$field_items %>% map(~ tibble(
                group = group,
                label = .x$label
            ))
            res <- bind_rows(res)
            return(res)
        }) %>%
        bind_rows()

    df_target_articles$count <- NA_integer_

    for (i in seq_len(nrow(df_target_articles))) {
        group_i <- df_target_articles$group[i]
        label_i <- df_target_articles$label[i]

        target <- df_target_articles %>%
            filter(group == group_i, label == label_i) %>%
            nrow()

        df_target_articles$count[i] <- target
    }
    df_target_articles <-
        df_target_articles %>%
        distinct()
    # 数値チェック、アラート設定の有無を結合
    df_target_articles <- df_target_articles %>%
        left_join(df_validators, by = c("group", "label")) %>%
        left_join(df_normal_ranges, by = c("group", "label")) %>%
        mutate(
            cat = case_when(
                !is.na(cat.x) & !is.na(cat.y) ~ "数値・アラート有",
                !is.na(cat.x) & is.na(cat.y) ~ "数値チェック有",
                is.na(cat.x) & !is.na(cat.y) ~ "アラート設定有",
                TRUE ~ "条件なし"
            )
        ) %>%
        select(group, label, count, cat)

    # ソート順の取得
    item_visits_sheets <- target_json$sheets %>%
        map(~ {
            aliasName <- .x[["alias_name"]]
            group <- visitGroups %>%
                filter(alias_name == !!aliasName) %>%
                pull(group)
            articles <- .x$field_items %>%
                keep(~ .x$type == "FieldItem::Article") %>%
                map_df(~ tibble(
                    group = group,
                    label = .x$label
                ))
        }) %>%
        bind_rows() %>%
        distinct()
    item_visits_sort_orders <- item_visits_sheets %>%
        inner_join(visitGroupSheetAndFieldOrders, by = c("group" = "alias_name", "label" = "field_label")) %>%
        select(label, seq, field_seq) %>%
        arrange(seq, field_seq)
    for (i in seq_len(nrow(item_visits_sort_orders))) {
        label_i <- item_visits_sort_orders$label[i]
        idx <- i + 1
        if (idx > nrow(item_visits_sort_orders)) {
            next
        }
        for (j in idx:nrow(item_visits_sort_orders)) {
            if (item_visits_sort_orders$label[j] == label_i) {
                item_visits_sort_orders$seq[j] <- -999
                item_visits_sort_orders$field_seq[j] <- -999
            }
        }
    }
    # 出力行名の作成
    output_item_visits_rownames <- item_visits_sort_orders %>%
        filter(seq != -999) %>%
        arrange(seq, field_seq) %>%
        select(label)
    # 出力列名の取得
    temp_groups <- df_target_articles %>%
        select(group) %>%
        distinct()
    output_item_visits_visit_names <- temp_groups %>%
        inner_join(visitGroups, by = c("group")) %>%
        select(name) %>%
        distinct()
    temp_sheet_sort_orders <- visitGroups %>%
        inner_join(sheetOrders, by = c("alias_name" = "sheet")) %>%
        arrange(seq)
    for (i in seq_len(nrow(temp_sheet_sort_orders))) {
        name_i <- temp_sheet_sort_orders$name[i]
        idx <- i + 1
        if (idx > nrow(temp_sheet_sort_orders)) {
            next
        }
        for (j in idx:nrow(temp_sheet_sort_orders)) {
            if (temp_sheet_sort_orders$name[j] == name_i) {
                temp_sheet_sort_orders$seq[j] <- -999
            }
        }
    }
    temp_sheet_sort_orders <- temp_sheet_sort_orders %>%
        filter(seq != -999) %>%
        arrange(seq)
    output_item_visits_visit_names <- temp_sheet_sort_orders %>%
        pull(name)
    # output_item_visits_visit_namesを列名にしたtibbleを作成
    output_item_visits_tbl <- tibble()
    for (visit_name in output_item_visits_visit_names) {
        output_item_visits_tbl[, visit_name] <- NA
        for (i in seq_len(nrow(output_item_visits_rownames))) {
            output_item_visits_tbl[i, visit_name] <- 0
        }
    }
    output_item_visits_visit_label <- output_item_visits_tbl %>% bind_cols(output_item_visits_rownames, .)

    for (i in 1:nrow(df_target_articles)) {
        label_i <- df_target_articles$label[i]
        count_i <- df_target_articles$count[i]
        group_i <- df_target_articles$group[i]
        group_name <- visitGroups %>%
            filter(group == !!group_i) %>%
            pull(name) %>%
            unique()
        cat_i <- df_target_articles$cat[i]
        row_idx <- which(output_item_visits_visit_label$label == label_i)
        col_idx <- which(colnames(output_item_visits_visit_label) == group_name)
        output_item_visits_visit_label[row_idx, col_idx] <- count_i
        output_item_visits_visit_label[row_idx, "数値チェック・アラート条件の有無"] <- cat_i
    }
    json_item_visit <- output_item_visits_visit_label
    temp_colnames <- colnames(json_item_visit) %>% str_replace_all(" ", ".")
    colnames(json_item_visit) <- temp_colnames
    json <- json_item_visit %>% as.data.frame()
    if (identical(sheet, json)) {
        return(NULL)
    }
    if (nrow(sheet) != nrow(json)) {
        stop(str_c("Row count mismatch in sheet '", sheetName, "' in trial: ", trialName))
    }
    if (ncol(sheet) != ncol(json)) {
        stop(str_c("Column count mismatch in sheet '", sheetName, "' in trial: ", trialName))
    }
    if (!identical(colnames(sheet), colnames(json))) {
        print(colnames(sheet))
        print(colnames(json))
        stop(str_c("Column names mismatch in sheet '", sheetName, "' in trial: ", trialName))
    }
    if (!identical(sheet$label, json$label)) {
        stop(str_c("Labels mismatch in sheet '", sheetName, "' in trial: ", trialName))
    }
    for (row in 1:nrow(sheet)) {
        for (col in 2:ncol(sheet)) {
            val_sheet <- sheet[row, col][[1]]
            val_json <- json[row, col][[1]]
            if (is.na(val_sheet) && is.na(val_json)) {
                next
            }
            if (val_sheet != val_json) {
                stop(str_c("Value mismatch at row ", row, ", column ", col, " in sheet '", sheetName, "' in trial: ", trialName))
            }
        }
    }
    return(CheckTarget(json, sheet))
}
