GetGroupSheetNames <- function(targetColumn) {
    return(str_remove(targetColumn, "_[0-9]+$"))
}
ReplaceItemVisitSheetName <- function(check_group, alias_name_columnName) {
    reference_colnames <- c("条件の参照先情報", "論理式の参照先情報", "最小値の参照先情報", "最大値の参照先情報")
    group_name <- check_group[["group"]] %>%
        unlist() %>%
        unique()
    # 参照先情報内の自シート名をgroupに置き換える
    for (col in reference_colnames) {
        for (row in 1:nrow(check_group)) {
            if (!is.na(check_group[row, col, drop = TRUE])) {
                check_group[row, col] <- str_replace_all(
                    check_group[row, col, drop = TRUE],
                    check_group[row, alias_name_columnName, drop = TRUE],
                    group_name
                )
            }
        }
    }
    return(check_group)
}

CheckIdenticalItemVisitList <- function(item_visit_by_group_list, alias_name_columnName) {
    sheetName_columnName <- "シート名"
    res <- item_visit_by_group_list %>% map(~ {
        outputSheetName <- ifelse(length(.x) > 1, str_remove(.x[[1]][[sheetName_columnName]][1], "\\(.*$"), .x[[1]][[sheetName_columnName]][1])
        for (i in seq_along(.x)) {
            .x[[i]][[sheetName_columnName]] <- outputSheetName
        }
        base_tibble <- .x[[1]] %>% ReplaceItemVisitSheetName(., alias_name_columnName)
        if (length(.x) == 1) {
            return(base_tibble)
        }
        rest_tibbles <- .x[-1]
        res <- base_tibble
        identical_tibble_1 <- base_tibble %>% select(-all_of(alias_name_columnName))
        different_columns <- NULL
        for (i in seq_along(rest_tibbles)) {
            identical_tibble_2 <- rest_tibbles[[i]] %>%
                ReplaceItemVisitSheetName(., alias_name_columnName) %>%
                select(-all_of(alias_name_columnName))
            if (identical(identical_tibble_1, identical_tibble_2)) {
                alias_name <- rest_tibbles[[i]][[alias_name_columnName]] %>%
                    unlist() %>%
                    unique()
                res[[alias_name_columnName]] <- str_c(
                    res[[alias_name_columnName]], ", ", alias_name
                )
            } else {
                error_title <- str_c("グループ", base_tibble[["group"]][1], "のシート", base_tibble[[alias_name_columnName]][1], "は、前のシートと異なる項目があります。\n")
                error_columns <- list()
                for (col in colnames(identical_tibble_1)) {
                    if (!identical(identical_tibble_1[[col]], identical_tibble_2[[col]])) {
                        error_columns <- append(error_columns, col)
                    }
                }
                error_message <- paste(error_columns, collapse = ", ") %>%
                    str_c("列", ., "の項目が異なります。")
                different_columns <- append(different_columns, error_title) %>% append(error_message)
                res <- bind_rows(res, rest_tibbles[[i]])
            }
        }
        if (!is.null(different_columns)) {
            print(different_columns %>% unique())
        }
        return(res)
    })
    return(res)
}

GetItemVisitByGroupList <- function(item_visit_by_group, alias_name_columnName) {
    item_visit_groups <- item_visit_by_group[["group"]] %>% unique()
    res <- item_visit_groups %>%
        map(~ {
            group <- .x
            tibble_item_visit <- item_visit_by_group %>%
                filter(group == !!group)
            alias_name_list <- tibble_item_visit[[alias_name_columnName]] %>%
                unique()
            tibble_item_visit_by_alias_name <- alias_name_list %>% map(~ tibble_item_visit %>%
                filter(.data[[alias_name_columnName]] == .x))
            names(tibble_item_visit_by_alias_name) <- alias_name_list
            return(tibble_item_visit_by_alias_name)
        })
    names(res) <- item_visit_groups
    return(res)
}
EditItemVisit <- function(item_visit) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
    alias_name_columnName <- kAliasNameJapaneseColumnName
    item_visit_by_group <- item_visit %>%
        mutate(group = GetGroupSheetNames(.data[[alias_name_columnName]]))
    item_visit_by_group_list <- GetItemVisitByGroupList(item_visit_by_group, alias_name_columnName)
    unique_item_visit <- CheckIdenticalItemVisitList(item_visit_by_group_list, alias_name_columnName)
    item_visit_tibble <- unique_item_visit %>% bind_rows()
    res <- item_visit_tibble %>% select(-"group")
    return(res)
}
