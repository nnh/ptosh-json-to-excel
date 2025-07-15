GetGroupSheetNames <- function(targetColumn) {
    return(str_remove(targetColumn, "_[0-9]+$"))
}


CheckIdenticalItemVisitList <- function(item_visit_by_group_list, alias_name_columnName) {
    sheetName_columnName <- "シート名"
    res <- item_visit_by_group_list %>% map(~ {
        outputSheetName <- ifelse(length(.x) > 1, str_remove(.x[[1]][[sheetName_columnName]][1], "\\(.*$"), .x[[1]][[sheetName_columnName]][1])
        for (i in seq_along(.x)) {
            .x[[i]][[sheetName_columnName]] <- outputSheetName
        }
        base_tibble <- .x[[1]]
        if (length(.x) == 1) {
            return(base_tibble)
        }
        rest_tibbles <- .x[-1]
        res <- base_tibble
        identical_tibble_1 <- base_tibble %>% select(-all_of(alias_name_columnName))
        different_columns <- NULL
        for (i in seq_along(rest_tibbles)) {
            identical_tibble_2 <- rest_tibbles[[i]] %>%
                select(-all_of(alias_name_columnName))
            check_tibble1 <- identical_tibble_1 %>% select(-all_of(kReferenceColnames))
            check_tibble2 <- identical_tibble_2 %>% select(-all_of(kReferenceColnames))
            if (identical(check_tibble1, check_tibble2)) {
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
ReplaceItemVisitSheetName <- function(
    item_visit_by_group_list, item_visit_tibble) {
    group_and_alias_name <- list()
    for (i in 1:length(item_visit_by_group_list)) {
        alias_names <- item_visit_by_group_list[[i]] %>%
            names()
        if (length(alias_names) == 1) {
            group_and_alias_name[[i]] <- NULL
        } else {
            group_and_alias_name[[i]] <- list()
            group_and_alias_name[[i]]$group <- item_visit_by_group_list %>%
                names() %>%
                .[i]
            group_and_alias_name[[i]]$alias_name <- alias_names
        }
    }
    group_and_alias_name <- group_and_alias_name %>%
        keep(~ !is.null(.x)) %>%
        map_df(~ tibble(group = .x$group, alias_name = .x$alias_name))
    if (nrow(group_and_alias_name) > 0) {
        # referencesを置換する
        for (col_idx in seq_along(kReferenceColnames)) {
            col_name <- kReferenceColnames[col_idx]
            for (i in 1:nrow(group_and_alias_name)) {
                group <- group_and_alias_name$group[i]
                alias_name <- group_and_alias_name$alias_name[i]
                item_visit_tibble <- item_visit_tibble %>%
                    mutate(!!col_name := str_replace_all(
                        .data[[col_name]],
                        alias_name,
                        group
                    ))
            }
        }
    }
    res <- item_visit_tibble %>% select(-"group")
    return(res)
}
EditItemVisit <- function(item_visit) {
    if (nrow(item_visit) == 0) {
        return(item_visit)
    }
    alias_name_columnName <- kAliasNameJapaneseColumnName
    item_visit_by_group <- item_visit %>%
        mutate(group = GetGroupSheetNames(.data[[alias_name_columnName]]))
    item_visit_by_group_list <- GetItemVisitByGroupList(
        item_visit_by_group, alias_name_columnName
    )
    unique_item_visit <- CheckIdenticalItemVisitList(
        item_visit_by_group_list, alias_name_columnName
    )
    item_visit_tibble <- unique_item_visit %>% bind_rows()
    res <- ReplaceItemVisitSheetName(
        item_visit_by_group_list, item_visit_tibble
    )
    return(res)
}
