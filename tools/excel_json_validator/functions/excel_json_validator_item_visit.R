#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.7.7
GetItemVisitCheckItemsFromJson <- function(item_visit_fieldItems) {
    article <- item_visit_fieldItems |> GetItemArticleFromFieldItems()
    checkItems <- article |>
        map(~ {
            df <- .
            res <- df |>
                map(~ {
                    target <- .x
                    if (!is.list(target)) {
                        return(NULL)
                    }
                    numericality <- target[["validators"]][["numericality"]]
                    hasNumericality <- !is.null(numericality)
                    normalRangeLessThan <- target[["normal_range"]][["less_than_or_equal_to"]]
                    normalRangeGreaterThan <- target[["normal_range"]][["greater_than_or_equal_to"]]
                    # normalRangeLessThanかnormalRangeGreaterThanのどちらもNULLならFalse
                    hasNormalRange <- FALSE
                    if (is.null(normalRangeLessThan) && is.null(normalRangeGreaterThan)) {
                        hasNormalRange <- FALSE
                    } else if ((is.null(normalRangeLessThan) || normalRangeLessThan == "") &&
                        (is.null(normalRangeGreaterThan) || normalRangeGreaterThan == "")) {
                        # どちらも空白ならFalse
                        hasNormalRange <- FALSE
                    } else {
                        # どちらか一つでも値が入っていたらTrue
                        hasNormalRange <- TRUE
                    }
                    # 判定結果を変数に格納
                    field_type_result <- NULL
                    if (!hasNumericality && !hasNormalRange) {
                        field_type_result <- "条件なし"
                    } else if (hasNumericality && hasNormalRange) {
                        field_type_result <- "数値・アラート有"
                    } else if (hasNumericality && !hasNormalRange) {
                        field_type_result <- "数値チェック有"
                    } else if (!hasNumericality && hasNormalRange) {
                        field_type_result <- "アラート設定有"
                    }
                    if (!is.null(field_type_result)) {
                        return(list(
                            field_id = target[["name"]],
                            field_type = field_type_result
                        ))
                    } else {
                        return(NULL)
                    }
                }) %>%
                keep(~ !is.null(.) && length(.) > 0)
            return(res)
        }) |>
        keep(~ length(.) > 0)

    result <- CreateItemsByTargetTibble(
        checkItems,
        id_col = "field_id",
        type_col = "field_type"
    )
    result[["numericality_normal_range_check"]] <- result[["field_type"]]
    result <- result %>% select(-"field_type")
    return(result)
}
GetFieldItemsItemVisitByJsonList <- function(item_visit_jsonList, jpNameAndAliasName) {
    if (length(item_visit_jsonList) == 0) {
        return(NULL)
    }
    item_visit_fieldItems <- item_visit_jsonList |> GetFieldItemsByJsonList()

    alias_names <- item_visit_jsonList |> names()
    groups <- alias_names %>%
        str_remove("_[0-9]+$") %>%
        unique()
    for (i in 1:length(groups)) {
        group <- groups[i]
        target_alias_names <- alias_names %>%
            str_match_all(str_c("^", group, "_[0-9]+$")) %>%
            unlist()
        if (length(target_alias_names) < 2) {
            groups[i] <- NA
        }
    }
    groups <- groups[!is.na(groups)]
    target_fieldItems <- item_visit_fieldItems %>% map(~ {
        fieldItems <- .x
        res <- fieldItems %>% map(~ {
            # 必要な要素だけ抽出し、他は削除
            res <- .x[c("name", "label", "option", "default_value", "validators", "type")]
            if (!is.null(res[["option"]]) && is.list(res[["option"]])) {
                temp_option_name <- res[["option"]][["name"]]
                res[["option"]] <- NULL
                res[["option"]][["name"]] <- temp_option_name
            }
            # 要素がなくてもエラーにならないようにNULLを許容
            res <- res[!sapply(res, is.null)]
            return(res)
        })
        return(res)
    })
    for (group_count in 1:length(groups)) {
        group <- groups[group_count]
        target_alias_names <- alias_names[str_starts(alias_names, group)]
        target_items <- target_fieldItems[names(target_fieldItems) %in% target_alias_names]
        base_items <- target_items[[1]]
        duplicated_check <- TRUE
        for (i in 2:length(target_items)) {
            if (!identical(base_items, target_items[[i]])) {
                duplicated_check <- FALSE
                print(str_c(
                    "Validation error in item_visit: ",
                    group, " has different items: ",
                    paste0(target_alias_names, collapse = ", ")
                ))
                break
            }
        }
        if (duplicated_check) {
            target_fieldItems[[group]] <- base_items
            for (i in 1:length(target_alias_names)) {
                target_fieldItems[[target_alias_names[[i]]]] <- NULL
            }
        }
    }
    res <- target_fieldItems
    for (i in 1:length(res)) {
        groupName <- names(res)[i]
        matched_row <- jpNameAndAliasName %>% filter(alias_name == groupName)
        if (nrow(matched_row) == 0) {
            matched_row <- jpNameAndAliasName %>%
                filter(startsWith(alias_name, groupName)) %>%
                .[1, , drop = FALSE]
            matched_row[1, "alias_name"] <- groupName
            matched_row[1, "jpname"] <- matched_row[1, "jpname"] %>% str_remove("\\(.*\\)$")
        }
        if (nrow(matched_row) == 0) {
            stop(str_c("No matching alias_name found for groupName: ", groupName))
        }
        jpName <- matched_row[1, "jpname", drop = TRUE]
        aliasName <- matched_row[1, "alias_name", drop = TRUE]
        for (j in 1:length(res[[i]])) {
            res[[i]][[j]][["jpname"]] <- jpName
            res[[i]][[j]][["alias_name"]] <- aliasName
        }
    }
    return(res)
}
GetItem_item_visit <- function(sheetList, item_visit_jsonList, item_visit_fieldItems) {
    # sheet
    sheet <- "item_visit" %>% GetItemFromSheet(sheetList, .)
    sheetEmpty <- nrow(sheet) == 1 && all(is.na(sheet) | sheet == "")
    if (sheetEmpty && length(item_visit_jsonList) == 0) {
        return(NULL)
    }
    # json
    item_visit_fieldItems_alias_names <- item_visit_fieldItems |> names()
    item_visit_jsonList_matched <- item_visit_jsonList[names(item_visit_jsonList) %in% item_visit_fieldItems_alias_names]
    item_visit_jsonList_unmatched <- item_visit_jsonList[!names(item_visit_jsonList) %in% item_visit_fieldItems_alias_names]
    unmatched_alias_names <- setdiff(item_visit_fieldItems_alias_names, names(item_visit_jsonList_matched))
    target_visit_jsonList_unique <- list()
    convert_alias_name_list <- list()
    for (alias_name in unmatched_alias_names) {
        join_alias_name <- names(item_visit_jsonList) %>%
            str_match_all(str_c("^", alias_name, "_[0-9]+$")) %>%
            unlist() %>%
            paste0(collapse = ", ")
        convert_alias_name_list[[alias_name]] <- list()
        convert_alias_name_list[[alias_name]][["alias_name"]] <- alias_name
        convert_alias_name_list[[alias_name]][["join_alias_name"]] <- join_alias_name
        for (target_json in item_visit_jsonList_unmatched) {
            if (startsWith(target_json[["alias_name"]], alias_name)) {
                target_visit_jsonList_unique[[alias_name]] <- target_json
                jpname <- target_json[["name"]] %>% str_remove("\\(.*\\)$")
                convert_alias_name_list[[alias_name]][["join_jpname"]] <- jpname
                target_visit_jsonList_unique[[alias_name]][["alias_name"]] <- alias_name
                break
            }
        }
    }
    if (length(convert_alias_name_list) > 0) {
        convert_alias_name_tibble <- convert_alias_name_list %>%
            map_df(~ as_tibble(.), .id = "alias_name")
    } else {
        convert_alias_name_tibble <- NULL
    }
    item_visit_jsonList <- c(item_visit_jsonList_matched, target_visit_jsonList_unique)

    json_items <- GetItemFromJson(item_visit_fieldItems, item_visit_jsonList, sheet)
    json_items <- json_items %>% select(-"numericality_normal_range_check")
    check_items <- GetItemVisitCheckItemsFromJson(item_visit_fieldItems)
    json <- EditOutputJsonItems(
        target = check_items,
        json = json_items,
        colName = "numericality_normal_range_check",
        sheet_colnames = sheet |> colnames(),
        na_convert_targets = c("option.name", "default_value")
    )
    if (!is.null(convert_alias_name_tibble)) {
        temp1 <- json %>% inner_join(
            convert_alias_name_tibble,
            by = c("alias_name" = "alias_name")
        )
        temp1 <- temp1 %>%
            select(-c("jpname", "alias_name")) %>%
            rename(jpname = join_jpname, alias_name = join_alias_name)

        temp2 <- json %>% anti_join(
            convert_alias_name_tibble,
            by = c("alias_name" = "alias_name")
        )
        temp <- bind_rows(temp1, temp2)
        json <- temp %>% select(all_of(json |> colnames()))
    }
    for (target_alias_name in unmatched_alias_names) {
        for (variableName in c("presence_if_references", "formula_if_references", "references_after", "references_before")) {
            json[[variableName]] <- json[[variableName]] %>%
                str_replace_all(str_c("^", target_alias_name, "_[0-9]+$"), target_alias_name)
        }
    }
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
