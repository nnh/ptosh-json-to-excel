#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.7.16
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
GetItem_item_visit <- function(sheetList, jsonList, fieldItems) {
    sheetName <- "item_visit"
    varName <- "numericality_normal_range_check"
    # sheet
    sheet <- sheetName %>% GetItemFromSheet(sheetList, .)
    # json
    json_items <- GetItemFromJson(fieldItems, jsonList, sheet) %>%
        select(-all_of(varName))
    check_items <- GetItemVisitCheckItemsFromJson(fieldItems)
    json <- EditOutputJsonItems(
        target = check_items,
        json = json_items,
        colName = varName,
        sheet_colnames = sheet |> colnames(),
        na_convert_targets = c("option.name", "default_value")
    )
    item_visit_jsonList <- jsonList %>%
        keep(~ .x[["category"]] == "visit") %>%
        names() %>%
        tibble(alias_name = .)
    if (all(sheet %>% unlist() %>% is.na() | sheet %>% unlist() == "")) {
        if (nrow(item_visit_jsonList) == 0) {
            return(NULL)
        }
    }
    item_visit_jsonList$group <- item_visit_jsonList$alias_name %>%
        str_remove("_[0-9]+$")
    groups <- item_visit_jsonList$group %>%
        unique()
    target_item_visit <- tibble()
    for (i in 1:length(groups)) {
        group <- groups[i]
        temp <- item_visit_jsonList %>%
            filter(group == groups[i])
        alias_name_and_group <- list()
        alias_name_text <- temp$alias_name %>%
            paste0(collapse = ", ")
        target_item_visit <- bind_rows(target_item_visit, temp[1, ])
        target_item_visit[[i, "group_count"]] <- nrow(temp)
        target_item_visit[[i, "alias_name_text"]] <- alias_name_text
    }
    json <- json %>% filter(alias_name %in% target_item_visit$alias_name)

    kReferenceColnames <- c(
        "presence_if_references",
        "formula_if_references",
        "references_after",
        "references_before"
    )
    for (i in 1:nrow(target_item_visit)) {
        group_count <- target_item_visit[i, "group_count"]
        if (group_count == 1) {
            next
        }
        aliasName <- target_item_visit[i, "alias_name", drop = TRUE]
        group <- target_item_visit[i, "group", drop = TRUE]
        alias_name_text <- target_item_visit[i, "alias_name_text", drop = TRUE]
        target <- json %>%
            filter(alias_name == aliasName)
        non_target <- json %>%
            filter(alias_name != aliasName)
        target$alias_name <- alias_name_text
        target$jpname <- target$jpname %>% str_remove("\\(.*\\)$")
        json <- bind_rows(non_target, target)
        for (j in 1:length(kReferenceColnames)) {
            colname <- kReferenceColnames[j]
            json[[colname]] <- json[[colname]] %>%
                str_replace_all(aliasName, group)
        }
    }
    json <- json %>% arrange(alias_name, name)
    sheet <- sheet %>% arrange(alias_name, name)
    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
