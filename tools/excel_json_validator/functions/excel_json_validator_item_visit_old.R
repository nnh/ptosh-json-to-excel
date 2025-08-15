#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.8.12
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

GetItem_item_visit_old <- function(sheetList, visit_not_visit_fieldItems, visit_not_visit_jsonList, sheetName) {
    varName <- "numericality_normal_range_check"
    # sheet
    sheet <- sheetName %>% GetItemFromSheet(sheetList, .)
    # json
    json_items <- GetItemFromJson(visit_not_visit_fieldItems, visit_not_visit_jsonList, sheet) %>%
        select(-all_of(varName))
    check_items <- GetItemVisitCheckItemsFromJson(visit_not_visit_fieldItems)
    json <- EditOutputJsonItems(
        target = check_items,
        json_items = json_items,
        colName = varName,
        sheet_colnames = sheet |> colnames(),
        na_convert_targets = c("option.name", "default_value")
    )
    item_visit_jsonList <- visit_not_visit_jsonList %>%
        keep(~ .x[["category"]] == "visit") %>%
        names() %>%
        tibble(alias_name = .)
    if (all(sheet %>% unlist() %>% is.na() | sheet %>% unlist() == "")) {
        if (nrow(item_visit_jsonList) == 0 | !isVisit) {
            return(NULL)
        }
    }
    json <- json %>%
        inner_join(item_visit_jsonList, by = "alias_name") %>%
        as.data.frame()
    json <- json %>% arrange(alias_name, name)
    sheet <- sheet %>% arrange(alias_name, name)
    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
