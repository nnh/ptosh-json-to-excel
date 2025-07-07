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
                    if (!is.list(.)) {
                        return(NULL)
                    }
                    numericality <- .[["validators"]][["numericality"]]
                    hasNumericality <- !is.null(numericality)
                    normalRangeLessThan <- .[["normal_range"]][["less_than_or_equal_to"]]
                    normalRangeGreaterThan <- .[["normal_range"]][["greater_than_or_equal_to"]]
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
                        field_type_result <- "アラート設定有"
                    } else if (!hasNumericality && hasNormalRange) {
                        field_type_result <- "数値チェック設定有"
                    }
                    if (!is.null(field_type_result)) {
                        return(list(
                            field_id = .[["name"]],
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

    result <- CreateItemsByTargetTibble(checkItems, id_col = "field_id", type_col = "field_type")
    return(result)
}

GetItem_item_visit <- function(sheetList, item_visit_jsonList, item_visit_fieldItems) {
    # sheet
    sheet <- "item_visit" %>% GetItemFromSheet(sheetList, .)
    # json
    json_items <- GetItemFromJson(item_visit_fieldItems, item_visit_jsonList, sheet)
    check_items <- GetItemVisitCheckItemsFromJson(item_visit_fieldItems)
    json <- EditOutputJsonItems(
        target = check_items,
        json = json_items,
        colName = "field_type",
        sheet_colnames = sheet |> colnames(),
        na_convert_targets = c("option.name", "default_value")
    )
    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
