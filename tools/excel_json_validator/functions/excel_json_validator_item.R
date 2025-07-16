#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.7.7
GetItemFieldTypeFromJson <- function(fieldItems) {
    article <- fieldItems |> GetItemArticleFromFieldItems()
    fieldTypes <- article |>
        map(~ {
            df <- .
            res <- df |>
                map(~ {
                    if (!is.list(.)) {
                        return(NULL)
                    }
                    if (!is.null(.[["field_type"]]) && !.[["field_type"]] %in% c("text", "text_area")) {
                        return(NULL)
                    }
                    numericality <- .[["validators"]][["numericality"]]
                    if (!is.null(numericality)) {
                        field_type <- "数値"
                    } else {
                        field_type <- "テキスト"
                    }
                    return(list(
                        field_id = .[["name"]],
                        field_type = field_type
                    ))
                }) %>%
                keep(~ !is.null(.) && length(.) > 0)
            return(res)
        }) |>
        keep(~ length(.) > 0)
    result <- CreateItemsByTargetTibble(fieldTypes, id_col = "field_id", type_col = "field_type")
    return(result)
}
GetItem_item <- function(sheetList, jsonList, fieldItems) {
    sheetName <- "item"
    varName <- "field_type"
    # sheet
    sheet <- sheetName %>% GetItemFromSheet(sheetList, .)
    # json
    json_items <- GetItemFromJson(fieldItems, jsonList, sheet) %>% select(-all_of(c(varName)))
    field_types <- GetItemFieldTypeFromJson(fieldItems)
    json <- EditOutputJsonItems(
        target = field_types,
        json = json_items,
        colName = varName,
        sheet_colnames = sheet |> colnames(),
        na_convert_targets = c("option.name", "default_value")
    )
    item_jsonList <- jsonList %>%
        keep(~ .x[["category"]] != "visit") %>%
        names()
    json <- json %>% filter(alias_name %in% item_jsonList)

    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
