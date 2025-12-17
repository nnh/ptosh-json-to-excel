#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.12.16
CheckItemNonVisit <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetItemNonVisitFromJson()
}
GetItemNonVisitFromJson <- function() {
    itemNonVisit <- target_json[["sheets"]] %>% keep(~ .x[["category"]] != "visit")
    sheetsIdx <- seq(length(itemNonVisit), 1)
    for (sheetIdx in sheetsIdx) {
        field_items <- itemNonVisit[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            itemNonVisit[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItem_idx in fieldItems_idx) {
            if (itemNonVisit[[sheetIdx]]$field_items[[fieldItem_idx]][["type"]] != "FieldItem::Article") {
                itemNonVisit[[sheetIdx]]$field_items[[fieldItem_idx]] <- NULL
                next
            }
        }
        if (length(itemNonVisit[[sheetIdx]]$field_items) == 0) {
            itemNonVisit[[sheetIdx]] <- NULL
            next
        }
    }
    if (length(itemNonVisit) == 0) {
        return(NULL)
    }
    item_tibble <- itemNonVisit %>%
        map(~ {
            aliasName <- .x[["alias_name"]]
            field_items <- .x[["field_items"]] %>%
                map(~ {
                    tibble(
                        alias_name = aliasName,
                        name = .x[["name"]],
                        label = .x[["label"]],
                        option_name = .x[["option_name"]] %||% NA_character_,
                        default_value = .x[["default_value"]] %||% NA_character_,
                        validators.presence.validate_presence_if = .x[["validators"]][["presence"]][["validate_presence_if"]] %||% NA_character_,
                        validators.formula.validate_formula_if = .x[["validators"]][["formula"]][["validate_formula_if"]] %||% NA_character_,
                        validators.formula.validate_formula_message = .x[["validators"]][["formula"]][["validate_formula_message"]] %||% NA_character_,
                        validators.date.validate_date_before_or_equal_to = .x[["validators"]][["date"]][["validate_date_before_or_equal_to"]] %||% NA_character_,
                        validators.date.validate_date_after_or_equal_to = .x[["validators"]][["date"]][["validate_date_after_or_equal_to"]] %||% NA_character_
                    )
                }) %>%
                bind_rows()
        }) %>%
        bind_rows()
}
# GetItemFieldTypeFromJson <- function(fieldItems) {
#     article <- fieldItems |> GetItemArticleFromFieldItems()
#     fieldTypes <- article |>
#         map(~ {
#             df <- .
#             res <- df |>
#                 map(~ {
#                     if (!is.list(.)) {
#                         return(NULL)
#                     }
#                     if (!is.null(.[["field_type"]]) && !.[["field_type"]] %in% c("text", "text_area")) {
#                         return(NULL)
#                     }
#                     numericality <- .[["validators"]][["numericality"]]
#                     if (!is.null(numericality)) {
#                         field_type <- "数値"
#                     } else {
#                         field_type <- "テキスト"
#                     }
#                     return(list(
#                         field_id = .[["name"]],
#                         field_type = field_type
#                     ))
#                 }) %>%
#                 keep(~ !is.null(.) && length(.) > 0)
#             return(res)
#         }) |>
#         keep(~ length(.) > 0)
#     result <- CreateItemsByTargetTibble(fieldTypes, id_col = "field_id", type_col = "field_type")
#     return(result)
# }
# GetItem_item <- function(sheetList, jsonList, fieldItems, sheetName) {
#     varName <- "field_type"
#     # sheet
#     sheet <- sheetName %>% GetItemFromSheet(sheetList, .)
#     # json
#     json_items <- GetItemFromJson(fieldItems, jsonList, sheet) %>% select(-all_of(c(varName)))
#     field_types <- GetItemFieldTypeFromJson(fieldItems)
#     json <- EditOutputJsonItems(
#         target = field_types,
#         json_items = json_items,
#         colName = varName,
#         sheet_colnames = sheet |> colnames(),
#         na_convert_targets = c("option.name", "default_value")
#     )
#     item_jsonList <- jsonList %>%
#         keep(~ .x[["category"]] != "visit") %>%
#         names()
#     json <- json %>% filter(alias_name %in% item_jsonList)
#
#     result <- list(
#         sheet = sheet,
#         json = json
#     )
#     return(result)
# }
#
