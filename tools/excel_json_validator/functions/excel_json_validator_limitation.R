#' test script
#'
#' @file excel_json_validator_limitation.R
#' @author Mariko Ohtsuka
#' @date 2025.12.16
CheckLimitation <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    sheet <- sheet %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    json <- GetLimitationFromJson()
    sheet <- sheet %>% arrange(alias_name, name)
    sheet[["normal_range.less_than_or_equal_to"]] <- ifelse(sheet[["normal_range.less_than_or_equal_to"]] == "1e+06", "1000000", sheet[["normal_range.less_than_or_equal_to"]])
    json <- json %>% arrange(alias_name, name)
    return(CheckTarget(sheet, json))
}
GetLimitationFromJson <- function() {
    limitation_sheets <- target_json[["sheets"]]
    sheetsIdx <- seq(length(limitation_sheets), 1)
    for (sheetIdx in sheetsIdx) {
        field_items <- limitation_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            limitation_sheets[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItemIdx in fieldItems_idx) {
            if (!is.null(limitation_sheets[[sheetIdx]]$field_items[[fieldItemIdx]][["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]])) {
                next
            }
            if (!is.null(limitation_sheets[[sheetIdx]]$field_items[[fieldItemIdx]][["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]])) {
                next
            }
            if (!is.null(limitation_sheets[[sheetIdx]]$field_items[[fieldItemIdx]][["normal_range"]][["less_than_or_equal_to"]])) {
                next
            }
            if (!is.null(limitation_sheets[[sheetIdx]]$field_items[[fieldItemIdx]][["normal_range"]][["greater_than_or_equal_to"]])) {
                next
            }
            limitation_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
        }
        field_items <- limitation_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            limitation_sheets[[sheetIdx]] <- NULL
            next
        }
    }
    if (length(limitation_sheets) == 0) {
        return(NULL)
    }
    limitation <- limitation_sheets %>%
        map(~ {
            aliasName <- .x[["alias_name"]]
            res <- .x[["field_items"]] %>%
                map(~ {
                    res <- tibble(
                        alias_name = aliasName,
                        name = .x[["name"]],
                        label = .x[["label"]],
                        default_value = .x[["default_value"]],
                        normal_range.less_than_or_equal_to = .x[["normal_range"]][["less_than_or_equal_to"]],
                        normal_range.greater_than_or_equal_to = .x[["normal_range"]][["greater_than_or_equal_to"]],
                        validators.numericality.validate_numericality_less_than_or_equal_to = .x[["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]],
                        validators.numericality.validate_numericality_greater_than_or_equal_to = .x[["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]]
                    )
                    return(res)
                }) %>%
                bind_rows()
            return(res)
        }) %>%
        bind_rows()
    df2 <- JoinVisitGroupsValidator(limitation, key = "alias_name", target = "group") %>% distinct()
    res <- GetItemsSelectColnames(df2, c(
        "jpname", "alias_name", "name", "label", "default_value", "normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to",
        "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to"
    ), jpNameAndGroup)
    res <- res %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    res <- res %>% filter(
        normal_range.less_than_or_equal_to != "" |
            normal_range.greater_than_or_equal_to != "" |
            validators.numericality.validate_numericality_less_than_or_equal_to != "" |
            validators.numericality.validate_numericality_greater_than_or_equal_to != ""
    )
    return(res)
}
