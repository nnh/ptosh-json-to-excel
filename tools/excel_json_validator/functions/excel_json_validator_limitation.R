#' test script
#'
#' @file excel_json_validator_limitation.R
#' @author Mariko Ohtsuka
#' @date 2026.1.13
CheckLimitation <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    sheet <- sheet %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    json <- GetLimitationFromJson()
    if (is.null(json) || nrow(json) == 0) {
        json <- data.frame(
            jpname = "",
            alias_name = "",
            name = "",
            label = "",
            default_value = "",
            normal_range.less_than_or_equal_to = "",
            normal_range.greater_than_or_equal_to = "",
            validators.numericality.validate_numericality_less_than_or_equal_to = "",
            validators.numericality.validate_numericality_greater_than_or_equal_to = "",
            stringsAsFactors = FALSE
        )
    }
    sheet[["normal_range.less_than_or_equal_to"]] <- ifelse(sheet[["normal_range.less_than_or_equal_to"]] == "1e+06", "1000000", sheet[["normal_range.less_than_or_equal_to"]])
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
    df3 <- df2 %>% inner_join(visitGroupSheetAndFieldOrders, by = c("alias_name" = "alias_name", "name" = "field_id"))
    df4 <- GetItemsSelectColnames(df3, c(
        "jpname", "alias_name", "name", "label", "default_value", "normal_range.less_than_or_equal_to", "normal_range.greater_than_or_equal_to",
        "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to", "seq", "field_seq"
    ), jpNameAndGroup)
    df5 <- df4 %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    df6 <- df5 %>% filter(
        normal_range.less_than_or_equal_to != "" |
            normal_range.greater_than_or_equal_to != "" |
            validators.numericality.validate_numericality_less_than_or_equal_to != "" |
            validators.numericality.validate_numericality_greater_than_or_equal_to != ""
    )
    res <- df6 %>%
        arrange(seq, field_seq) %>%
        select(-seq, -field_seq)
    return(res)
}
