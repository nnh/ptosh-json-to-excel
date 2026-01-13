#' test script
#'
#' @file excel_json_validator_date.R
#' @author Mariko Ohtsuka
#' @date 2026.1.13
CheckDate <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    outputValidators <- sheet %>% select(
        jpname, alias_name, name, label, validators.date.validate_date_after_or_equal_to, references_after, validators.date.validate_date_before_or_equal_to, references_before
    )
    json <- CheckValidatorsDate()
    if (is.null(json)) {
        res <- NULL
    } else {
        test1 <- json %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
        test2 <- sheet %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
        res <- CheckTarget(test1, test2)
    }
    return(res)
}
CheckValidatorsDate <- function() {
    date_json <- target_json
    date_sheets <- date_json$sheets
    sheetsIdx <- seq(length(date_sheets), 1)
    data_tibble <- tibble()
    for (sheetIdx in sheetsIdx) {
        field_items <- date_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            date_sheets[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItemIdx in fieldItems_idx) {
            if (is.null(date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date)) {
                date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
                next
            }
            if (length(date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date) == 0) {
                date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
                next
            }
            if (is.null(date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_before_or_equal_to)) {
                date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_before_or_equal_to <- ""
            }
            if (is.null(date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_after_or_equal_to)) {
                date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_after_or_equal_to <- ""
            }
            temp <- tibble(
                alias_name = date_sheets[[sheetIdx]]$alias_name,
                name = date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$name,
                label = date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$label,
                validators.date.validate_date_before_or_equal_to = date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_before_or_equal_to,
                validators.date.validate_date_after_or_equal_to = date_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$date$validate_date_after_or_equal_to
            )
            data_tibble <- bind_rows(data_tibble, temp)
        }
    }
    for (row in 1:nrow(data_tibble)) {
        data_tibble[row, "references_before"] <- ReplaceFieldForReference(
            data_tibble[row, "validators.date.validate_date_before_or_equal_to"][[1]],
            data_tibble[row, "alias_name"][[1]],
            fieldInfoForGetReference
        )
        data_tibble[row, "references_after"] <- ReplaceFieldForReference(
            data_tibble[row, "validators.date.validate_date_after_or_equal_to"][[1]],
            data_tibble[row, "alias_name"][[1]],
            fieldInfoForGetReference
        )
    }
    df2 <- JoinVisitGroupsValidator(data_tibble, key = "alias_name", target = "group") %>% distinct()
    df3 <- df2 %>% inner_join(visitGroupSheetAndFieldOrders, by = c("alias_name" = "alias_name", "name" = "field_id"))
    df4 <- GetItemsSelectColnames(df3, c("jpname", "alias_name", "name", "label", "validators.date.validate_date_after_or_equal_to", "references_after", "validators.date.validate_date_before_or_equal_to", "references_before", "seq", "field_seq"), jpNameAndGroup)
    df5 <- df4 %>%
        arrange(seq, field_seq) %>%
        select(-seq, -field_seq)
    res <- df5

    return(res)
}
