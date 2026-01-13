#' test script
#'
#' @file excel_json_validator_options.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9

CheckOption <- function(sheetList, fieldItems, sheetName) {
    target_colnames <- engToJpnColumnMappings[[sheetName]] %>% RemoveSheetFieldSeqColumnFromVec()
    sheet <- sheetList[[sheetName]] |>
        rename(all_of(target_colnames))
    json <- GetOptionFromJson(fieldItems)

    return(CheckTarget(sheet, json))
}
GetOptionFromJson <- function(fieldItems) {
    dummy_res <- data.frame(
        jpname = "",
        alias_name = "",
        option_name = "",
        name = "",
        seq = "",
        code = "",
        is_usable = ""
    )
    if (length(fieldItems) == 0) {
        return(dummy_res)
    }
    option_fieldItems <- fieldItems
    options <- tibble()
    for (i in seq_along(option_fieldItems)) {
        if (length(option_fieldItems[[i]]) == 0) {
            next
        }
        aliasName <- names(option_fieldItems)[i]
        temp <- visitGroups %>% filter(alias_name == aliasName)
        if (!is.na(temp$group[1])) {
            aliasName <- temp$group[1]
        }
        fieldItem_idx <- seq(length(option_fieldItems[[i]]), 1)
        for (j in fieldItem_idx) {
            if (option_fieldItems[[i]][[j]][["type"]] != "FieldItem::Article") {
                option_fieldItems[[i]][[j]] <- NULL
                next
            }
            if (is.null(option_fieldItems[[i]][[j]][["option_name"]])) {
                option_fieldItems[[i]][[j]] <- NULL
                next
            }
            target_options <- options_json %>% keep(~ .x[["name"]] %in% option_fieldItems[[i]][[j]][["option_name"]])
            if (length(target_options) == 0) {
                stop(str_c("Option with name ", option_fieldItems[[i]][[j]][["option_name"]], " not found in options_json."))
            }
            option_values <- target_options[[1]][["values"]] |>
                keep(~ .x[["is_usable"]]) %>%
                bind_rows()
            fieldId <- option_fieldItems[[i]][[j]][["name"]]
            sheetAndFieldOrders <- visitGroupSheetAndFieldOrders %>% filter(alias_name == aliasName & field_id == fieldId)
            if (nrow(sheetAndFieldOrders) == 0) {
                stop(str_c("Sheet and Field order not found for alias_name: ", aliasName, ", field_id: ", fieldId))
            }
            sheet_seq <- sheetAndFieldOrders %>% pull(seq)
            field_seq <- sheetAndFieldOrders %>% pull(field_seq)
            option_values$sheet.seq <- sheet_seq
            option_values$field_item.seq <- field_seq
            option_values$option_name <- option_fieldItems[[i]][[j]][["option_name"]]
            option_values$alias_name <- aliasName
            options <- bind_rows(options, option_values)
        }
    }
    df2 <- JoinVisitGroupsValidator(options, key = "alias_name", target = "group")
    df3 <- df2 %>% arrange(sheet.seq, field_item.seq, seq)
    res <- GetItemsSelectColnames(df3, c("jpname", "alias_name", "option_name", "name", "seq", "code", "is_usable"), jpNameAndGroup) %>% distinct()
    target_colnames <- engToJpnColumnMappings[["option"]] %>%
        RemoveSheetFieldSeqColumnFromVec() %>%
        names()
    colnames(res) <- target_colnames
    res$option.values_seq <- res$option.values_seq %>% as.numeric()

    return(res)
}
