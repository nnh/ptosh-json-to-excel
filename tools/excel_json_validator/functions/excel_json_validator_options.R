#' test script
#'
#' @file excel_json_validator_options.R
#' @author Mariko Ohtsuka
#' @date 2025.12.23

CheckOption <- function(sheetList, fieldItems, sheetName) {
    target_colnames <- engToJpnColumnMappings[[sheetName]] %>% RemoveSheetFieldSeqColumnFromVec()
    sheet <- sheetList[[sheetName]] |>
        rename(all_of(target_colnames))
    json <- GetOptionFromJson(fieldItems)
    sheet <- sheet %>% arrange(alias_name, option.name, option.values_seq)
    json <- json %>% arrange(alias_name, option.name, option.values_seq)
    json$option.values_seq <- json$option.values_seq %>% as.integer()
    sheet$option.values_seq <- sheet$option.values_seq %>% as.integer()
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
            option_values$option_name <- option_fieldItems[[i]][[j]][["option_name"]]
            option_values$alias_name <- aliasName
            options <- bind_rows(options, option_values)
        }
    }
    df2 <- JoinVisitGroupsValidator(options, key = "alias_name", target = "group")
    res <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "option_name", "name", "seq", "code", "is_usable"), jpNameAndGroup)
    target_colnames <- engToJpnColumnMappings[["option"]] %>%
        RemoveSheetFieldSeqColumnFromVec() %>%
        names()
    colnames(res) <- target_colnames

    return(res)
}
