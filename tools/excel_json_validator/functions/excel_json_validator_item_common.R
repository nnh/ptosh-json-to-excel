#' test script
#'
#' @file excel_json_validator_common.R
#' @author Mariko Ohtsuka
#' @date 2025.7.28
CreateItemsByTargetTibble <- function(target, id_col = "field_id", type_col = "field_type") {
    result <- imap(
        target,
        function(entries, alias_name) {
            map_dfr(
                entries,
                function(entry) {
                    tibble(
                        alias_name = alias_name,
                        name = entry[[id_col]],
                        field_type = entry[[type_col]]
                    )
                }
            )
        }
    ) %>% bind_rows()
    return(result)
}
CreateItemListItemsCleanEntry <- function(x) {
    x %>%
        map(~ {
            named_x <- .x[!is.null(names(.x)) & names(.x) != ""]
            tibble(!!!named_x)
        }) %>%
        bind_rows()
}
GetItemFromSheet <- function(sheetList, sheetName) {
    temp <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    output_items <- temp |>
        rename(
            validate_formula_message = "validators.formula.validate_formula_message",
            validate_formula_if = "validators.formula.validate_formula_if",
            validate_date_after_or_equal_to = "validators.date.validate_date_after_or_equal_to",
            validate_date_before_or_equal_to = "validators.date.validate_date_before_or_equal_to",
            validate_presence_if = "validators.presence.validate_presence_if"
        )
    return(output_items)
}
CreateItemTemplateTibble <- function(item_cols) {
    template_df_items <- tibble(!!!setNames(rep(list(NA), length(item_cols)), item_cols))
    return(template_df_items)
}
GetItemArticleFromFieldItems <- function(fieldItems) {
    article <- fieldItems |>
        map(~ {
            df <- .
            res <- df |>
                map(~ {
                    if (.[["type"]] == "FieldItem::Article") {
                        return(.)
                    } else {
                        return(NULL)
                    }
                }) |>
                keep(~ !is.null(.))
        }) |>
        keep(~ length(.) > 0)
    return(article)
}
GetItemArticleOptionName <- function(article) {
    article_option_name <- article |> map(~ {
        df <- .
        res <- df |> map(~ {
            if (!is.list(.)) {
                return("")
            }
            option <- .[["option"]]
            if (is.null(option)) {
                return("")
            } else {
                temp <- list(option.name = option[["name"]])
                return(temp)
            }
        })
        return(res)
    })
    return(article_option_name)
}
GetItemArticleValidators <- function(article) {
    article_validators <- article |> map(~ {
        df <- .
        res <- df |> map(~ {
            if (!is.list(.)) {
                return("")
            }
            validators <- .[["validators"]]
            if (is.null(validators)) {
                return("")
            } else {
                validatorsDate <- validators[["date"]]
                temp <- list()
                temp <- temp |>
                    append(validators[["presence"]]) |>
                    append(validators[["formula"]])
                if (!is.null(validatorsDate[["validate_date_after_or_equal_to"]])) {
                    temp2 <- list(validate_date_after_or_equal_to = validatorsDate[["validate_date_after_or_equal_to"]])
                    temp <- temp |> append(temp2)
                }
                if (!is.null(validatorsDate[["validate_date_before_or_equal_to"]])) {
                    temp2 <- list(validate_date_before_or_equal_to = validatorsDate[["validate_date_before_or_equal_to"]])
                    temp <- temp |> append(temp2)
                }
                return(temp)
            }
        })
        return(res)
    })
    return(article_validators)
}

CreateItemListItems <- function(jsonList, article, article_option_name, article_validators) {
    nameAndAliasname <- jsonList |>
        map(~ list(jpname = .[["name"]], alias_name = .[["alias_name"]])) |>
        set_names(map_chr(jsonList, ~ .[["alias_name"]])) |>
        keep(~ {
            alias <- .[["alias_name"]]
            !is.null(alias) && !is.null(article[[alias]]) && length(article[[alias]]) > 0
        })
    list_items <- list()
    for (i in 1:length(nameAndAliasname)) {
        list_items[[i]] <- list()
        for (j in 1:length(article_option_name[[i]])) {
            list_items[[i]][[j]] <- list()
            list_items[[i]][[j]] <- list_items[[i]][[j]] |>
                append(nameAndAliasname[[i]]) |>
                append(list(name = article[[i]][[j]][["name"]])) |>
                append(list(label = article[[i]][[j]][["label"]])) |>
                append(list(
                    default_value = if (is.null(article[[i]][[j]][["default_value"]])) {
                        "NA"
                    } else {
                        article[[i]][[j]][["default_value"]]
                    }
                )) |>
                append(
                    if (!is.list(article_option_name[[i]][[j]])) {
                        list(option.name = "NA")
                    } else {
                        article_option_name[[i]][[j]]
                    }
                ) |>
                append(article_validators[[i]][[j]])
            list_items[[i]][[j]] <- list_items[[i]][[j]] %>% keep(~ !is.null(.))
        }
    }
    names(list_items) <- nameAndAliasname |> map_chr(~ .[["alias_name"]])
    return(list_items)
}
GetItemFromJson <- function(fieldItems, jsonList, sheet) {
    template_df_items <- CreateItemTemplateTibble(sheet |> colnames())
    article <- fieldItems |> GetItemArticleFromFieldItems()
    article_option_name <- article |> GetItemArticleOptionName()
    article_validators <- article |> GetItemArticleValidators()
    list_items <- jsonList |> CreateItemListItems(article, article_option_name, article_validators)
    flatten_list_items <- list_items |>
        map(CreateItemListItemsCleanEntry) %>%
        bind_rows()
    df_items <- template_df_items |>
        bind_rows(flatten_list_items) |>
        filter(!is.na(jpname))
    nameAndLable <- df_items |> select("jpname", "alias_name", "name", "label")
    json_items <- df_items |>
        GetRefBefAft("before") |>
        GetRefBefAft("after") |>
        GetRefBefAft("formula_if") |>
        GetRefBefAft("presence_if")
    return(json_items)
}
EditOutputJsonItems <- function(
    target, json_items, colName = "field_type",
    sheet_colnames,
    na_convert_targets = c("option.name", "default_value")) {
    # left_join by alias_name and name
    if (nrow(target) == 0) {
        json <- json_items %>%
            mutate(!!colName := NA)
    } else {
        json <- json_items %>%
            left_join(target, by = c("alias_name", "name"))
    }
    # sheetと同じ列だけ出す
    json <- json %>% select(any_of(sheet_colnames))
    # "NA" -> NA
    json <- json %>%
        mutate(across(all_of(na_convert_targets), ~ na_if(., "NA")))

    return(json)
}
ValidateSheetAndJsonEquality <- function(sheet, json) {
    if (nrow(sheet) != nrow(json)) {
        stop("Row count mismatch between sheet and json")
    }
    if (ncol(sheet) != ncol(json)) {
        stop("Column count mismatch between sheet and json")
    }
    for (col in 1:ncol(sheet)) {
        if (identical(as.character(sheet[, col, drop = TRUE]), as.character(json[, col, drop = TRUE]))) {
            next
        } else {
            for (row in 1:nrow(sheet)) {
                if (is.na(sheet[row, col]) && is.na(json[row, col])) {
                    next
                }
                if (sheet[row, col] != json[row, col]) {
                    stop(str_c(
                        "Validation error in item at row ", row, " and column ", col, ": ",
                        sheet[row, col], " != ", json[row, col]
                    ))
                }
            }
        }
    }
    return(TRUE)
}
ExecValidateSheetAndJsonEquality <- function(checkChecklist, sheetName) {
    if (is.null(checkChecklist[[sheetName]])) {
        return()
    }
    res <- ValidateSheetAndJsonEquality(
        checkChecklist[[sheetName]][["sheet"]],
        checkChecklist[[sheetName]][["json"]]
    )
    if (!res) {
        stop(str_c("Validation error in ", sheetName))
    }
    return(res)
}
