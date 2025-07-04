#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.7.4
GetItemArticleFromFieldItems <- function(fieldItems) {
    article <- fieldItems |>
        map(~ {
            df <- .
            res <- df |>
                map(~ {
                    if (.$type == "FieldItem::Article") {
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
            validators <- .$validators
            if (is.null(validators)) {
                return("")
            } else {
                validatorsDate <- validators$date
                temp <- list()
                temp <- temp |>
                    append(validators$presence) |>
                    append(validators$formula)
                if (!is.null(validatorsDate$validate_date_after_or_equal_to)) {
                    temp2 <- list(validate_date_after_or_equal_to = validatorsDate$validate_date_after_or_equal_to)
                    temp <- temp |> append(temp2)
                }
                if (!is.null(validatorsDate$validate_date_before_or_equal_to)) {
                    temp2 <- list(validate_date_before_or_equal_to = validatorsDate$validate_date_before_or_equal_to)
                    temp <- temp |> append(temp2)
                }
                return(temp)
            }
        })
        return(res)
    })
    return(article_validators)
}

CreateItemListItemsCleanEntry <- function(x) {
    x %>%
        map(~ {
            named_x <- .x[!is.null(names(.x)) & names(.x) != ""]
            tibble(!!!named_x)
        }) %>%
        bind_rows()
}

CreateItemListItems <- function(jsonList, article, article_option_name, article_validators) {
    nameAndAliasname <- jsonList |>
        map(~ list(jpname = .$name, alias_name = .$alias_name)) |>
        keep(~ !is.null(article[[.$alias_name]]))
    list_items <- list()
    for (i in 1:length(nameAndAliasname)) {
        list_items[[i]] <- list()
        for (j in 1:length(article_option_name[[i]])) {
            list_items[[i]][[j]] <- list()
            list_items[[i]][[j]] <- list_items[[i]][[j]] |>
                append(nameAndAliasname[[i]]) |>
                append(list(name = article[[i]][[j]]$name)) |>
                append(list(label = article[[i]][[j]]$label)) |>
                append(list(
                    default_value = if (is.null(article[[i]][[j]]$default_value)) {
                        "NA"
                    } else {
                        article[[i]][[j]]$default_value
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
    names(list_items) <- nameAndAliasname |> map_chr(~ .$alias_name)
    return(list_items)
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

GetItemFromJson <- function(fieldItems, jsonList, template_df_items) {
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
                    if (!is.null(.$field_type) && !.$field_type %in% c("text", "text_area")) {
                        return(NULL)
                    }
                    numericality <- .$validators$numericality
                    if (!is.null(numericality)) {
                        field_type <- "数値"
                    } else {
                        field_type <- "テキスト"
                    }
                    return(list(
                        field_id = .$name,
                        field_type = field_type
                    ))
                }) %>%
                keep(~ !is.null(.) && length(.) > 0)
            return(res)
        }) |>
        keep(~ length(.) > 0)
    result <- imap(
        fieldTypes,
        function(entries, alias_name) {
            map_dfr(
                entries,
                function(entry) {
                    tibble(
                        alias_name = alias_name,
                        name = entry$field_id,
                        field_type = entry$field_type
                    )
                }
            )
        }
    ) %>% bind_rows()
    return(result)
}
GetItem_item <- function(sheetList, jsonList, fieldItems) {
    # sheet
    sheet_name <- "item"
    sheet <- GetItemFromSheet(sheetList, sheet_name)
    # json
    template <- CreateItemTemplateTibble(sheet |> colnames())
    json_items <- GetItemFromJson(fieldItems, jsonList, template) %>% select(-"field_type")
    field_types <- GetItemFieldTypeFromJson(fieldItems)
    # left_join by alias_name and name
    if (nrow(field_types) == 0) {
        # field_typesが空なら、json_itemsの行数分のNAのfield_type列を追加
        json <- json_items %>%
            mutate(field_type = NA)
    } else {
        json <- json_items %>%
            left_join(field_types, by = c("alias_name", "name"))
    }
    # sheetと同じ列だけ出す
    json <- json %>% select(colnames(sheet))
    # "NA" -> NA
    json <- json %>%
        mutate(across(
            c(option.name, default_value),
            ~ na_if(., "NA")
        ))
    result <- list(
        sheet = sheet,
        json = json
    )
    return(result)
}
GetItem_item_visit <- function() {

}
