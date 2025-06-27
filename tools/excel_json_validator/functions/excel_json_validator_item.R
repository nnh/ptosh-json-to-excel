#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.6.27
GetItemFromJson <- function(sheetList, jsonList, fieldItems, jpNameAndAliasName) {
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
    article_option_name <- article |> map(~ {
        df <- .
        res <- df |> map(~ {
            if (!is.list(.)) {
                return("")
            }
            option <- .$option
            if (is.null(option)) {
                return("")
            } else {
                temp <- list(option.name = option$name)
                return(temp)
            }
        })
        return(res)
    })
    article_validatores <- article |> map(~ {
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
                append(list(default_value = article[[i]][[j]]$default_value)) |>
                append(article_option_name[[i]][[j]]) |>
                append(article_validatores[[i]][[j]])
            list_items[[i]][[j]] <- list_items[[i]][[j]] %>% keep(~ !is.null(.) && . != "")
        }
    }
    names(list_items) <- nameAndAliasname |> map_chr(~ .$alias_name)
    sheetName <- "item"
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
    itemCols <- output_items |> colnames()
    df_items <- list_items |> flatten_df()
    template_df_items <- tibble(!!!setNames(rep(list(NA), length(itemCols)), itemCols))
    df_items <- template_df_items |>
        bind_rows(df_items) |>
        filter(!is.na(jpname))
    nameAndLable <- df_items |> select("jpname", "alias_name", "name", "label")
    test_df_items <- df_items |>
        GetRefBefAft("before") |>
        GetRefBefAft("after") |>
        GetRefBefAft("formula_if") |>
        GetRefBefAft("presence_if")
    return(list(json = test_df_items, sheet = output_items))
}
