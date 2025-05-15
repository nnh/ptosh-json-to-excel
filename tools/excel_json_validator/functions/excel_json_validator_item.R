#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
GetRefBefAft <- function(target, befAft) {
    targetAliasNameAndNameAndLabel <<- target |>
        select(c("alias_name", "name", "label")) |>
        distinct()
    target$test <- NA
    testList <- list()
    if (befAft == "before" | befAft == "after") {
        target_colname <- str_c("validate_date_", befAft, "_or_equal_to")
        output_colname <- str_c("references_", befAft)
    } else {
        target_colname <- str_c("validate_", befAft)
        output_colname <- str_c(befAft, "_references")
    }
    for (i in 1:length(target[[target_colname]])) {
        if (!is.na(target[[target_colname]][[i]])) {
            temp <- target[[target_colname]][[i]] %>% gsub("f(\\d+)", "field\\1", .)
            if (str_detect(temp, "field[0-9]*")) {
                testList[[i]] <- temp |> str_extract_all("field[0-9]*")
            } else {
                testList[[i]] <- NA
            }
        } else {
            testList[[i]] <- NA
        }
    }

    for (i in 1:length(testList)) {
        if (!is.na(testList[[i]])) {
            test_item <- testList[[i]][[1]] |> unique()
            refText <- ""
            for (j in 1:length(test_item)) {
                temp_test <- targetAliasNameAndNameAndLabel |> filter(alias_name == target[i, "alias_name", drop = T] & name == test_item[j])
                refText <- str_c(refText, "(", str_c(temp_test, collapse = ","), ")")
                target[i, "test"] <- refText
            }
        }
    }

    output_df <- target
    output_df[[output_colname]] <- output_df$test
    output_df <- output_df |> select(-c("test"))
    return(output_df)
}
GetItemFromJson <- function(sheetList, jsonList) {
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
