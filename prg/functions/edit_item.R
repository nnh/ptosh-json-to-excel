GetTargetByType <- function(field_items, type) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x$type)) {
                return(FALSE)
            } else if (.x$type == type) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditItem <- function(field_items, alias_name) {
    target <- field_items %>% map_df(~ {
        presence_if_references <- GetFieldText(.x$validators$presence$validate_presence_if, alias_name)
        formula_if_references <- GetFieldText(.x$validators$formula$validate_formula_if, alias_name)
        references_after <- GetFieldText(.x$validators$date$validate_date_after_or_equal_to, alias_name)
        references_before <- GetFieldText(.x$validators$date$validate_date_before_or_equal_to, alias_name)
        res <- tibble::tibble(
            name = .x$name,
            label = .x$label,
            option.name = .x$option$name,
            default_value = .x$default_value %||% NA,
            validators.presence.validate_presence_if = .x$validators$presence$validate_presence_if %||% NA,
            presence_if_references = presence_if_references %||% NA,
            validators.formula.validate_formula_if = .x$validators$formula$validate_formula_if %||% NA,
            formula_if_references = formula_if_references %||% NA,
            validators.formula.validate_formula_message = .x$validators$formula$validate_formula_message %||% NA,
            validators.date.validate_date_after_or_equal_to = .x$validators$date$validate_date_after_or_equal_to %||% NA,
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = .x$validators$date$validate_date_before_or_equal_to %||% NA,
            references_before = references_before %||% NA
        )
        return(res)
    })
}

EditRefFieldText <- function(df_sheet_field) {
    join_field_info <- dplyr::left_join(df_sheet_field, field_list, by = c("alias_name", "field_number"))
    join_field_info$text <- paste(join_field_info$alias_name, join_field_info$name, join_field_info$label, sep = ",") %>%
        paste0("(", ., ")")
    res <- join_field_info$text %>% paste(collapse = "")
    return(res)
}
ExtractAliasAndField <- function(x, thisSheetName) {
    if (str_detect(x, "^ref\\(")) {
        sheet_field <- x %>%
            str_remove_all("^ref\\(|\\)$") %>%
            str_remove_all("'") %>%
            str_split(",\\s*") %>%
            unlist()
        alias <- sheet_field[1]
        number <- as.numeric(sheet_field[2])
    } else if (str_detect(x, "^(field|f)\\d+$")) {
        alias <- thisSheetName
        number <- as.numeric(str_extract(x, "\\d+"))
    } else {
        alias <- NA_character_
        number <- NA_real_
    }
    list(alias = alias, number = number)
}
GetFieldText <- function(target, thisSheetName) {
    kFieldText <- c("ref\\('\\w[\\w\\d\\p{Punct}]*', \\d+\\)", "f\\d+", "field\\d+")
    fieldTextList <- unlist(str_extract_all(target, kFieldText))

    if (length(fieldTextList) == 0) {
        return(NULL)
    }

    df_sheet_field <- tibble::tibble(
        raw = fieldTextList
    ) %>%
        mutate(
            parsed = purrr::map(raw, ~ ExtractAliasAndField(.x, thisSheetName)),
            alias_name = purrr::map_chr(parsed, "alias"),
            field_number = purrr::map_dbl(parsed, "number")
        ) %>%
        select(-parsed)
    res <- EditRefFieldText(df_sheet_field)
    return(res)
}
