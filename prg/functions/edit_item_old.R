EditItem_old <- function(field_items, alias_name) {
    target <- field_items %>% map_df(~ {
        presence_if_references <- GetFieldText(.x$validators$presence$validate_presence_if, alias_name)
        formula_if_references <- GetFieldText(.x$validators$formula$validate_formula_if, alias_name)
        references_after <- GetFieldText(.x$validators$date$validate_date_after_or_equal_to, alias_name)
        references_before <- GetFieldText(.x$validators$date$validate_date_before_or_equal_to, alias_name)
        res <- tibble::tibble(
            name = .x$name,
            label = .x$label,
            option.name = .x[["option"]][["name"]] %||% NA,
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
