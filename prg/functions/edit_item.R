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
EditItem <- function(field_items) {
    target <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x$name,
            label = .x$label,
            option.name = .x$option$name,
            default_value = .x$default_value,
            validators.presence.validate_presence_if = .x$validators$presence$validate_presence_if,
            validators.formula.validate_formula_if = .x$validators$formula$validate_formula_if,
            #            formula_if_references = .x$formula_if_references,
            validators.formula.validate_formula_message = .x$validators$formula$validate_formula_message,
            validators.date.validate_date_after_or_equal_to = .x$validators$date$validate_date_after_or_equal_to,
            #            references_after = .x$references_after,
            validators.date.validate_date_before_or_equal_to = .x$validators$date$validate_date_before_or_equal_to,
            #            references_before = .x$references_before
        )
        return(res)
    })
}
