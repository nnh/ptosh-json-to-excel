GetItem <- function(json_file) {
    keep_elements <- c(
        "name",
        "label",
        "option",
        "default_value",
        "validators"
    )
    target_field_items <- json_file$field_items %>%
        keep(~ !is.null(.x$type) && .x$type == "FieldItem::Article") %>%
        map(function(x) {
            item <- set_names(map(keep_elements, ~ x[[.x]]), keep_elements)
            item$option.name <- x$option$name
            item$validators.presence.validate_presence_if <- x$validators$presence$validate_presence_if
            item$validators.formula.validate_formula_if <- x$validators$formula$.validate_formula_if
            item$date.validate_date_after_or_equal_to <- x$validators$date$.validate_date_after_or_equal_to
            item$date.validate_date_before_or_equal_to <- x$validators$date$.validate_date_before_or_equal_to
            item$option <- NULL
            item$validators <- NULL
            return(item)
        })
    res_field_items <- bind_rows(target_field_items)
    res$jpname <- json_file$name
    res$alias_name <- json_file$alias_name
    return(res_field_items)
}
