GetDate <- function(field_items) {
    target <- field_items %>% keep(
        ~ (
            !is.null(.x[["validators"]][["date"]][["validate_date_after_or_equal_to"]]) || !is.null(.x[["validators"]][["date"]][["validate_date_before_or_equal_to"]])
        )
    )
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditDate <- function(field_items, alias_name) {
    target <- field_items %>% map_df(~ {
        references_after <- GetFieldText(.x[["validators"]][["date"]][["validate_date_after_or_equal_to"]], alias_name)
        references_before <- GetFieldText(.x[["validators"]][["date"]][["validate_date_before_or_equal_to"]], alias_name)
        res <- tibble::tibble(
            name = .x[["name"]],
            label = .x[["label"]],
            validators.date.validate_date_after_or_equal_to = .x[["validators"]][["date"]][["validate_date_after_or_equal_to"]] %||% NA,
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = .x[["validators"]][["date"]][["validate_date_before_or_equal_to"]] %||% NA,
            references_before = references_before %||% NA
        )
        return(res)
    })
}
