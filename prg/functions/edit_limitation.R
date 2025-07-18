GetLimitation <- function(field_items) {
    target <- field_items %>% keep(
        ~ (
            (
                (!is.null(.x[["normal_range"]][["less_than_or_equal_to"]]) &&
                    !is.na(.x[["normal_range"]][["less_than_or_equal_to"]]) &&
                    .x[["normal_range"]][["less_than_or_equal_to"]] != "") ||
                    (!is.null(.x[["normal_range"]][["greater_than_or_equal_to"]]) &&
                        !is.na(.x[["normal_range"]][["greater_than_or_equal_to"]]) &&
                        .x[["normal_range"]][["greater_than_or_equal_to"]] != "") ||
                    (!is.null(.x[["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]]) &&
                        !is.na(.x[["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]]) &&
                        .x[["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]] != "") ||
                    (!is.null(.x[["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]]) &&
                        !is.na(.x[["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]]) &&
                        .x[["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]] != "")
            )
        )
    )
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditLimitation <- function(field_items) {
    target <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[["name"]],
            label = .x[["label"]],
            default_value = .x[["default_value"]] %||% NA,
            normal_range.less_than_or_equal_to = .x[["normal_range"]][["less_than_or_equal_to"]] %||% NA,
            normal_range.greater_than_or_equal_to = .x[["normal_range"]][["greater_than_or_equal_to"]] %||% NA,
            validators.numericality.validate_numericality_less_than_or_equal_to = .x[["validators"]][["numericality"]][["validate_numericality_less_than_or_equal_to"]] %||% NA,
            validators.numericality.validate_numericality_greater_than_or_equal_to = .x[["validators"]][["numericality"]][["validate_numericality_greater_than_or_equal_to"]] %||% NA
        )
        return(res)
    })
}
