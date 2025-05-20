GetAllocation <- function(json_file) {
    allocation <- json_file$allocation
    if (is.null(allocation)) {
        return(NULL)
    }
    is_zelen <- allocation$is_zelen
    zelen_imbalance <- allocation$zelen_imbalance
    is_double_blinded <- allocation$is_double_blinded
    double_blind_emails <- allocation$double_blind_emails
    allocation_method <- allocation$allocation_method
    formula_field <- json_file$field_items %>%
        map(~ .x$formula_field) %>%
        keep(~ !is.null(.x))
    formula_field_str <- if (length(formula_field) > 0) {
        paste(unlist(formula_field), collapse = ", ")
    } else {
        ""
    }
    groups <- allocation$groups %>% map_df(~ {
        if_references <- GetFieldText(.x$`if`, json_file$alias_name)
        group_tbl <- tibble::tibble(
            groups.code = .x$code,
            groups.label = .x$label,
            groups.if = .x$`if`,
            groups.if_references = if (is.null(if_references)) NA else if_references,
            groups.message = .x$message
        )
        return(group_tbl)
    })
    res <- tibble::tibble(
        is_zelen = is_zelen,
        zelen_imbalance = zelen_imbalance,
        is_double_blinded = is_double_blinded,
        double_blind_emails = double_blind_emails,
        allocation_method = allocation_method
    ) %>%
        bind_cols(groups)
    res$formula_field <- formula_field_str
    formula_field_references <- GetFieldText(formula_field_str, json_file$alias_name)
    res$formula_field_references <- if (is.null(formula_field_references)) NA else formula_field_references
    return(res)
}
