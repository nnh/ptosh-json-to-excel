GetComment <- function(json_file) {
    target <- json_file$field_items %>%
        map(~ {
            if (is.null(.x$content) || .x$content == "") {
                return(NULL)
            }
            return(tibble::tibble(
                name = .x$name %||% NA,
                label = .x$label %||% NA,
                content = .x$content %||% NA
            ))
        }) %>%
        bind_rows()
    if (nrow(target) == 0) {
        return(NULL)
    }
    target$jpname <- json_file$name
    target$alias_name <- json_file$alias_name
    res <- target %>%
        select(kEngColumnNames$comment) %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    return(res)
}
