GetComment <- function(field_items, condition_column) {
    target <- field_items %>%
        map(~ {
            if (is.null(.x[[condition_column]]) || .x[[condition_column]] == "") {
                return(NULL)
            }
            return(tibble::tibble(
                name = .x[["name"]] %||% NA,
                label = .x[["label"]] %||% NA,
                !!condition_column := .x[[condition_column]] %||% NA
            ))
        }) %>%
        bind_rows()
    if (nrow(target) == 0) {
        return(NULL)
    }
    return(target)
}
