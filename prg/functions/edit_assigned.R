EditAssigned <- function(field_items) {
    target <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x$name,
            label = .x$label,
            default_value = .x$default_value,
        )
        return(res)
    })
}
