GetPresence <- function(field_items) {
    target <- field_items %>%
        GetTargetByType("FieldItem::Article") %>%
        keep(~ is.null(.x$validators$presence))
    if (length(target) == 0) {
        return(NULL)
    }
    res <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x$name,
                label = .x$label,
            )
            return(res)
        })
    return(res)
}
