EditTitle <- function(field_items) {
    target <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[["name"]],
            label = .x[["label"]],
            level = .x[["level"]] %||% NA,
        )
        return(res)
    })
}
