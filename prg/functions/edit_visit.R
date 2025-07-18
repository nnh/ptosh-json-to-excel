GetVisit <- function(field_items) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[["label"]])) {
                return(FALSE)
            } else if (.x[["label"]] == "Visit Number") {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    res <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x[["name"]],
                default_value = .x[["default_value"]] %||% NA,
            )
            return(res)
        })
    return(res)
}
