GetDisplay <- function(field_items) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[["type"]])) {
                return(FALSE)
            } else if (.x[["type"]] == "FieldItem::Assigned" && .x[["is_invisible"]] == FALSE) {
                return(TRUE)
            } else if (.x[["type"]] == "FieldItem::Article" && .x[["is_invisible"]] == TRUE) {
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
                label = .x[["label"]],
            )
            return(res)
        })
    return(res)
}
