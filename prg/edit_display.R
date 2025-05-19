GetDisplay <- function(json_file) {
    target <- json_file$field_items %>%
        keep(~ {
            if (is.null(.x$type)) {
                return(FALSE)
            } else if (.x$type == "FieldItem::Assigned" && .x$is_invisible == FALSE) {
                return(TRUE)
            } else if (.x$type == "FieldItem::Article" && .x$is_invisible == TRUE) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    res$jpname <- json_file$name
    res$alias_name <- json_file$alias_name
    res <- res %>% select(kEngColumnNames$display)
}
