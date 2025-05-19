GetVisit <- function(field_items) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x$label)) {
                return(FALSE)
            } else if (.x$label == "Visit Number") {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
