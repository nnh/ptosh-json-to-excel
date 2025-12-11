#' edit_display.R
#'
#' @file edit_display.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
GetDisplay <- function(field_items, sheet) {
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
    display <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x[["name"]],
                label = .x[["label"]],
            )
            return(res)
        })

    res <- JoinJpnameAndAliasNameAndSelectColumns("display", sheet)
    return(res)
}
