#' edit_comment.R
#'
#' @file edit_comment.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
GetComment <- function(field_items, condition_column, sheet) {
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
    if (condition_column == "content") {
        dfname <- "comment"
    } else if (condition_column == "description") {
        dfname <- "explanation"
    } else if (condition_column == "link_type") {
        dfname <- "master"
    } else {
        return(NULL)
    }
    assign(dfname, target)
    res <- JoinJpnameAndAliasNameAndSelectColumns(dfname, sheet)

    return(res)
}
