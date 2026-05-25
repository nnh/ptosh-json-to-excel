#' edit_master.R
#'
#' @file edit_master.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
GetMaster <- function(field_items, sheet) {
    condition_column <- "link_type"
    dfname <- "master"
    target <- field_items %>%
        map(~ {
            if (is.null(.x[[condition_column]]) || .x[[condition_column]] == "") {
                return(NULL)
            }
            return(tibble::tibble(
                name = .x[[.const[["kFieldItemsFieldId"]]]] %||% NA,
                label = .x[[.const[["kFieldItemsFieldName"]]]] %||% NA,
                !!condition_column := .x[[condition_column]] %||% NA
            ))
        }) %>%
        bind_rows()
    if (nrow(target) == 0) {
        return(NULL)
    }
    res <- JoinJpnameAndAliasNameAndSelectColumns(target, dfname, sheet)

    return(res)
}
