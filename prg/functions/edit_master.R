#' edit_master.R
#'
#' @file edit_master.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
GetMaster <- function(field_items, sheet) {
    condition_column <- .const[["kLinkType"]]
    dfname <- .const[["kMaster"]]
    target <- field_items %>%
        map(~ {
            if (is.null(.x[[condition_column]]) || .x[[condition_column]] == "") {
                return(NULL)
            }
            return(tibble::tibble(
                name = PluckOrNA(.x, .const[["kFieldItemsFieldId"]]),
                label = PluckOrNA(.x, .const[["kFieldItemsFieldName"]]),
                !!condition_column := PluckOrNA(.x, condition_column)
            ))
        }) %>%
        bind_rows()
    if (nrow(target) == 0) {
        return(NULL)
    }
    res <- JoinJpnameAndAliasNameAndSelectColumns(target, dfname, sheet)

    return(res)
}
