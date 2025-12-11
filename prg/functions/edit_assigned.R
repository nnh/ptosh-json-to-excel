#' edit_assigned.R
#'
#' @file edit_assigned.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
EditAssigned <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetTargetByType("FieldItem::Assigned")
    assigned <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[["name"]],
            label = .x[["label"]],
            default_value = .x[["default_value"]],
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns("assigned", sheet)
    return(res)
}
