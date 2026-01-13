#' edit_assigned.R
#'
#' @file edit_assigned.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
EditAssigned <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetTargetByType("FieldItem::Assigned")
    assigned <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[[.const[["kFieldItemsFieldId"]]]],
            label = .x[[.const[["kFieldItemsFieldName"]]]],
            default_value = .x[[.const[["kFieldItemDefaultValue"]]]],
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns("assigned", sheet)
    return(res)
}
