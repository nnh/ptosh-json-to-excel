#' edit_date.R
#'
#' @file edit_date.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
HasDateValidation <- function(x) {
    !is.null(PluckConst(x, .const[["kValidateDateAfterOrEqualTo"]])) ||
        !is.null(PluckConst(x, .const[["kValidateDateBeforeOrEqualTo"]]))
}
GetDate <- function(field_items) {
    target <- field_items %>% keep(HasDateValidation)
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditDate <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetDate()
    alias_name <- sheet[[.const[["kAliasName"]]]]
    date <- field_items %>% map_df(~ {
        references_after <- PluckConst(.x, .const[["kValidateDateAfterOrEqualTo"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        references_before <- PluckConst(.x, .const[["kValidateDateBeforeOrEqualTo"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        res <- tibble::tibble(
            name = .x[[.const[["kFieldItemsFieldId"]]]],
            label = .x[[.const[["kFieldItemsFieldName"]]]],
            validators.date.validate_date_after_or_equal_to = PluckOrNA(.x, .const[["kValidateDateAfterOrEqualTo"]]),
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = PluckOrNA(.x, .const[["kValidateDateBeforeOrEqualTo"]]),
            references_before = references_before %||% NA
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns(date, .const[["kDate"]], sheet)
    return(res)
}
