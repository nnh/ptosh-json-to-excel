#' edit_date.R
#'
#' @file edit_date.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
GetDate <- function(field_items) {
    target <- field_items %>% keep(
        ~ (
            !is.null(purrr::pluck(.x, !!!.const[["kValidateDateAfterOrEqualTo"]], .default = NULL)) ||
                !is.null(purrr::pluck(.x, !!!.const[["kValidateDateBeforeOrEqualTo"]], .default = NULL))
        )
    )
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditDate <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetDate()
    alias_name <- sheet[[.const[["kAliasName"]]]]
    date <- field_items %>% map_df(~ {
        references_after <- purrr::pluck(.x, !!!.const[["kValidateDateAfterOrEqualTo"]], .default = NULL) %>%
            GetFieldText(alias_name)
        references_before <- purrr::pluck(.x, !!!.const[["kValidateDateBeforeOrEqualTo"]], .default = NULL) %>%
            GetFieldText(alias_name)
        res <- tibble::tibble(
            name = .x[[.const[["kFieldItemsFieldId"]]]],
            label = .x[[.const[["kFieldItemsFieldName"]]]],
            validators.date.validate_date_after_or_equal_to = purrr::pluck(.x, !!!.const[["kValidateDateAfterOrEqualTo"]], .default = NA),
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = purrr::pluck(.x, !!!.const[["kValidateDateBeforeOrEqualTo"]], .default = NA),
            references_before = references_before %||% NA
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns(date, "date", sheet)
    return(res)
}
