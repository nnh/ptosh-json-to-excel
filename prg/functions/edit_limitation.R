#' edit_limitation.R
#'
#' @file edit_limitation.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
HasValueLimitation <- function(x) {
    !is.null(x) && !is.na(x) && x != ""
}
GetLimitation <- function(field_items) {
    target <- field_items %>%
        keep(~ {
            normal_lte <- purrr::pluck(
                .x, !!!.const[["kNormalRangeLessThanOrEqualTo"]],
                .default = NULL
            )
            normal_gte <- purrr::pluck(
                .x, !!!.const[["kNormalRangeGreaterThanOrEqualTo"]],
                .default = NULL
            )

            num_lte <- purrr::pluck(
                .x, !!!.const[["kValidatorsNumericalityLessThanOrEqualTo"]],
                .default = NULL
            )
            num_gte <- purrr::pluck(
                .x, !!!.const[["kValidatorsNumericalityGreaterThanOrEqualTo"]],
                .default = NULL
            )

            HasValueLimitation(normal_lte) ||
                HasValueLimitation(normal_gte) ||
                HasValueLimitation(num_lte) ||
                HasValueLimitation(num_gte)
        })
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
EditLimitation <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetLimitation()
    limitation <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[[.const[["kFieldItemsFieldId"]]]],
            label = .x[[.const[["kFieldItemsFieldName"]]]],
            default_value = .x[[.const[["kFieldItemDefaultValue"]]]] %||% NA,
            normal_range.less_than_or_equal_to = purrr::pluck(.x, !!!.const[["kNormalRangeLessThanOrEqualTo"]], .default = NA),
            normal_range.greater_than_or_equal_to = purrr::pluck(.x, !!!.const[["kNormalRangeGreaterThanOrEqualTo"]], .default = NA),
            validators.numericality.validate_numericality_less_than_or_equal_to = purrr::pluck(.x, !!!.const[["kValidatorsNumericalityLessThanOrEqualTo"]], .default = NA),
            validators.numericality.validate_numericality_greater_than_or_equal_to = purrr::pluck(.x, !!!.const[["kValidatorsNumericalityGreaterThanOrEqualTo"]], .default = NA)
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns(limitation, "limitation", sheet)
    return(res)
}
