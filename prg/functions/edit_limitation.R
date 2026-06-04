#' edit_limitation.R
#'
#' @file edit_limitation.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
HasValueLimitation <- function(x) {
    !is.null(x) && !is.na(x) && x != ""
}
HasLimitationValidation <- function(x) {
    HasValueLimitation(PluckConst(x, .const[["kNormalRangeLessThanOrEqualTo"]])) ||
        HasValueLimitation(PluckConst(x, .const[["kNormalRangeGreaterThanOrEqualTo"]])) ||
        HasValueLimitation(PluckConst(x, .const[["kValidatorsNumericalityLessThanOrEqualTo"]])) ||
        HasValueLimitation(PluckConst(x, .const[["kValidatorsNumericalityGreaterThanOrEqualTo"]]))
}
GetLimitation <- function(field_items) {
    target <- field_items %>% keep(HasLimitationValidation)
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
            default_value = PluckOrNA(.x, .const[["kFieldItemDefaultValue"]]),
            normal_range.less_than_or_equal_to = PluckOrNA(.x, .const[["kNormalRangeLessThanOrEqualTo"]]),
            normal_range.greater_than_or_equal_to = PluckOrNA(.x, .const[["kNormalRangeGreaterThanOrEqualTo"]]),
            validators.numericality.validate_numericality_less_than_or_equal_to = PluckOrNA(.x, .const[["kValidatorsNumericalityLessThanOrEqualTo"]]),
            validators.numericality.validate_numericality_greater_than_or_equal_to = PluckOrNA(.x, .const[["kValidatorsNumericalityGreaterThanOrEqualTo"]])
        )
        return(res)
    })
    res <- JoinJpnameAndAliasNameAndSelectColumns(limitation, .const[["kLimitation"]], sheet)
    return(res)
}
