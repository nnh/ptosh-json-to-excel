#' edit_item.R
#'
#' @file edit_item.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
EditItemAndItemVisit <- function(field_items, sheet_name) {
    visit_group <- visit_info %>% filter(alias_name == sheet_name)
    if (visit_group %>% nrow() == 1) {
        item <- NULL
        item_visit <- EditItem(field_items, sheet_name)
    } else {
        item <- EditItem(field_items, sheet_name)
        item_visit <- NULL
    }
    return(list2(
        !!.const[["kItemNonVisit"]] := item,
        !!.const[["kItemVisit"]] := item_visit
    ))
}
EditItem <- function(field_items, alias_name) {
    aliasName <- alias_name
    sheet_seq <- sheet_info %>%
        filter(alias_name == aliasName) %>%
        purrr::pluck("sort_order", 1)
    target_field_items <- field_items %>% GetTargetByType(.const[["kArticle"]])
    target <- target_field_items %>% map_df(~ {
        presence_if_references <- PluckConst(.x, .const[["kValidatePresenceIf"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        formula_if_references <- PluckConst(.x, .const[["kValidateFormulaIf"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        references_after <- PluckConst(.x, .const[["kValidateDateAfterOrEqualTo"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        references_before <- PluckConst(.x, .const[["kValidateDateBeforeOrEqualTo"]]) %>%
            GetFieldText(alias_name, field_list, visit_info)
        numericality <- PluckConst(.x, .const[["kValidatorsNumericality"]])
        numericality_check <- !is.null(numericality)
        normal_range_gte <- PluckOrNA(.x, .const[["kNormalRangeGreaterThanOrEqualTo"]])
        normal_range_lss <- PluckOrNA(.x, .const[["kNormalRangeLessThanOrEqualTo"]])
        normal_range_check <- (!is.null(normal_range_gte) && !is.na(normal_range_gte)) ||
            (!is.null(normal_range_lss) && !is.na(normal_range_lss))
        numericality_normal_range_check <- dplyr::case_when(
            numericality_check & normal_range_check  ~ .const[["kCheckBoth"]],
            numericality_check & !normal_range_check ~ .const[["kCheckNumericality"]],
            !numericality_check & normal_range_check ~ .const[["kCheckNormalRange"]],
            TRUE                                     ~ .const[["kCheckNone"]]
        )

        # フィールドタイプ
        field_type <- dplyr::case_when(
            .x[["field_type"]] %in% c("text", "text_area") & numericality_check  ~ "数値",
            .x[["field_type"]] %in% c("text", "text_area") & !numericality_check ~ "テキスト",
            TRUE ~ NA_character_
        )
        res <- tibble::tibble(
            name = PluckOrNA(.x, .const[["kFieldItemsFieldId"]]),
            label = PluckOrNA(.x, .const[["kFieldItemsFieldName"]]),
            option.name = PluckOrNA(.x, "option_name"),
            default_value = PluckOrNA(.x, .const[["kFieldItemDefaultValue"]]),
            validators.presence.validate_presence_if = PluckOrNA(.x, .const[["kValidatePresenceIf"]]),
            presence_if_references = presence_if_references %||% NA,
            validators.formula.validate_formula_if = PluckOrNA(.x, .const[["kValidateFormulaIf"]]),
            formula_if_references = formula_if_references %||% NA,
            validators.formula.validate_formula_message = PluckOrNA(.x, .const[["kValidateFormulaMessage"]]),
            validators.date.validate_date_after_or_equal_to = PluckOrNA(.x, .const[["kValidateDateAfterOrEqualTo"]]),
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = PluckOrNA(.x, .const[["kValidateDateBeforeOrEqualTo"]]),
            references_before = references_before %||% NA,
            field_type = field_type,
            numericality_normal_range_check = numericality_normal_range_check,
            field_item.seq = PluckOrNA(.x, .const[["kFieldItemsSeq"]]),
        )
        res[["sheet.seq"]] <- sheet_seq
        return(res)
    })
    return(target)
}
