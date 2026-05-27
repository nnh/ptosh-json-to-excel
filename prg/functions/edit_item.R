#' edit_item.R
#'
#' @file edit_item.R
#' @author Mariko Ohtsuka
#' @date 2026.5.27
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
            GetFieldText(alias_name)
        formula_if_references <- PluckConst(.x, .const[["kValidateFormulaIf"]]) %>%
            GetFieldText(alias_name)
        references_after <- PluckConst(.x, .const[["kValidateDateAfterOrEqualTo"]]) %>%
            GetFieldText(alias_name)
        references_before <- PluckConst(.x, .const[["kValidateDateBeforeOrEqualTo"]]) %>%
            GetFieldText(alias_name)
        numericality <- PluckConst(.x, .const[["kValidatorsNumericality"]])
        numericality_check <- !is.null(numericality)
        normal_range_gte <- PluckConst(.x, .const[["kNormalRangeGreaterThanOrEqualTo"]], NA)
        normal_range_lss <- PluckConst(.x, .const[["kNormalRangeLessThanOrEqualTo"]], NA)
        normal_range_check <- (!is.null(normal_range_gte) && !is.na(normal_range_gte)) ||
            (!is.null(normal_range_lss) && !is.na(normal_range_lss))
        if (numericality_check) {
            if (normal_range_check) {
                numericality_normal_range_check <- "数値・アラート有"
            } else {
                numericality_normal_range_check <- "数値チェック有"
            }
        } else {
            if (normal_range_check) {
                numericality_normal_range_check <- "アラート設定有"
            } else {
                numericality_normal_range_check <- "条件なし"
            }
        }

        # フィールドタイプ
        if (.x[["field_type"]] %in% c("text", "text_area")) {
            if (numericality_check) {
                field_type <- "数値"
            } else {
                field_type <- "テキスト"
            }
        } else {
            field_type <- NA
        }
        res <- tibble::tibble(
            name = .x[[.const[["kFieldItemsFieldId"]]]] %||% NA,
            label = .x[[.const[["kFieldItemsFieldName"]]]] %||% NA,
            option.name = .x[["option_name"]] %||% NA,
            default_value = .x[[.const[["kFieldItemDefaultValue"]]]] %||% NA,
            validators.presence.validate_presence_if = PluckConst(.x, .const[["kValidatePresenceIf"]], NA),
            presence_if_references = presence_if_references %||% NA,
            validators.formula.validate_formula_if = PluckConst(.x, .const[["kValidateFormulaIf"]], NA),
            formula_if_references = formula_if_references %||% NA,
            validators.formula.validate_formula_message = purrr::pluck(.x, "validators", "formula", "validate_formula_message", .default = NA),
            validators.date.validate_date_after_or_equal_to = PluckConst(.x, .const[["kValidateDateAfterOrEqualTo"]], NA),
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = PluckConst(.x, .const[["kValidateDateBeforeOrEqualTo"]], NA),
            references_before = references_before %||% NA,
            field_type = field_type,
            numericality_normal_range_check = numericality_normal_range_check,
            field_item.seq = .x[[.const[["kFieldItemsSeq"]]]] %||% NA,
        )
        res[["sheet.seq"]] <- sheet_seq
        return(res)
    })
    return(target)
}
