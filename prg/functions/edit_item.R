EditItem <- function(field_items, alias_name) {
    target_field_items <- field_items %>% GetTargetByType("FieldItem::Article")
    target <- target_field_items %>% map_df(~ {
        presence_if_references <- GetFieldText(.x$validators$presence$validate_presence_if, alias_name)
        formula_if_references <- GetFieldText(.x$validators$formula$validate_formula_if, alias_name)
        references_after <- GetFieldText(.x$validators$date$validate_date_after_or_equal_to, alias_name)
        references_before <- GetFieldText(.x$validators$date$validate_date_before_or_equal_to, alias_name)
        numericality_gte <- purrr::pluck(.x, "validators", "numericality", "validate_numericality_greater_than_or_equal_to", .default = NA)
        numericality_lss <- purrr::pluck(.x, "validators", "numericality", "validate_numericality_less_than_or_equal_to", .default = NA)
        numericality_check <- (!is.null(numericality_gte) && !is.na(numericality_gte)) ||
            (!is.null(numericality_lss) && !is.na(numericality_lss))
        normal_range_gte <- purrr::pluck(.x, "normal_range", "greater_than_or_equal_to", .default = NA)
        normal_range_lss <- purrr::pluck(.x, "normal_range", "less_than_or_equal_to", .default = NA)
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
        if (.x$field_type %in% c("text", "text_area")) {
            if (numericality_check) {
                field_type <- "数値"
            } else {
                field_type <- "テキスト"
            }
        } else {
            field_type <- NA
        }
        res <- tibble::tibble(
            name = .x$name,
            label = .x$label,
            option.name = .x$option$name %||% NA,
            default_value = .x$default_value %||% NA,
            validators.presence.validate_presence_if = .x$validators$presence$validate_presence_if %||% NA,
            presence_if_references = presence_if_references %||% NA,
            validators.formula.validate_formula_if = .x$validators$formula$validate_formula_if %||% NA,
            formula_if_references = formula_if_references %||% NA,
            validators.formula.validate_formula_message = .x$validators$formula$validate_formula_message %||% NA,
            validators.date.validate_date_after_or_equal_to = .x$validators$date$validate_date_after_or_equal_to %||% NA,
            references_after = references_after %||% NA,
            validators.date.validate_date_before_or_equal_to = .x$validators$date$validate_date_before_or_equal_to %||% NA,
            references_before = references_before %||% NA,
            field_type = field_type,
            numericality_normal_range_check = numericality_normal_range_check
        )
        return(res)
    })
    return(target)
}
