#' edit_allocation.R
#'
#' @file edit_allocation.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
GetAllocation <- function(json_file) {
    allocation <- json_file[[.const[["kAllocation"]]]]
    if (is.null(allocation)) {
        return(NULL)
    }
    is_zelen <- allocation[["is_zelen"]]
    zelen_imbalance <- allocation[["zelen_imbalance"]]
    is_double_blinded <- allocation[["is_double_blinded"]]
    double_blind_emails <- allocation[["double_blind_emails"]]
    allocation_method <- allocation[["allocation_method"]]
    formula_field <- json_file[["field_items"]] %>%
        map(~ .x[["formula_field"]]) %>%
        keep(~ !is.null(.x))
    formula_field_str <- if (length(formula_field) > 0) {
        paste(unlist(formula_field), collapse = ", ")
    } else {
        ""
    }
    groups <- allocation[[.const[["kAllocationGroups"]]]] %>% map_df(~ {
        if_references <- GetFieldText(.x[["if"]], json_file[[.const[["kAliasName"]]]], field_list, visit_info)
        group_tbl <- tibble::tibble(
            groups.code = .x[[.const[["kAllocationGroupsCode"]]]],
            groups.label = .x[[.const[["kAllocationGroupsLabel"]]]],
            groups.if = .x[["if"]],
            groups.if_references = if (is.null(if_references)) NA else if_references,
            groups.message = .x[["message"]]
        )
        return(group_tbl)
    })
    res <- tibble::tibble(
        is_zelen = is_zelen,
        zelen_imbalance = zelen_imbalance,
        is_double_blinded = is_double_blinded,
        double_blind_emails = double_blind_emails,
        allocation_method = allocation_method
    ) %>%
        bind_cols(groups)
    res[["formula_field"]] <- formula_field_str
    temp_formula_field_references <- GetDfSheetField(formula_field_str, json_file[[.const[["kAliasName"]]]])
    if (!is.null(temp_formula_field_references)) {
        formula_field_references <- temp_formula_field_references %>% EditRefFieldTextVec(field_list, visit_info)
        # Replace raw in formula_field_str with text for all rows if raw is present
        if (!is.null(formula_field_references) && nrow(formula_field_references) > 0) {
            for (i in 1:nrow(formula_field_references)) {
                raw_val  <- formula_field_references[["raw"]][i]
                text_val <- formula_field_references[["text"]][i]
                if (!is.na(raw_val) && !is.na(text_val)) {
                    if (grepl(raw_val, formula_field_str, fixed = TRUE)) {
                        formula_field_str <- gsub(raw_val, text_val, formula_field_str, fixed = TRUE)
                    }
                }
            }
        }
    }
    res[["formula_field_references"]] <- formula_field_str
    res <- JoinJpnameAndAliasNameAndSelectColumns(res, .const[["kAllocation"]], json_file)
    return(res)
}
