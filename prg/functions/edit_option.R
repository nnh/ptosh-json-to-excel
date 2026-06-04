#' edit_option.R
#'
#' @file edit_option.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
GetOptionsValues <- function(option) {
    option_name <- option[["name"]]
    option_values <- option[["values"]]
    df_option_values <- option_values %>%
        map_df(~ tibble(
            option.values_name = .x[["name"]] %||% NA,
            option.values_seq = .x[[.const[["kOptionSeq"]]]] %||% NA,
            option.values_code = .x[["code"]] %||% NA,
            option.values_is_usable = .x[["is_usable"]] %||% NA
        ))
    df_option_values[["option.name"]] <- option_name
    return(df_option_values)
}

GetOptions <- function(field_items, sheet, options_flag, options_json) {
    if (options_flag) {
        target <- field_items %>%
            keep(~ !is.null(.x[["option_name"]]) && .x[[.const[["kFieldItemsType"]]]] == .const[["kArticle"]])
    } else {
        target <- field_items %>%
            keep(~ !is.null(.x[["option"]]) && .x[[.const[["kFieldItemsType"]]]] == .const[["kArticle"]])
    }

    if (length(target) == 0) {
        return(NULL)
    }
    option <- target %>%
        map(~ {
            if (options_flag) {
                option_name <- .x[["option_name"]]
                option <- options_json[[option_name]]
            } else {
                option <- .x[["option"]]
            }
            if (length(option) == 0) {
                stop(paste("Option ID", option_name, "not found in options JSON."))
            }
            field_item_seq <- .x[[.const[["kFieldItemsSeq"]]]]
            df_option_values <- option %>% GetOptionsValues(.)
            df_option_values[["field_item.seq"]] <- field_item_seq
            return(df_option_values)
        }) %>%
        bind_rows() %>%
        distinct() %>%
        filter(option.values_is_usable == TRUE)
    option[["sheet.seq"]] <- sheet[["sort_order"]]
    res <- JoinJpnameAndAliasNameAndSelectColumns(option, .const[["kOption"]], sheet)

    return(res)
}
