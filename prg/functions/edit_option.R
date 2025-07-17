GetOptionsValues <- function(option) {
    option_name <- option[["name"]]
    option_values <- option[["values"]]
    df_option_values <- option_values %>%
        map_df(~ tibble(
            option.values_name = .x[["name"]] %||% NA,
            option.values_seq = .x[["seq"]] %||% NA,
            option.values_code = .x[["code"]] %||% NA,
            option.values_is_usable = .x[["is_usable"]] %||% NA
        ))
    df_option_values[["option.name"]] <- option_name
    return(df_option_values)
}

GetOptions <- function(field_items) {
    if (options_flag) {
        target <- field_items %>%
            keep(~ !is.null(.x[["option_id"]]) && .x[["type"]] == "FieldItem::Article")
    } else {
        target <- field_items %>%
            keep(~ !is.null(.x[["option"]]) && .x[["type"]] == "FieldItem::Article")
    }
    if (length(target) == 0) {
        return(NULL)
    }
    options <- target %>%
        map(~ {
            if (options_flag) {
                option_id <- .x[["option_id"]]
                option <- options_json %>%
                    keep(~ .x[["id"]] == option_id)
            } else {
                option <- .x[["option"]]
            }
            if (length(option) == 0) {
                stop(paste("Option ID", option_id, "not found in options JSON."))
            }
            if (options_flag) {
                df_option_values <- option %>% map_df(~ GetOptionsValues(.))
            } else {
                df_option_values <- option %>% GetOptionsValues(.)
            }
            return(df_option_values)
        }) %>%
        bind_rows() %>%
        distinct() %>%
        filter(option.values_is_usable == TRUE)
    return(options)
}
