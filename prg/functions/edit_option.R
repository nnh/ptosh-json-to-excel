GetOptions <- function(field_items) {
    target <- field_items %>%
        keep(~ !is.null(.x$option) && .x$type == "FieldItem::Article")
    target_options <- target %>% keep(~ {
        options <- .x$option
        any(map_lgl(options$values, ~ isTRUE(.x$is_usable)))
    })
    options <- target_options %>%
        map(~ {
            option <- .x$option
            option_name <- option$name
            option_values <- option$values
            df_option_values <- option_values %>%
                map_df(~ tibble(
                    option.values_name = .x$name %||% NA,
                    option.values_seq = .x$seq %||% NA,
                    option.values_code = .x$code %||% NA,
                    option.values_is_usable = .x$is_usable %||% NA
                ))
            df_option_values$option.name <- option_name
            return(df_option_values)
        }) %>%
        bind_rows()
    return(options)
}
