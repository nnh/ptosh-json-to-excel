GetOptions <- function(json_file) {
    # option.values_is_usableがTRUEのもの
    # かつ
    # ""type"": ""FieldItem::Article"
    target <- json_file$field_items %>%
        keep(~ !is.null(.x$option) && .x$type == "FieldItem::Article")
    target_options <- target %>% keep(~ {
        options <- .x$option
        any(map_lgl(options$values, ~ isTRUE(.x$is_usable)))
    })
    options <- target_options %>% map(~ {
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
        df_option_values$jpname <- json_file$name
        df_option_values$alias_name <- json_file$alias_name
        res <- df_option_values %>%
            select(kEngColumnNames$option) %>%
            mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
        return(res)
    })
    return(options)
}
