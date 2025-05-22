GetPresence <- function(field_items, json_file) {
    cdisc_sheet_configs <- json_file$cdisc_sheet_configs
    exclude_fields <- cdisc_sheet_configs %>%
        map(~ {
            table <- .x$table
            stat <- table %>%
                keep(~ identical(.x, "STAT"))
            if (.x$prefix == "IE") {
                ieorres <- table %>%
                    keep(~ identical(.x, "ORRES"))
            } else {
                ieorres <- NULL
            }
            res <- c(stat, ieorres)
            if (length(res) == 0) {
                return(NULL)
            }
            return(res)
        }) %>%
        keep(~ !is.null(.x))
    exclude_field_names <- exclude_fields %>%
        map_chr(~ names(.x)[1])
    target <- field_items %>%
        GetTargetByType("FieldItem::Article") %>%
        keep(~ is.null(.x$validators$presence))
    if (length(target) == 0) {
        return(NULL)
    }
    res <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x$name,
                label = .x$label,
            )
            return(res)
        })
    res <- res %>% dplyr::filter(!name %in% exclude_field_names)
    return(res)
}
