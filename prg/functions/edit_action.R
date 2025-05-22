GetAction <- function(field_items, alias_name) {
    target <- field_items %>%
        keep(~ !is.null(.x$flip_flops) && length(.x$flip_flops) > 0)
    if (length(target) == 0) {
        return(NULL)
    }
    target_field_list <- field_list %>%
        select(name, alias_name, label) %>%
        filter(alias_name == !!alias_name)
    field_items_name_label <- field_items %>%
        map_df(~ tibble(name = .x$name, fields.label = .x$label))
    action <- target %>% map_df(~ {
        field_items <- .x
        flip_flops <- field_items$flip_flops %>% map_df(~ {
            id <- .x$id
            field_item_id <- .x$field_item_id
            codes <- tibble(codes = as.character(.x$codes))
            fields <- tibble(fields = as.character(.x$fields))
            res <- codes %>%
                tidyr::crossing(fields, .)
            res$id <- id
            res$field_item_id <- field_item_id
            return(res)
        })
        flip_flops$field_item_id.name <- field_items$name
        flip_flops$field_item_id.label <- field_items$label
        return(flip_flops)
    })
    res <- action %>%
        left_join(field_items_name_label, by = c("fields" = "name"))
    return(res)
}
