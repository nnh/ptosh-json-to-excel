#' edit_action.R
#'
#' @file edit_action.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
GetAction <- function(field_items, alias_name, sheet) {
    target <- field_items %>%
        keep(~ !is.null(.x[["flip_flops"]]) && length(.x[["flip_flops"]]) > 0)
    if (length(target) == 0) {
        return(NULL)
    }
    target_field_list <- field_list %>%
        select(name, alias_name, label) %>%
        filter(alias_name == !!alias_name)
    field_items_name_label <- field_items %>%
        map_df(~ tibble(name = .x[["name"]], fields.label = .x[["label"]]))
    action <- target %>% map_df(~ {
        field_items <- .x
        flip_flops <- field_items[["flip_flops"]] %>% map_df(~ {
            codes <- tibble(codes = as.character(.x[["codes"]]))
            fields <- tibble(fields = as.character(.x[["fields"]]))
            res <- codes %>%
                tidyr::crossing(fields, .)
            return(res)
        })
        flip_flops[["field_item_id.name"]] <- field_items[["name"]]
        flip_flops[["field_item_id.label"]] <- field_items[["label"]]
        return(flip_flops)
    })
    action <- action %>%
        left_join(field_items_name_label, by = c("fields" = "name"))
    res <- JoinJpnameAndAliasNameAndSelectColumns("action", sheet)
    return(res)
}
