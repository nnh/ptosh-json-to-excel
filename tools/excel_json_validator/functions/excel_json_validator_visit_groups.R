#' test script
#'
#' @file excel_json_validator_visit_groups.R
#' @author Mariko Ohtsuka
#' @date 2026.1.7
#'
#'
CheckVisitGroupValidator <- function(target_json, isVisit) {
    if (!isVisit) {
        return(TRUE)
    }
    df_isVisit_json <- isVisit_json %>%
        map(~ {
            name <- .x[["name"]] %>%
                str_remove("\\([^()]*\\)$")
            alias_name <- .x$alias_name
            visitnum <- alias_name %>%
                str_extract(("[0-9]+$")) %>%
                as.numeric()
            visit <- .x[["name"]] %>%
                str_extract("\\([^()]*\\)$") %>%
                str_remove_all("\\(|\\)")
            return(tibble(name = name, alias_name = alias_name, visit_num = visitnum, visit_name = visit))
        }) %>%
        bind_rows()
    df_isVisit_sort <- df_isVisit_json %>%
        left_join(sheetOrders, by = c("alias_name" = "sheet")) %>%
        arrange(visit_num, seq) %>%
        select(-seq)
    # jsonのvisit_groupsと照合
    forTest <- visitGroups %>% select(-group)
    if (!identical(df_isVisit_sort, forTest)) {
        stop("Error: visit_groups in json does not match the visit sheets.")
    }
    return(TRUE)
}
