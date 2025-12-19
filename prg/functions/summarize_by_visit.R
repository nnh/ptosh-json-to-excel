#' 同一グループのVISIT情報を集約する処理
#'
#' @file summarize_by_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.12.19
#
SummarizeByVisit <- function(sheet_data_combine) {
    res <- sheet_data_combine
    summarize_target_sheet_names <- c("option", "assigned", "limitation", "date")
    for (sheet_name in summarize_target_sheet_names) {
        if (nrow(res[[sheet_name]]) == 0) {
            next
        }
        for (row in 1:nrow(res[[sheet_name]])) {
            aliasName <- res[[sheet_name]] %>%
                dplyr::slice(row) %>%
                dplyr::pull(alias_name)

            visit_group <- visit_info %>%
                dplyr::filter(alias_name == aliasName)
            if (nrow(visit_group) == 1) {
                res[[sheet_name]][row, "jpname"] <- visit_group[["visit_group_name"]]
                res[[sheet_name]][row, "alias_name"] <- visit_group[["visit_group"]]
            }
        }
        res[[sheet_name]] <- res[[sheet_name]] %>% distinct()
    }
    return(res)
}
