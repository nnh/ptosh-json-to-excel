#' 同一グループのVISIT情報を集約する処理
#'
#' @file summarize_by_visit.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
#
SummarizeByVisit <- function(sheet_data_combine, visit_info) {
    res <- sheet_data_combine
    summarize_target_sheet_names <- c(.const[["kOption"]], .const[["kAssigned"]], .const[["kLimitation"]], .const[["kDate"]])
    visit_group_map <- visit_info %>%
        dplyr::select(
            alias_name,
            visit_group_name,
            visit_group
        ) %>%
        dplyr::distinct()
    for (sheet_name in summarize_target_sheet_names) {
        if (nrow(res[[sheet_name]]) == 0) {
            next
        }
        res[[sheet_name]] <- res[[sheet_name]] %>%
            dplyr::left_join(visit_group_map, by = .const[["kAliasName"]]) %>%
            dplyr::mutate(
                !!.const[["kOutputJapanaseNameEnglish"]] := dplyr::coalesce(visit_group_name, !!sym(.const[["kOutputJapanaseNameEnglish"]])),
                !!.const[["kAliasName"]] := dplyr::coalesce(visit_group, !!sym(.const[["kAliasName"]]))
            ) %>%
            dplyr::select(-visit_group_name, -visit_group) %>%
            dplyr::distinct()
    }
    return(res)
}
