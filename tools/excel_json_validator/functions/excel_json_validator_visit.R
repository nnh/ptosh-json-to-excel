#' test script
#'
#' @file excel_json_validator_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.12.12
CheckJsonVisitForVisit <- function(visitJson) {
    res <- visitJson %>%
        map_df(~ {
            visit <- .x
            res <- data.frame(
                jpname = visit[["name"]],
                alias_name = visit[["alias_name"]],
                name = visit[["alias_name"]] %>% str_split("_") %>% map_chr(~ tail(.x, 1)) %>% as.numeric(),
                default_value = visit[["name"]] %>% str_remove("\\)$") %>% str_split("\\(") %>% map_chr(~ tail(.x, 1))
            )
            return(res)
        }) %>%
        arrange(name)
    res$name <- as.character(res$name)
    return(res)
}
CheckVisit <- function(sheetList, sheetName, json) {
    visitJson <- json[["sheets"]] %>% keep(~ .[["category"]] == "visit")
    if (length(visitJson) == 0) {
        #         visit_fieldItems <- jsonList |> GetFieldItemsByJsonList()
        #         json <- GetVisitFromJson(visit_fieldItems, jpNameAndAliasName)
        #         sheet <- sheetList[[sheetName]] |>
        #             rename(!!!engToJpnColumnMappings[[sheetName]])
        #         sheet$default_value <- sheet$default_value %>% as.character()
    } else {
        json <- CheckJsonVisitForVisit(visitJson)
        sheet <- sheetList[[sheetName]] |>
            rename(!!!engToJpnColumnMappings[["visit_to_visit"]])
        sheet$name <- sheet$name %>% as.character()
    }
    return(CheckTarget(sheet, json))
}
# GetVisitFromJson <- function(fieldItems, jpNameAndAliasName) {
#     df <- map2(fieldItems, names(fieldItems), ~ {
#         fieldItem <- .x
#         aliasName <- .y
#         res <- fieldItem |> keep(~ .[["label"]] == "Visit Number")
#         visit <- res |> map_df(~ list(name = .[["name"]], default_value = .[["default_value"]]))
#         visit[["alias_name"]] <- aliasName
#         return(visit)
#     }) |> bind_rows()
#     res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "default_value"), jpNameAndAliasName)
#     return(res)
# }
