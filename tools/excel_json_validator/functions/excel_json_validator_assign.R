#' test script
#'
#' @file excel_json_validator_assign.R
#' @author Mariko Ohtsuka
#' @date 2025.12.15
CheckAssigned <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetAssignedFromJson(fieldItems)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    return(CheckTarget(sheet, json))
}
GetAssignedFromJson <- function(fieldItems) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .[["type"]] == "FieldItem::Assigned")
        assigned <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], default_value = .[["default_value"]]))
        assigned[["alias_name"]] <- aliasName
        return(assigned)
    }) |> bind_rows()
    df2 <- JoinVisitGroupsValidator(df, key = "alias_name", target = "group") %>% distinct()
    res <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label", "default_value"), jpNameAndGroup)
    return(res)
}
