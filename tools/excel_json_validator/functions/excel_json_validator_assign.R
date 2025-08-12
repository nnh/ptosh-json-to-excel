#' test script
#'
#' @file excel_json_validator_assign.R
#' @author Mariko Ohtsuka
#' @date 2025.8.12
CheckAssigned <- function(sheetList, fieldItems, jpNameAndAliasName, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetAssignedFromJson(fieldItems, jpNameAndAliasName)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    return(CheckTarget(sheet, json))
}
GetAssignedFromJson <- function(fieldItems, jpNameAndAliasName) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .[["type"]] == "FieldItem::Assigned")
        assigned <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], default_value = .[["default_value"]]))
        assigned[["alias_name"]] <- aliasName
        return(assigned)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "default_value"), jpNameAndAliasName)
    return(res)
}
