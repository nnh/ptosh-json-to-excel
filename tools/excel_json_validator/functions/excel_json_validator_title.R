#' test script
#'
#' @file excel_json_validator_title.R
#' @author Mariko Ohtsuka
#' @date 2025.7.4
CheckTitle <- function(sheetList, fieldItems, jpNameAndAliasName) {
    sheetName <- "title"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetTitleFromJson(fieldItems, jpNameAndAliasName)
    return(CheckTarget(sheet, json))
}
GetTitleFromJson <- function(fieldItems, jpNameAndAliasName) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .[["type"]] == "FieldItem::Heading")
        title <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], level = .[["level"]]))
        title[["alias_name"]] <- aliasName
        return(title)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "level"), jpNameAndAliasName)
    res <- res |> mutate(level = as.numeric(level))
    return(res)
}
