#' test script
#'
#' @file excel_json_validator_presence.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckPresence <- function(sheetList) {
    sheetName <- "presence"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetPresenceFromJson()
    return(CheckTarget(sheet, json))
}
GetPresenceFromJson <- function() {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .$type == "FieldItem::Article" & is.null(.$validators$presence))
        presence <- res |> map_df(~ list(name = .$name, label = .$label))
        presence$alias_name <- aliasName
        return(presence)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label"))
    return(res)
}
