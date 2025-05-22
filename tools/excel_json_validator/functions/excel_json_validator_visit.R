#' test script
#'
#' @file excel_json_validator_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckVisit <- function(sheetList) {
    sheetName <- "visit"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetVisitFromJson()
    return(CheckTarget(sheet, json))
}
GetVisitFromJson <- function() {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .$label == "Visit Number")
        visit <- res |> map_df(~ list(name = .$name, default_value = .$default_value))
        visit$alias_name <- aliasName
        return(visit)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "default_value"))
    return(res)
}
