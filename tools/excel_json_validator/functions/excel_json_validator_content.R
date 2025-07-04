#' test script
#'
#' @file excel_json_validator_content.R
#' @author Mariko Ohtsuka
#' @date 2025.7.4
CheckContent <- function(sheetList, fieldItems, jpNameAndAliasName) {
    sheetName <- "comment"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetContentFromJson(fieldItems, jpNameAndAliasName)
    return(CheckTarget(sheet, json))
}
GetContentFromJson <- function(fieldItems, jpNameAndAliasName) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ !is.null(.[["content"]]))
        if (length(res) == 0) {
            return(NULL)
        }
        content <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], content = .[["content"]]))
        content[["alias_name"]] <- aliasName
        return(content)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "content"), jpNameAndAliasName)
    return(res)
}
