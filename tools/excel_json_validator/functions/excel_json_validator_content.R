#' test script
#'
#' @file excel_json_validator_content.R
#' @author Mariko Ohtsuka
#' @date 2025.8.12
CheckContent <- function(sheetList, fieldItems, jpNameAndAliasName, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetContentFromJson(fieldItems, jpNameAndAliasName)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    sheet$content <- sheet$content %>% CleanTextForComment()
    json$content <- json$content %>% CleanTextForComment()
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
