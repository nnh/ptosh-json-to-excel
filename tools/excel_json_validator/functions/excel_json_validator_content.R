#' test script
#'
#' @file excel_json_validator_content.R
#' @author Mariko Ohtsuka
#' @date 2025.12.15
CheckContent <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetContentFromJson(fieldItems)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    sheet$content <- sheet$content %>% CleanTextForComment()
    json$content <- json$content %>% CleanTextForComment()
    return(CheckTarget(sheet, json))
}
GetContentFromJson <- function(fieldItems) {
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
    df2 <- JoinVisitGroupsValidator(df, key = "alias_name", target = "group") %>% distinct()
    res <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label", "content"), jpNameAndGroup)
    return(res)
}
