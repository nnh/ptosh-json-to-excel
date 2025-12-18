#' test script
#'
#' @file excel_json_validator_title.R
#' @author Mariko Ohtsuka
#' @date 2025.12.15
CheckTitle <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetTitleFromJson(fieldItems)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    return(CheckTarget(sheet, json))
}
GetTitleFromJson <- function(fieldItems) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .[["type"]] == "FieldItem::Heading")
        title <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], level = .[["level"]]))
        title[["alias_name"]] <- aliasName
        return(title)
    }) |> bind_rows()
    df2 <- JoinVisitGroupsValidator(df, key = "alias_name", target = "group")
    res <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label", "level"), jpNameAndGroup)
    res <- res |> mutate(level = as.numeric(level))
    return(res)
}
JoinVisitGroupsValidator <- function(df, key = "alias_name", target = "group") {
    joinVisitGroups <- left_join(df, visitGroups[, c(key, target)], by = key)
    for (row in 1:nrow(joinVisitGroups)) {
        if (!is.na(joinVisitGroups[row, target])) {
            joinVisitGroups[row, key] <- joinVisitGroups[row, target]
        }
    }
    distinctJoinVisitGroups <- joinVisitGroups %>% distinct()
    res <- distinctJoinVisitGroups %>% select(-all_of(target))
    return(res)
}
