#' test script
#'
#' @file excel_json_validator_assign.R
#' @author Mariko Ohtsuka
#' @date 2026.1.7
CheckAssigned <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetAssignedFromJson(fieldItems)
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
    df3 <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label", "default_value"), jpNameAndGroup)
    res <- df3 |>
        left_join(visitGroupSheetAndFieldOrders, by = c("alias_name" = "alias_name", "name" = "field_id")) |>
        arrange(seq, field_seq) |>
        select(-seq, -field_seq, -field_label)
    return(res)
}
