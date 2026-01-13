#' test script
#'
#' @file excel_json_validator_master.R
#' @author Mariko Ohtsuka
#' @date 2026.1.13
CheckMaster <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetMasterFromJson(fieldItems)
    return(CheckTarget(sheet, json))
}
GetMasterFromJson <- function(fieldItems) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ !is.null(.[["link_type"]]))
        master <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], link_type = .[["link_type"]]))
        master[["alias_name"]] <- aliasName
        return(master)
    }) |>
        bind_rows() |>
        filter(link_type != "")
    df2 <- df %>% inner_join(visitGroupSheetAndFieldOrders, by = c("alias_name" = "alias_name", "name" = "field_id"))
    df3 <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label", "link_type", "seq", "field_seq"), jpNameAndAliasName)
    df4 <- df3 %>%
        arrange(seq, field_seq) %>%
        select(-seq, -field_seq)
    res <- df4
    return(res)
}
