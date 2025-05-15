#' test script
#'
#' @file excel_json_validator_master.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckMaster <- function(sheetList) {
    sheetName <- "master"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetMasterFromJson()
    return(CheckTarget(sheet, json))
}
GetMasterFromJson <- function() {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ !is.null(.$link_type))
        master <- res |> map_df(~ list(name = .$name, label = .$label, link_type = .$link_type))
        master$alias_name <- aliasName
        return(master)
    }) |>
        bind_rows() |>
        filter(link_type != "")
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "link_type"))
    return(res)
}
