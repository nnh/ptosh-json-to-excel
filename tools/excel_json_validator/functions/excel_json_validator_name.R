#' test script
#'
#' @file excel_json_validator_name.R
#' @author Mariko Ohtsuka
#' @date 2025.12.15
CheckName <- function(sheetList, target_json, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- target_json[["sheets"]] |>
        map_df(~ list(name = .[["name"]], alias_name = .[["alias_name"]], images_count = .[["images_count"]])) |>
        as.data.frame()
    json <- json |> mutate(images_count = as.numeric(images_count))
    return(CheckTarget(sheet, json))
}
