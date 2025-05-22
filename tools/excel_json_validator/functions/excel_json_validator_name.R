#' test script
#'
#' @file excel_json_validator_name.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckName <- function(sheetList, jsonList) {
    sheetName <- "name"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- jsonList |>
        map_df(~ list(name = .$name, alias_name = .$alias_name, images_count = .$images_count)) |>
        as.data.frame()
    json <- json |> mutate(images_count = as.numeric(images_count))
    return(CheckTarget(sheet, json))
}
