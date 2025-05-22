#' test script
#'
#' @file excel_json_validator_explanation.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckExplanation <- function(sheetList) {
  sheetName <- "explanation"
  sheet <- sheetList[[sheetName]] |>
    rename(!!!engToJpnColumnMappings[[sheetName]])
  json <- GetExplanationFromJson()
  return(CheckTarget(sheet, json))
}
GetExplanationFromJson <- function() {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x
    aliasName <- .y
    res <- fieldItem |> keep(~ !is.null(.$description))
    explanation <- res |> map_df(~ list(name = .$name, label = .$label, description = .$description))
    explanation$alias_name <- aliasName
    return(explanation)
  }) |>
    bind_rows() |>
    filter(description != "")
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "description"))
  return(res)
}
