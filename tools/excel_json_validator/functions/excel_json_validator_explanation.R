#' test script
#'
#' @file excel_json_validator_explanation.R
#' @author Mariko Ohtsuka
#' @date 2025.8.12
CheckExplanation <- function(sheetList, fieldItems, jpNameAndAliasName, sheetName) {
  sheet <- sheetList[[sheetName]] |>
    rename(!!!engToJpnColumnMappings[[sheetName]])
  json <- GetExplanationFromJson(fieldItems, jpNameAndAliasName)
  sheet <- sheet %>% arrange(alias_name, name)
  json <- json %>% arrange(alias_name, name)
  sheet$description <- sheet$description %>% CleanTextForComment()
  json$description <- json$description %>% CleanTextForComment()
  return(CheckTarget(sheet, json))
}
GetExplanationFromJson <- function(fieldItems, jpNameAndAliasName) {
  df <- map2(fieldItems, names(fieldItems), ~ {
    fieldItem <- .x
    aliasName <- .y
    res <- fieldItem |> keep(~ !is.null(.[["description"]]))
    explanation <- res |> map_df(~ list(name = .[["name"]], label = .[["label"]], description = .[["description"]]))
    explanation[["alias_name"]] <- aliasName
    return(explanation)
  }) |>
    bind_rows() |>
    filter(description != "")
  res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label", "description"), jpNameAndAliasName)
  return(res)
}
