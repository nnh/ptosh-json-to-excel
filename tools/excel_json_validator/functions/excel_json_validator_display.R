#' test script
#'
#' @file excel_json_validator_display.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckDisplay <- function(sheetList) {
    sheetName <- "display"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetDisplayFromJson()
    return(CheckTarget(sheet, json))
}
GetDisplayFromJson <- function() {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |>
            map(~ {
                temp <- .
                type <- temp$type
                is_invisible <- temp$is_invisible
                if (type == "FieldItem::Assigned" & !is_invisible) {
                    res <- tibble(alias_name = aliasName, name = temp$name, label = temp$label)
                    return(res)
                }
                if (type == "FieldItem::Article" & is_invisible) {
                    res <- tibble(alias_name = aliasName, name = temp$name, label = temp$label)
                    return(res)
                }
                return(NULL)
            }) |>
            keep(~ !is.null(.))
        return(res)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "label"))
    return(res)
}
