#' test script
#'
#' @file excel_json_validator_options.R
#' @author Mariko Ohtsuka
#' @date 2025.5.15
CheckOption <- function(sheetList) {
    sheetName <- "option"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetOptionFromJson()
    return(CheckTarget(sheet, json))
}
GetOptionFromJson <- function() {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |>
            map(~ {
                item <- .
                if (item$type != "FieldItem::Article") {
                    return(NULL)
                }
                option <- item$option
                if (is.null(option)) {
                    return(NULL)
                }
                option$values <- option$values |> keep(~ .$is_usable)
                optionName <- option$name
                optionValues <- option$values |> map(~ list(option.name = optionName, option.values_name = .$name, option.values_seq = .$seq, option.values_code = .$code, option.values_is_usable = .$is_usable))
                return(optionValues)
            }) |>
            keep(~ !is.null(.)) |>
            map_df(~.)
        res$alias_name <- aliasName
        return(res)
    }) |>
        bind_rows() |>
        distinct()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "option.name", "option.values_name", "option.values_seq", "option.values_code", "option.values_is_usable"))
    res <- res |> mutate(option.values_seq = as.numeric(option.values_seq))
    return(res)
}
