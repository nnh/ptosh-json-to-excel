#' test script
#'
#' @file excel_json_validator_action.R
#' @author Mariko Ohtsuka
#' @date 2025.6.27
CheckAction <- function(sheetList, fieldItems, jpNameAndAliasName) {
    sheetName <- "action"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    sheet$id <- sheet$id |> as.integer()
    sheet$field_item_id <- sheet$field_item_id |> as.integer()
    json <- GetActionFromJson(fieldItems, jpNameAndAliasName)
    json$id <- json$id |> as.integer()
    json$field_item_id <- json$field_item_id |> as.integer()
    return(CheckTarget(sheet, json))
}
GetActionFromJson <- function(fieldItems, jpNameAndAliasName) {
    action <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        flip_flops <- fieldItem |>
            map(~ list(aliasName = aliasName, name = .$name, label = .$label, flip_flops = .$flip_flops)) |>
            keep(~ length(.$flip_flops) > 0)
        if (length(flip_flops) == 0) {
            return(NULL)
        }
        res <- flip_flops |> map(~ {
            flip_flop <- .$flip_flops
            field_item_id.name <- .$name
            field_item_id.label <- .$label
            alias_name <- .$aliasName
            res <- flip_flop |> map_df(~ {
                temp <- crossing(codes = list_c(.$code), fields = list_c(.$fields)) |> tibble()
                temp$alias_name <- alias_name
                temp$id <- .$id
                temp$field_item_id <- .$field_item_id
                temp$field_item_id.name <- field_item_id.name
                temp$field_item_id.label <- field_item_id.label
                return(temp)
            })
            return(res)
        })
        return(res)
    }) |>
        keep(~ !is.null(.)) |>
        bind_rows()
    aliasnameAndFieldIdAndLabel <- GetAliasnameAndFieldIdAndLabel(fieldItems)
    if (nrow(action) > 0) {
        df <- action |> inner_join(aliasnameAndFieldIdAndLabel, by = c("alias_name", "fields"))
    } else {
        df <- action
    }
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "id", "field_item_id", "field_item_id.name", "field_item_id.label", "codes", "fields", "fields.label"), jpNameAndAliasName)
    return(res)
}
