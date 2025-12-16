#' test script
#'
#' @file excel_json_validator_action.R
#' @author Mariko Ohtsuka
#' @date 2025.12.15
CheckAction <- function(sheetList, fieldItems, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetActionFromJson(fieldItems)
    sheet <- sheet %>% arrange(alias_name, fields)
    json <- json %>% arrange(alias_name, fields)
    return(CheckTarget(sheet, json))
}
GetActionFromJson <- function(fieldItems) {
    action <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        flip_flops <- fieldItem |>
            map(~ list(
                aliasName = aliasName,
                name = .[["name"]],
                label = .[["label"]],
                flip_flops = .[["flip_flops"]]
            )) |>
            keep(~ length(.[["flip_flops"]]) > 0)
        if (length(flip_flops) == 0) {
            return(NULL)
        }
        res <- flip_flops |> map(~ {
            flip_flop <- .[["flip_flops"]]
            field_item_id.name <- .[["name"]]
            field_item_id.label <- .[["label"]]
            alias_name <- .[["aliasName"]]
            res <- flip_flop |> map_df(~ {
                # コードとフィールドIDのデカルト積を取得
                temp <- crossing(codes = list_c(.[["codes"]]), fields = list_c(.[["fields"]]))
                temp[["alias_name"]] <- alias_name
                temp[["id"]] <- .[["id"]]
                temp[["field_item_id"]] <- .[["field_item_id"]]
                temp[["field_item_id.name"]] <- field_item_id.name
                temp[["field_item_id.label"]] <- field_item_id.label
                return(temp)
            })
            return(res)
        })
        return(res)
    }) |>
        keep(~ !is.null(.)) |>
        bind_rows()
    aliasnameAndFieldIdAndLabel <- GetAliasnameAndFieldIdAndLabel(fieldItems)
    aliasnameAndFieldIdAndLabelGroups <- aliasnameAndFieldIdAndLabel %>% left_join(visitGroups, by = c("alias_name"))
    for (row in 1:nrow(aliasnameAndFieldIdAndLabelGroups)) {
        if (is.na(aliasnameAndFieldIdAndLabelGroups[row, "group"])) {
            aliasnameAndFieldIdAndLabelGroups[row, "group"] <- aliasnameAndFieldIdAndLabelGroups[row, "alias_name"]
        }
    }
    aliasnameAndFieldIdAndLabelGroups <- aliasnameAndFieldIdAndLabelGroups %>% select(-name, -visit_num, -visit_name)
    if (nrow(action) > 0) {
        df <- action |> inner_join(aliasnameAndFieldIdAndLabelGroups, by = c("alias_name", "fields"))
        df[["alias_name"]] <- df[["group"]]
        df <- df %>%
            select(-group) %>%
            distinct()
    } else {
        df <- action
    }
    res <- GetItemsSelectColnames(
        df,
        c("jpname", "alias_name", "field_item_id.name", "field_item_id.label", "codes", "fields", "fields.label"),
        jpNameAndGroup
    )
    return(res)
}
