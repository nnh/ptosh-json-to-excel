#' test script
#'
#' @file excel_json_validator_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.7.28
CheckJsonVisitForVisit <- function(visitJson) {
    res <- visitJson %>%
        map_df(~ {
            visit <- .x
            res <- data.frame(
                jpname = visit[["name"]],
                alias_name = visit[["alias_name"]],
                name = visit[["name"]] %>% str_remove("\\)$") %>% str_split("\\(") %>% map_chr(~ tail(.x, 1)),
                default_value = visit[["alias_name"]] %>% str_split("_") %>% map_chr(~ tail(.x, 1)) %>% as.numeric()
            )
            return(res)
        }) %>%
        arrange(default_value)
    res$default_value <- as.character(res$default_value)
    return(res)
}
CheckVisit <- function(sheetList, jpNameAndAliasName, sheetName, jsonList) {
    if (trialName == "TAS0728-HER2") {
        print("TAS0728-HER2はVISITチェック処理をスキップします")
        return(NULL)
    }
    if (trialName == "gpower") {
        print("gpowerはVISITチェック処理をスキップします")
        return(NULL)
    }
    if (trialName == "allr23") {
        print("allr23はVISITチェック処理をスキップします")
        return(NULL)
    }
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    sheet$default_value <- sheet$default_value %>% as.character()
    visitJson <- jsonList %>% keep(~ .[["category"]] == "visit")
    if (length(visitJson) == 0) {
        visit_fieldItems <- jsonList |> GetFieldItemsByJsonList()
        json <- GetVisitFromJson(visit_fieldItems, jpNameAndAliasName)
    } else {
        json <- CheckJsonVisitForVisit(visitJson)
    }
    return(CheckTarget(sheet, json))
}
GetVisitFromJson <- function(fieldItems, jpNameAndAliasName) {
    df <- map2(fieldItems, names(fieldItems), ~ {
        fieldItem <- .x
        aliasName <- .y
        res <- fieldItem |> keep(~ .[["label"]] == "Visit Number")
        visit <- res |> map_df(~ list(name = .[["name"]], default_value = .[["default_value"]]))
        visit[["alias_name"]] <- aliasName
        return(visit)
    }) |> bind_rows()
    res <- GetItemsSelectColnames(df, c("jpname", "alias_name", "name", "default_value"), jpNameAndAliasName)
    return(res)
}
