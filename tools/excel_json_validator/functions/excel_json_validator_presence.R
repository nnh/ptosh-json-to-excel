#' test script
#'
#' @file excel_json_validator_presence.R
#' @author Mariko Ohtsuka
#' @date 2025.12.16
CheckPresence <- function(sheetList, target_json, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetPresenceFromJson(target_json)
    sheet <- sheet %>% arrange(alias_name, name)
    json <- json %>% arrange(alias_name, name)
    return(CheckTarget(sheet, json))
}
GetPresenceFromJson <- function(target_json) {
    presence_sheets <- target_json$sheets
    sheetsIdx <- seq(length(presence_sheets), 1)
    for (sheetIdx in sheetsIdx) {
        aliasName <- presence_sheets[[sheetIdx]]$alias_name
        field_items <- presence_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            presence_sheets[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItemIdx in fieldItems_idx) {
            if (presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$type != "FieldItem::Article") {
                presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
                next
            }
            if (!is.null(presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$validators$presence)) {
                presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
                next
            }
        }
        field_items <- presence_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            presence_sheets[[sheetIdx]] <- NULL
            next
        }
        # 除外対象となるcdisc_sheet_configのフィールドを抽出する
        exclude_fields <- presence_sheets[[sheetIdx]]$cdisc_sheet_configs %>%
            map(~ {
                prefix <- .x$prefix

                stat_fields <- .x$table %>%
                    keep(~ .x == "STAT") %>%
                    names()

                orres_fields <- .x$table %>%
                    keep(~ .x == "ORRES" & prefix == "IE") %>%
                    names()

                c(stat_fields, orres_fields)
            }) %>%
            unlist()

        if (length(exclude_fields) == 0) {
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItemIdx in fieldItems_idx) {
            field_id <- presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$name
            if (field_id %in% exclude_fields) {
                presence_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
            }
        }
    }
    target_presences <- presence_sheets %>%
        keep(~ length(.x$field_items) > 0)

    presences <- tibble()
    for (i in seq_along(target_presences)) {
        aliasName <- target_presences[[i]]$alias_name
        res <- target_presences[[i]]$field_items %>%
            map(~ tibble(
                alias_name = aliasName,
                name = .x$name,
                label = .x$label
            )) %>%
            bind_rows()
        presences <- bind_rows(presences, res)
    }
    df2 <- JoinVisitGroupsValidator(presences, key = "alias_name", target = "group") %>% distinct()
    res <- GetItemsSelectColnames(df2, c("jpname", "alias_name", "name", "label"), jpNameAndGroup)
}
