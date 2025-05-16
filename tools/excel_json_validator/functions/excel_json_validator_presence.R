#' test script
#'
#' @file excel_json_validator_presence.R
#' @author Mariko Ohtsuka
#' @date 2025.5.16
CheckPresence <- function(sheetList) {
    sheetName <- "presence"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetPresenceFromJson()
    return(CheckTarget(sheet, json))
}

GetPresenceFromJson <- function() {
    jpnameAndAliasnameAndSheetId <- jsonList %>% map_df(~ list(sheet_id = .x$id, jpname = .x$name, alias_name = .x$alias_name))
    articles <- fieldItems %>%
        map(~ keep(.x, ~ .x$type == "FieldItem::Article"))
    no_validators_presence <- articles %>%
        list_c() %>%
        keep(~ is.null(.x$validators$presence))
    df_no_validators_presence <- no_validators_presence %>%
        map_df(~ {
            sheet_id <- .x$sheet_id
            name <- .x$name
            label <- .x$label
            return(data.frame(sheet_id = sheet_id, name = name, label = label))
        })
    df_no_validators_presence_jpname_aliasname <- df_no_validators_presence %>%
        inner_join(jpnameAndAliasnameAndSheetId, by = c("sheet_id" = "sheet_id")) %>%
        arrange(alias_name, sheet_id)
    cdiscSheetConfigs <- jsonList %>%
        map(~ .x$cdisc_sheet_config)
    excludeTargetsStat <- cdiscSheetConfigs %>%
        list_c() %>%
        map_df(~ {
            prefix <- .x$prefix
            sheet_id <- .x$sheet_id
            table <- .x$table
            fields <- NULL
            for (i in seq_along(table)) {
                if (is.null(table[[i]])) {
                    next
                }
                if (table[[i]] != "STAT") {
                    next
                }
                fields <- list(name = names(table)[[i]], value = table[[i]])
            }
            res <- fields %>% map_df(~.x)
            res$prefix <- prefix
            res$sheet_id <- sheet_id
            return(res)
        })

    excludeTargetsIe <- cdiscSheetConfigs %>%
        map(~ keep(.x, ~ .x$prefix == "IE")) %>%
        keep(~ length(.x) > 0) %>%
        list_c()
    excludeTargetsIeorres <- excludeTargetsIe %>%
        map_df(~ {
            prefix <- .x$prefix
            sheet_id <- .x$sheet_id
            table <- .x$table
            fields <- NULL
            for (i in seq_along(table)) {
                if (is.null(table[[i]])) {
                    next
                }
                if (table[[i]] != "ORRES") {
                    next
                }
                fields <- list(name = names(table)[[i]], value = table[[i]])
            }
            res <- fields %>% map_df(~.x)
            res$prefix <- prefix
            res$sheet_id <- sheet_id
            return(res)
        })
    excludeTargets <- excludeTargetsIeorres %>%
        bind_rows(excludeTargetsStat) %>%
        inner_join(jpnameAndAliasnameAndSheetId, by = c("sheet_id" = "sheet_id")) %>%
        arrange(alias_name, sheet_id)
    presenceExcludeTargets <- df_no_validators_presence_jpname_aliasname %>%
        anti_join(excludeTargets, by = c("sheet_id" = "sheet_id", "name" = "name")) %>%
        select(c("jpname", "alias_name", "name", "label"))
    if (nrow(presenceExcludeTargets) == 0) {
        res <- data.frame(
            jpname = "",
            alias_name = "",
            name = "",
            label = "",
            stringsAsFactors = FALSE
        )
    } else {
        res <- presenceExcludeTargets
    }
    return(res)
}
