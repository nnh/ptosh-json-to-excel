#' test script
#'
#' @file excel_json_validator_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.12.17
CheckJsonVisitForVisit <- function(visitJson) {
    res <- visitJson %>%
        map_df(~ {
            visit <- .x
            res <- data.frame(
                name = visit[["alias_name"]] %>% str_split("_") %>% map_chr(~ tail(.x, 1)) %>% as.numeric(),
                default_value = visit[["name"]] %>% str_remove("\\)$") %>% str_split("\\(") %>% map_chr(~ tail(.x, 1))
            )
            return(res)
        }) %>%
        arrange(name) %>%
        distinct()
    res$name <- as.character(res$name)
    return(res)
}
CheckJsonVisitForNonVisit <- function() {
    visit_sheets <- target_json$sheets
    sheetsIdx <- seq(length(visit_sheets), 1)
    for (sheetIdx in sheetsIdx) {
        aliasName <- visit_sheets[[sheetIdx]]$alias_name
        field_items <- visit_sheets[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            visit_sheets[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItemIdx in fieldItems_idx) {
            if (visit_sheets[[sheetIdx]]$field_items[[fieldItemIdx]]$label != "Visit Number") {
                visit_sheets[[sheetIdx]]$field_items[[fieldItemIdx]] <- NULL
                next
            }
        }
        if (is.null(visit_sheets[[sheetIdx]]$field_items) || length(visit_sheets[[sheetIdx]]$field_items) == 0) {
            visit_sheets[[sheetIdx]] <- NULL
            next
        }
    }
    visit_sheets <- visit_sheets %>% keep(~ !is.null(.x$field_items) && length(.x$field_items) > 0)
    res <- visit_sheets %>%
        map(~ {
            jpName <- .x$name
            aliasName <- .x$alias_name
            field_items <- .x$field_items
            res <- field_items %>%
                map(~ {
                    list(
                        jpname = jpName,
                        alias_name = aliasName,
                        name = .x$name,
                        default_value = .x$default_value
                    )
                }) %>%
                bind_rows()
            return(res)
        }) %>%
        bind_rows()
    res <- res %>% data.frame()
    return(res)
}
CheckVisit <- function(sheetList, sheetName) {
    visitJson <- target_json[["sheets"]] %>% keep(~ .[["category"]] == "visit")
    if (length(visitJson) == 0) {
        json <- CheckJsonVisitForNonVisit()
        sheet <- sheetList[[sheetName]] |>
            rename(!!!engToJpnColumnMappings[[sheetName]])
        json <- json %>% arrange(alias_name, name)
        sheet <- sheet %>% arrange(alias_name, name)
    } else {
        json <- CheckJsonVisitForVisit(visitJson)
        sheet <- sheetList[[sheetName]] |>
            rename(!!!engToJpnColumnMappings[["visit_to_visit"]])
        sheet$name <- sheet$name %>% as.character()
    }
    return(CheckTarget(sheet, json))
}
