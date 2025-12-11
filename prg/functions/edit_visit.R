#' edit_visit.R
#'
#' @file edit_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
GetVisit <- function(field_items, sheet) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[["label"]])) {
                return(FALSE)
            } else if (.x[["label"]] == "Visit Number") {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    visit <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x[["name"]],
                default_value = .x[["default_value"]] %||% NA,
            )
            return(res)
        })

    res <- JoinJpnameAndAliasNameAndSelectColumns("visit", sheet)
    return(res)
}
GetVisitVisitName <- function(name) {
    res <- str_extract(name, "\\([^()]+\\)$") %>% str_remove_all("[()]")
    return(res)
}
GetVisitVisitNum <- function(alias_name) {
    res <- alias_name %>%
        str_extract("_\\d+$") %>%
        str_remove("_") %>%
        as.numeric()
    return(res)
}
CreateVisitToVisitSheetData <- function(sheets) {
    visit_json_files_group <- EditGroupVisit(sheets)
    visit <- visit_json_files_group %>%
        map_df(~ {
            json_file <- .
            name <- json_file[["name"]]
            alias_name <- json_file[["alias_name"]]
            visit <- GetVisitVisitName(name)
            visit_num <- GetVisitVisitNum(alias_name)
            tibble::tibble(
                jpname = name, alias_name = alias_name,
                name = visit_num, default_value = visit
            )
        })
    sort_visit <- visit %>% arrange(name)
    return(sort_visit)
}
GetIsInVisitGroup <- function(sheet_alias_name) {
    if (is_visit) {
        # sheet[["alias_name"]]がvisit_groups[["sheet_alias_name"]]に存在するか判定
        is_in_visit_group <- CheckExistenceOfVisitGroup(sheet_alias_name, visit_groups)
    } else {
        is_in_visit_group <- FALSE
    }
    return(is_in_visit_group)
}
GetIsInVisitGroupMin <- function(alias_name) {
    is_visit_min <- alias_name %in% visit_groups_min$alias_name
    return(is_visit_min)
}
GetVisitGroups <- function(is_visit, json_files) {
    if (!is_visit) {
        return(NULL)
    }
    visit_groups <- GetListSetName(json_files, "visit_groups", "alias_name") %>%
        map_dfr(function(group) {
            map_dfr(group$visit_sheets, function(sheet) {
                tibble(
                    name = group$name,
                    visit = group$alias_name,
                    alias_name = sheet$sheet_alias_name,
                    visitnum = sheet$visit_num %>% as.numeric()
                )
            })
        })
    return(visit_groups)
}
