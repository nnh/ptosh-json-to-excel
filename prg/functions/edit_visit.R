#' edit_visit.R
#'
#' @file edit_visit.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
GetVisit <- function(field_items, sheet) {
    kNonvisitCondition <- "Visit Number"
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[[.const[["kFieldItemsFieldName"]]]])) {
                return(FALSE)
            } else if (.x[[.const[["kFieldItemsFieldName"]]]] == kNonvisitCondition) {
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
                name = .x[[.const[["kFieldItemsFieldId"]]]],
                default_value = .x[[.const[["kFieldItemDefaultValue"]]]] %||% NA,
            )
            return(res)
        })

    res <- JoinJpnameAndAliasNameAndSelectColumns(visit, .const[["kVisit"]], sheet)
    return(res)
}
GetVisitGroupsFromJson <- function(json_files) {
    visit_groups <- json_files[[.const[["kVisitGroup"]]]] %>%
        map(~ {
            name <- .x[["name"]]
            alias_name <- .x[[.const[["kAliasName"]]]]
            reference_sheet <- .x[["reference_sheet"]]
            res <- .x[["visit_sheets"]] %>% map_df(~ {
                tibble::tibble(
                    visit_group_name = name,
                    visit_group = alias_name,
                    alias_name = .x[["sheet_alias_name"]],
                    visitnum = .x[["visit_num"]] %>% as.numeric(),
                    reference_sheet = reference_sheet
                )
            })
        }) %>%
        bind_rows()
    return(visit_groups)
}
GetVisitsFromJson <- function(json_files) {
    visits <- json_files[[.const[["kVisits"]]]] %>%
        map_df(~ {
            tibble::tibble(
                visit_name = .x[["name"]],
                visitnum = .x[["num"]] %>% as.numeric()
            )
        }) %>%
        arrange(visitnum)
    return(visits)
}
GetVisitGroupAndVisitsFromJson <- function(json_files) {
    visit_groups <- GetVisitGroupsFromJson(json_files)
    visits <- GetVisitsFromJson(json_files)
    res <- visit_groups %>%
        left_join(visits, by = c("visitnum" = "visitnum"))
    return(res)
}
GetVisitIsVisit <- function() {
    visit <- visit_info %>%
        select(visitnum, visit_name) %>%
        arrange(visitnum) %>%
        distinct()
    colnames(visit) <- c(.const[["kFieldItemsFieldId"]], .const[["kFieldItemDefaultValue"]])
    return(visit)
}
