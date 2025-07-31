GetVisit <- function(field_items) {
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
    res <- target %>%
        map_df(~ {
            res <- tibble::tibble(
                name = .x[["name"]],
                default_value = .x[["default_value"]] %||% NA,
            )
            return(res)
        })
    return(res)
}
CreateVisitToVisitSheetData <- function() {
    visit <- visit_json_files %>%
        map_df(~ {
            json_file <- GetJsonFile(.)
            name <- json_file[["name"]]
            alias_name <- json_file[["alias_name"]]
            visit <- str_extract(name, "\\([^()]+\\)$") %>% str_remove_all("[()]")
            visit_num <- alias_name %>%
                str_extract("_\\d+$") %>%
                str_remove("_") %>%
                as.numeric()
            tibble::tibble(
                jpname = name, alias_name = alias_name,
                name = visit_num, default_value = visit
            )
        })
    sort_visit <- visit %>% arrange(name)
    return(sort_visit)
}
