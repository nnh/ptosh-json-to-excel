#' edit_common.R
#'
#' @file edit_common.R
#' @author Mariko Ohtsuka
#' @date 2025.12.11
GetTargetByType <- function(field_items, type) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[["type"]])) {
                return(FALSE)
            } else if (.x[["type"]] == type) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
    if (length(target) == 0) {
        return(NULL)
    }
    return(target)
}
GetFieldList <- function(sheets) {
    field_list <- sheets %>%
        map(~ {
            json_file <- .
            field_items <- json_file %>% GetFieldItems()
            fields <- field_items %>%
                map(~ {
                    res <- tibble::tibble(
                        name = .x[["name"]],
                        field_number = .x[["name"]] %>% str_extract("\\d+") %>% as.numeric(),
                        label = .x[["label"]]
                    )
                    return(res)
                }) %>%
                bind_rows()
            fields[["jpname"]] <- json_file[["name"]]
            fields[["alias_name"]] <- json_file[["alias_name"]]
            return(fields)
        }) %>%
        bind_rows()
    return(field_list)
}
CombineSheetSafety <- function(sheet_data_list) {
    targetSheetNames <- kTargetSheetNames %>% append("name", .)
    sheet_data_combine <- targetSheetNames %>%
        map(~ map(sheet_data_list, pluck, .x) %>%
            compact() %>%
            bind_rows()) %>%
        set_names(targetSheetNames)
    # 0行0列のデータフレームを補完
    for (nm in names(sheet_data_combine)) {
        df <- sheet_data_combine[[nm]]
        if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
            if (!is.null(kEngColumnNames[[nm]])) {
                sheet_data_combine[[nm]] <- data.frame(matrix(ncol = length(kEngColumnNames[[nm]]), nrow = 0)) %>%
                    setNames(kEngColumnNames[[nm]])
            }
        }
    }
    return(sheet_data_combine)
}
