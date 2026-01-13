#' edit_common.R
#'
#' @file edit_common.R
#' @author Mariko Ohtsuka
#' @date 2026.1.9
GetTargetByType <- function(field_items, type) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x[[.const[["kFieldItemsType"]]]])) {
                return(FALSE)
            } else if (.x[[.const[["kFieldItemsType"]]]] == type) {
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
                        name = .x[[.const[["kFieldItemsFieldId"]]]],
                        field_number = .x[[.const[["kFieldItemsFieldId"]]]] %>% str_extract("\\d+") %>% as.numeric(),
                        label = .x[[.const[["kFieldItemsFieldName"]]]],
                        field_seq = .x[[.const[["kFieldItemsSeq"]]]]
                    )
                    return(res)
                }) %>%
                bind_rows()
            fields[[.const[["kOutputJapanaseNameEnglish"]]]] <- json_file[[.const[["kSheetJapaneseName"]]]]
            fields[[.const[["kAliasName"]]]] <- json_file[[.const[["kAliasName"]]]]
            return(fields)
        }) %>%
        bind_rows()
    return(field_list)
}
CombineSheetSafety <- function(sheet_data_list) {
    targetSheetNames <- .const[["kTargetSheetNames"]] %>% append("name", .)
    sheet_data_combine <- targetSheetNames %>%
        map(~ map(sheet_data_list, pluck, .x) %>%
            compact() %>%
            bind_rows()) %>%
        set_names(targetSheetNames)
    # 0行0列のデータフレームを補完
    for (nm in names(sheet_data_combine)) {
        df <- sheet_data_combine[[nm]]
        if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
            if (!is.null(.const[["kEngColumnNames"]][[nm]])) {
                sheet_data_combine[[nm]] <- data.frame(matrix(ncol = length(.const[["kEngColumnNames"]][[nm]]), nrow = 0)) %>%
                    setNames(.const[["kEngColumnNames"]][[nm]])
            }
        }
    }
    return(sheet_data_combine)
}
