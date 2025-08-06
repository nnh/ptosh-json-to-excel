#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.8.6
CheckItemVisit <- function(json, sheetName, sheetList) {
    sheet <- sheetList[[sheetName]]
    jsonItemVisit <- json
    sheet_colnames <- sheet %>%
        colnames() %>%
        sort()
    jsonItemVisit$jpname <- jsonItemVisit$jpname %>% str_replace_all(" +", ".")
    jsonItemVisit[["group"]] <- str_remove(jsonItemVisit[["jpname"]], "\\((\\w|\\.|-)*\\)$")
    json_jpnames <- jsonItemVisit$jpname %>%
        unlist() %>%
        str_remove("\\((\\w|\\.|-)*\\)$") %>%
        unique()
    json_colnames <- json_jpnames %>%
        c(., "ラベル", "数値チェック・アラート条件の有無") %>%
        sort()
    colname_identical <- identical(sheet_colnames, json_colnames)
    if (!colname_identical) {
        stop(str_c("Column names in sheet '", sheetName, "' do not match with JSON."))
    }
    sheet_labels <- sheet[["ラベル"]] %>%
        unlist() %>%
        sort()
    json_labels <- jsonItemVisit[["label"]] %>%
        unlist() %>%
        unique() %>%
        sort()
    labels_identical <- identical(sheet_labels, json_labels)
    if (!labels_identical) {
        stop(str_c("Labels in sheet '", sheetName, "' do not match with JSON."))
    }
    json_tibble <- tibble()
    for (col in json_colnames) {
        json_tibble[[col]] <- NA
    }
    for (col in json_jpnames) {
        for (row in 1:length(sheet_labels)) {
            sheet_label <- sheet_labels[row]
            json_values <- jsonItemVisit %>%
                filter(label == sheet_label & group == col) %>%
                nrow()
            if (length(json_values) == 0) {
                json_values <- 0
            }
            json_check <- jsonItemVisit %>%
                filter(label == sheet_label) %>%
                select(numericality_normal_range_check) %>%
                unlist() %>%
                unique()
            json_tibble[row, "ラベル"] <- sheet_labels[row]
            json_tibble[row, col] <- json_values
            json_tibble[row, "数値チェック・アラート条件の有無"] <- ifelse(length(json_check) == 0, NA, json_check)
        }
    }
    result_json <- as.data.frame(
        json_tibble %>%
            select(all_of(sheet_colnames)) %>%
            arrange(match(.[["ラベル"]], sheet_labels)) %>%
            mutate(across(everything(), as.character))
    )
    result_sheet <- sheet %>%
        select(all_of(sheet_colnames)) %>%
        arrange(match(.[["ラベル"]], sheet_labels)) %>%
        mutate(across(everything(), as.character))
    return(CheckTarget(result_sheet, result_json))
}
