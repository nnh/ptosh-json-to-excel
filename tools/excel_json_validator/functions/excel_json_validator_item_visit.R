#' test script
#'
#' @file excel_json_validator_item_visit.R
#' @author Mariko Ohtsuka
#' @date 2025.12.17
CheckItemVisit <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]]
    json_sheetNames <- itemVisitData[["jpname"]] %>%
        unlist() %>%
        unique()
    json_labels <- itemVisitData[["label"]] %>%
        unique()
    json <- data.frame(ラベル = json_labels)
    for (col in json_sheetNames) {
        json[[col]] <- NA
    }
    numericality_normal_range_check <- tibble(
        "ラベル" = rep(NA_character_, nrow(json)),
        "数値チェック・アラート条件の有無" = rep(NA, nrow(json))
    )

    colnames(json) <- c("ラベル", json_sheetNames)
    for (row in 1:nrow(json)) {
        for (col in json_sheetNames) {
            label_str <- json[row, "ラベル", drop = TRUE]
            target_rows <- itemVisitData %>%
                filter(jpname == col & label == label_str)
            count_values <- target_rows %>%
                nrow()
            if (length(count_values) == 0) {
                count_values <- 0
            }
            json[row, col] <- count_values
        }
        target_rows <- itemVisitData %>%
            filter(label == label_str)
        check_col <- target_rows %>%
            select(numericality_normal_range_check) %>%
            unlist() %>%
            unique()
        numericality_normal_range_check[row, "ラベル"] <- label_str
        numericality_normal_range_check[row, "数値チェック・アラート条件の有無"] <- check_col
    }
    colnames(json) <- colnames(json) %>% str_replace_all(" ", ".")
    json <- json %>%
        left_join(numericality_normal_range_check, by = "ラベル")

    if (!identical(sort(colnames(json)), sort(colnames(sheet)))) {
        stop(str_c("Column names in sheet '", sheetName, "' do not match with JSON."))
    }
    json <- json %>%
        select(all_of(colnames(sheet)))
    json <- json %>%
        arrange(ラベル)
    sheet <- sheet %>%
        arrange(ラベル)
    json <- json %>%
        mutate(across(everything(), as.character))
    sheet <- sheet %>%
        mutate(across(everything(), as.character))
    return(CheckTarget(json, sheet))
}
