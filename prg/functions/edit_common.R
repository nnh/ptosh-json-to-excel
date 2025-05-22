GetTargetByType <- function(field_items, type) {
    target <- field_items %>%
        keep(~ {
            if (is.null(.x$type)) {
                return(FALSE)
            } else if (.x$type == type) {
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
EditRefFieldTextVec <- function(df_sheet_field) {
    join_field_info <- dplyr::left_join(df_sheet_field, field_list, by = c("alias_name", "field_number"))
    join_field_info$text <- paste(join_field_info$alias_name, join_field_info$name, join_field_info$label, sep = ",") %>%
        paste0("(", ., ")")
    return(join_field_info)
}
EditRefFieldText <- function(df_sheet_field) {
    join_field_info <- EditRefFieldTextVec(df_sheet_field)
    res <- join_field_info$text %>%
        unique() %>%
        paste(collapse = "")
    return(res)
}
ExtractAliasAndField <- function(x, thisSheetName) {
    if (str_detect(x, "^ref\\(")) {
        sheet_field <- x %>%
            str_remove_all("^ref\\(|\\)$") %>%
            str_remove_all("'") %>%
            str_split(",\\s*") %>%
            unlist()
        alias <- sheet_field[1]
        number <- as.numeric(sheet_field[2])
    } else if (str_detect(x, "^(field|f)\\d+$")) {
        alias <- thisSheetName
        number <- as.numeric(str_extract(x, "\\d+"))
    } else {
        alias <- NA_character_
        number <- NA_real_
    }
    list(alias = alias, number = number)
}
GetDfSheetField <- function(target, thisSheetName) {
    kFieldText <- c("ref\\('\\w[\\w\\d\\p{Punct}]*', \\d+\\)", "f\\d+", "field\\d+", "ref\\('[^']+',\\s*\\d+\\)")
    fieldTextList <- unlist(lapply(kFieldText, function(pattern) str_extract_all(target, pattern))) %>% unique()

    if (length(fieldTextList) == 0) {
        return(NULL)
    }

    df_sheet_field <- tibble::tibble(
        raw = fieldTextList
    ) %>%
        mutate(
            parsed = purrr::map(raw, ~ ExtractAliasAndField(.x, thisSheetName)),
            alias_name = purrr::map_chr(parsed, "alias"),
            field_number = purrr::map_dbl(parsed, "number")
        ) %>%
        select(-parsed)
    return(df_sheet_field)
}
GetFieldText <- function(target, thisSheetName) {
    df_sheet_field <- GetDfSheetField(target, thisSheetName)
    if (is.null(df_sheet_field)) {
        return(NULL)
    }
    df_refFieldText <- EditRefFieldTextVec(df_sheet_field)
    temp_ref <- target
    res <- list()
    for (i in 1:nrow(df_refFieldText)) {
        if (str_detect(target, fixed(df_refFieldText$raw[i]))) {
            res <- append(res, df_refFieldText[i, "text"])
            temp_ref <- str_remove_all(temp_ref, fixed(df_refFieldText$raw[i]))
        }
    }
    if (length(res) == 0) {
        return(NULL)
    }
    res <- paste(res, collapse = "")
    return(res)
}
