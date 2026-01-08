#' 論理式参照先テキストを生成する処理
#'
#' @file replace_ref_text.R
#' @author Mariko Ohtsuka
#' @date 2025.12.19
#
EditRefFieldTextVec <- function(df_sheet_field) {
    join_field_info <- dplyr::left_join(df_sheet_field, field_list, by = c("alias_name", "field_number")) %>% select(-field_seq)
    join_field_info <- join_field_info %>%
        dplyr::left_join(visit_info, by = "alias_name")
    join_field_info$output_alias_name <- ifelse(is.na(join_field_info$visit_group), join_field_info$alias_name, join_field_info$visit_group)
    join_field_info[["text"]] <- paste(join_field_info[["output_alias_name"]], join_field_info[["name"]], join_field_info[["label"]], sep = ",") %>%
        paste0("(", ., ")")
    return(join_field_info)
}
EditRefFieldText <- function(df_sheet_field) {
    join_field_info <- EditRefFieldTextVec(df_sheet_field)
    res <- join_field_info[["text"]] %>%
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
RegexEscape <- function(text) {
    # 正規表現の特殊文字をバックスラッシュでエスケープ
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text)
}
GetFieldText <- function(target, thisSheetName) {
    df_sheet_field <- GetDfSheetField(target, thisSheetName)
    if (is.null(df_sheet_field)) {
        return(NULL)
    }
    df_refFieldText <- EditRefFieldTextVec(df_sheet_field)
    temp_ref <- target
    for (i in 1:nrow(df_refFieldText)) {
        raw_pattern <- RegexEscape(df_refFieldText$raw[i])
        # 後に数字が続かない場合のみマッチ
        regex_pattern <- paste0("(", raw_pattern, ")(?![0-9])")
        if (str_detect(temp_ref, regex(regex_pattern))) {
            temp_ref <- str_replace_all(temp_ref, regex(regex_pattern), df_refFieldText[["text"]][i])
        }
    }
    text_patterns <- df_refFieldText[["text"]]
    matched_with_pos <- sapply(text_patterns, function(pat) regexpr(pat, temp_ref, fixed = TRUE)[[1]])
    res <- text_patterns[matched_with_pos != -1][order(matched_with_pos[matched_with_pos != -1])] %>% unique()

    if (length(res) == 0) {
        return(NULL)
    }
    res <- paste(res, collapse = "")
    return(res)
}
