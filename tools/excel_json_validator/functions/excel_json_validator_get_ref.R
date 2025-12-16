#' test script
#'
#' @file excel_json_validator_get_ref.R
#' @author Mariko Ohtsuka
#' @date 2025.12.16
GetFieldInfoForGetRef <- function() {
    fieldInfo <- target_json$sheets %>%
        map(~ {
            aliasName <- .x$alias_name
            res <- .x$field_items %>%
                map(~ {
                    list(
                        alias_name = aliasName,
                        fieldId = .x$name,
                        label = .x$label
                    )
                }) %>%
                bind_rows()
        }) %>%
        bind_rows()
    res <- fieldInfo %>% left_join(visitGroups, by = c("alias_name" = "alias_name"))
    res$group <- ifelse(is.na(res$group), res$alias_name, res$group)
    return(res)
}
ReplaceFieldForReference <- function(targetText, targetSheetName, fieldInfoForGetReference) {
    fieldList <- targetText %>%
        str_extract_all("field[0-9]+") %>%
        unlist() %>%
        unique()
    refText <- fieldList %>%
        map_chr(~ {
            fieldId <- .x
            temp <- fieldInfoForGetReference %>%
                filter(alias_name == targetSheetName & fieldId == !!fieldId)
            if (nrow(temp) == 0) {
                return(NA_character_)
            } else {
                return(str_c("(", temp$group, ",", fieldId, ",", temp$label, ")"))
            }
        }) %>%
        str_c(collapse = ", ")
    return(refText)
}
