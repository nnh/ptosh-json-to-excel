#' test script
#'
#' @file excel_json_validator_get_ref.R
#' @author Mariko Ohtsuka
#' @date 2025.12.17
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
    targetText <- targetText %>% str_replace_all("\\s+", "")
    fieldList <- str_extract_all(targetText, "((field|f)[0-9]+|ref\\('[A-Za-z0-9]+',[0-9]+\\))") %>%
        unlist() %>%
        unique()
    if (length(fieldList) == 0) {
        return(NA_character_)
    }
    if (length(fieldList) == 1) {
        if (is.na(fieldList) || fieldList == "") {
            return(NA_character_)
        }
    }
    res <- c()
    for (i in 1:length(fieldList)) {
        if (str_detect(fieldList[i], "^ref\\('([A-Za-z0-9]+)',([0-9]+)\\)$")) {
            refSheetName <- str_match(fieldList[i], "^ref\\('([A-Za-z0-9]+)',([0-9]+)\\)$")[, 2]
            refFieldId <- str_match(fieldList[i], "^ref\\('([A-Za-z0-9]+)',([0-9]+)\\)$")[, 3] %>% str_c("field", .)
            refText <- MakeReferenceLabelForValidate(
                fieldInfoForGetReference,
                refSheetName,
                refFieldId
            )
        } else {
            refFieldId <- str_replace(fieldList[i], "^f([0-9]+)$", "field\\1")
            refText <- MakeReferenceLabelForValidate(
                fieldInfoForGetReference,
                targetSheetName,
                refFieldId
            )
        }
        res <- c(res, refText)
    }
    res <- res %>% str_c(collapse = "")
    return(res)
}
MakeReferenceLabelForValidate <- function(fieldInfoForGetReference,
                                          targetSheetName,
                                          fieldId) {
    temp <- fieldInfoForGetReference %>%
        dplyr::filter(
            alias_name == targetSheetName,
            fieldId == !!fieldId
        )

    if (nrow(temp) == 0) {
        return(NA_character_)
    }

    stringr::str_c("(", temp$group, ",", fieldId, ",", temp$label, ")")
}
