#' test script
#'
#' @file excel_json_validator_item.R
#' @author Mariko Ohtsuka
#' @date 2025.12.17
CheckItemVisitOld <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetItemVisitOldFromJson()
    itemVisitData <<- json
    sheet <- sheet %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    json <- json %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    return(CheckTarget(sheet, json))
}
GetItemVisitOldFromJson <- function() {
    item_json <- target_json[["sheets"]] %>% keep(~ .x[["category"]] == "visit")
    if (length(item_json) == 0) {
        res <- data.frame(
            jpname = "",
            alias_name = "",
            name = "",
            label = "",
            option.name = "",
            default_value = "",
            validators.presence.validate_presence_if = "",
            presence_if_references = "",
            validators.formula.validate_formula_if = "",
            formula_if_references = "",
            validators.formula.validate_formula_message = "",
            validators.date.validate_date_after_or_equal_to = "",
            references_after = "",
            validators.date.validate_date_before_or_equal_to = "",
            references_before = "",
            numericality_normal_range_check = "",
            stringsAsFactors = FALSE
        )
        return(res)
    }
    alert <- item_json %>%
        map(~ {
            aliasName <- .x[["alias_name"]]
            field_items <- .x[["field_items"]] %>%
                map(~ {
                    if (is.null(.x[["normal_range"]])) {
                        return(NULL)
                    }
                    if (length(.x[["normal_range"]]) == 0) {
                        return(NULL)
                    }
                    if (is.null(.x[["normal_range"]][["less_than_or_equal_to"]])) {
                        .x[["normal_range"]][["less_than_or_equal_to"]] <- ""
                    }
                    if (is.null(.x[["normal_range"]][["greater_than_or_equal_to"]])) {
                        .x[["normal_range"]][["greater_than_or_equal_to"]] <- ""
                    }
                    if (.x[["normal_range"]][["less_than_or_equal_to"]] == "" &
                        .x[["normal_range"]][["greater_than_or_equal_to"]] == "") {
                        return(NULL)
                    }
                    list(
                        alias_name = aliasName,
                        name = .x[["name"]],
                        less_than = .x[["normal_range"]][["less_than_or_equal_to"]],
                        greater_than = .x[["normal_range"]][["greater_than_or_equal_to"]]
                    )
                }) %>%
                discard(~ is.null(.))
            return(field_items)
        }) %>%
        discard(~ is.null(.) | length(.) == 0)
    item_visit_old <- EditItemFromJsonForValidate(item_json)
    item_visit_old$alert <- FALSE
    for (i in 1:length(alert[[1]])) {
        alert_aliasname <- alert[[1]][[i]][["alias_name"]]
        alert_fieldid <- alert[[1]][[i]][["name"]]
        for (row in 1:nrow(item_visit_old)) {
            if (item_visit_old[row, "alias_name"] == alert_aliasname) {
                if (item_visit_old[row, "name"] == alert_fieldid) {
                    item_visit_old[row, "alert"] <- TRUE
                }
            }
        }
    }
    for (row in 1:nrow(item_visit_old)) {
        target_alias_name <- item_visit_old[row, "alias_name"]
        target_name <- item_visit_old[row, "name"]
        alert_text <- ""
        for (i in 1:length(alert)) {
            if (alert[[1]][[i]][["alias_name"]] == target_alias_name) {
                if (alert[[1]][[i]][["name"]] == target_name) {
                    item_visit_old[row, "alert"] <- TRUE
                }
            }
        }
    }
    for (row in 1:nrow(item_visit_old)) {
        if (item_visit_old[row, "alert", drop = TRUE]) {
            if (item_visit_old[row, "numeric_check", drop = TRUE]) {
                item_visit_old[row, "numericality_normal_range_check"] <- "数値・アラート有"
            } else {
                item_visit_old[row, "numericality_normal_range_check"] <- "アラート設定有"
            }
        } else {
            if (item_visit_old[row, "numeric_check", drop = TRUE]) {
                item_visit_old[row, "numericality_normal_range_check"] <- "数値チェック有"
            } else {
                item_visit_old[row, "numericality_normal_range_check"] <- "条件なし"
            }
        }
    }
    df <- item_visit_old %>% select(-jpname, -fieldType, -numeric_check, -alert)
    df2 <- JoinVisitGroupsValidator(df, key = "alias_name", target = "group") %>% distinct()
    res <- GetItemsSelectColnames(df2, c(
        "jpname", "alias_name", "name", "label", "option.name", "default_value",
        "validators.presence.validate_presence_if",
        "presence_if_references",
        "validators.formula.validate_formula_if",
        "formula_if_references",
        "validators.formula.validate_formula_message",
        "validators.date.validate_date_after_or_equal_to",
        "references_after",
        "validators.date.validate_date_before_or_equal_to",
        "references_before",
        "numericality_normal_range_check"
    ), jpNameAndGroup)
    return(res)
}
CheckItemNonVisit <- function(sheetList, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetItemNonVisitFromJson()
    sheet <- sheet %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    json <- json %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
    return(CheckTarget(sheet, json))
}
GetItemNonVisitFromJson <- function() {
    item_json <- target_json[["sheets"]] %>% keep(~ .x[["category"]] != "visit")
    item_nonvisit <- EditItemFromJsonForValidate(item_json)
    item_nonvisit$field_type <- NA_character_
    for (row in 1:nrow(item_nonvisit)) {
        if (item_nonvisit[row, "fieldType", drop = TRUE] == "text" | item_nonvisit[row, "fieldType", drop = TRUE] == "text_area") {
            if (item_nonvisit[row, "numeric_check", drop = TRUE]) {
                item_nonvisit[row, "field_type"] <- "数値"
            } else {
                item_nonvisit[row, "field_type"] <- "テキスト"
            }
        }
    }
    res <- item_nonvisit %>% select(-fieldType, -numeric_check)
    return(res)
}
EditItemFromJsonForValidate <- function(item_json) {
    sheetsIdx <- seq(length(item_json), 1)
    for (sheetIdx in sheetsIdx) {
        field_items <- item_json[[sheetIdx]]$field_items
        if (is.null(field_items) || length(field_items) == 0) {
            item_json[[sheetIdx]] <- NULL
            next
        }
        fieldItems_idx <- seq(length(field_items), 1)
        for (fieldItem_idx in fieldItems_idx) {
            if (item_json[[sheetIdx]]$field_items[[fieldItem_idx]][["type"]] != "FieldItem::Article") {
                item_json[[sheetIdx]]$field_items[[fieldItem_idx]] <- NULL
                next
            }
        }
        if (length(item_json[[sheetIdx]]$field_items) == 0) {
            item_json[[sheetIdx]] <- NULL
            next
        }
    }
    if (length(item_json) == 0) {
        return(NULL)
    }
    item_tibble <- item_json %>%
        map(~ {
            jpName <- .x[["name"]]
            aliasName <- .x[["alias_name"]]
            field_items <- .x[["field_items"]] %>%
                map(~ {
                    tibble(
                        jpname = jpName,
                        alias_name = aliasName,
                        name = .x[["name"]],
                        label = .x[["label"]],
                        option.name = .x[["option_name"]] %||% NA_character_,
                        default_value = .x[["default_value"]] %||% NA_character_,
                        validators.presence.validate_presence_if = .x[["validators"]][["presence"]][["validate_presence_if"]] %||% NA_character_,
                        presence_if_references = NA_character_,
                        validators.formula.validate_formula_if = .x[["validators"]][["formula"]][["validate_formula_if"]] %||% NA_character_,
                        formula_if_references = NA_character_,
                        validators.formula.validate_formula_message = .x[["validators"]][["formula"]][["validate_formula_message"]] %||% NA_character_,
                        validators.date.validate_date_after_or_equal_to = .x[["validators"]][["date"]][["validate_date_after_or_equal_to"]] %||% NA_character_,
                        references_after = NA_character_,
                        validators.date.validate_date_before_or_equal_to = .x[["validators"]][["date"]][["validate_date_before_or_equal_to"]] %||% NA_character_,
                        references_before = NA_character_,
                        fieldType = .x[["field_type"]],
                        numeric_check = !is.null(.x[["validators"]][["numericality"]])
                    )
                }) %>%
                bind_rows()
        }) %>%
        bind_rows()
    for (row in 1:nrow(item_tibble)) {
        target_presence_if <- item_tibble[row, "validators.presence.validate_presence_if"] %>% pull()
        if (!is.na(target_presence_if)) {
            refText <- ReplaceFieldForReference(target_presence_if, item_tibble[row, "alias_name"] %>% pull(), fieldInfoForGetReference)
            item_tibble[row, "presence_if_references"] <- refText
        }
        target_formula_if <- item_tibble[row, "validators.formula.validate_formula_if"] %>% pull()
        if (!is.na(target_formula_if)) {
            refText <- ReplaceFieldForReference(target_formula_if, item_tibble[row, "alias_name"] %>% pull(), fieldInfoForGetReference)
            item_tibble[row, "formula_if_references"] <- refText
        }
        target_date_before <- item_tibble[row, "validators.date.validate_date_before_or_equal_to"] %>% pull()
        if (!is.na(target_date_before)) {
            refText <- ReplaceFieldForReference(target_date_before, item_tibble[row, "alias_name"] %>% pull(), fieldInfoForGetReference)
            item_tibble[row, "references_before"] <- refText
        }
        target_date_after <- item_tibble[row, "validators.date.validate_date_after_or_equal_to"] %>% pull()
        if (!is.na(target_date_after)) {
            refText <- ReplaceFieldForReference(target_date_after, item_tibble[row, "alias_name"] %>% pull(), fieldInfoForGetReference)
            item_tibble[row, "references_after"] <- refText
        }
    }
    item_tibble <- as.data.frame(item_tibble)
    return(item_tibble)
}
