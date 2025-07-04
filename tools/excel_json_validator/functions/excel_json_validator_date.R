#' test script
#'
#' @file excel_json_validator_date.R
#' @author Mariko Ohtsuka
#' @date 2025.7.4
CheckDate <- function(sheetList, jsonList) {
    sheetName <- "date"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    outputValidators <- sheet %>% select(
        jpname, alias_name, name, label, validators.date.validate_date_after_or_equal_to, references_after, validators.date.validate_date_before_or_equal_to, references_before
    )

    inputValidators <- GetValidatorsDateByTrial(jsonList)
    inputValidatorsAndReferences <- inputValidators %>%
        GetRefBefAft("date_before") %>%
        GetRefBefAft("date_after")
    checkValidators <- CheckValidatorsDate(inputValidatorsAndReferences, outputValidators)
    if (is.null(inputValidators)) {
        res <- NULL
    } else {
        res <- checkValidators
    }
    return(res)
}
CheckValidatorsDate <- function(inputValidators, outputValidators) {
    error_f <- FALSE
    inputValidators <- inputValidators %>% arrange(alias_name, name)
    inputValidators <- inputValidators %>%
        filter(!is.na(date_before) | !is.na(date_after)) %>%
        filter(date_before != "" | date_after != "")
    outputValidators <- outputValidators %>%
        arrange(alias_name, name) %>%
        filter(!is.na(validators.date.validate_date_after_or_equal_to) | !is.na(validators.date.validate_date_before_or_equal_to)) %>%
        filter(validators.date.validate_date_after_or_equal_to != "" | validators.date.validate_date_before_or_equal_to != "")

    if (nrow(inputValidators) != nrow(outputValidators)) {
        error_f <- TRUE
        print("!error! : Number of rows in input and output validators do not match.")
    }
    if (nrow(inputValidators) == 0) {
        print("No validators to check.")
        return(NULL)
    }
    for (row in 1:nrow(inputValidators)) {
        inputRow <- inputValidators[row, ]
        outputRow <- outputValidators[row, ]
        if (inputRow[["jpname"]] != outputRow[["jpname"]]) {
            error_f <- TRUE
            print(paste("!error! : Japanese names do not match at row", row))
        }
        if (inputRow[["alias_name"]] != outputRow[["alias_name"]]) {
            error_f <- TRUE
            print(paste("!error! : Alias names do not match at row", row))
        }
        if (inputRow[["name"]] != outputRow[["name"]]) {
            error_f <- TRUE
            print(paste("!error! : Names do not match at row", row))
        }
        if (inputRow[["label"]] != outputRow[["label"]]) {
            error_f <- TRUE
            print(paste("!error! : Labels do not match at row", row))
        }
        if (is.na(outputRow[["validators.date.validate_date_before_or_equal_to"]])) {
            if (!is.na(inputRow[["date_before"]])) {
                error_f <- TRUE
                print(paste("!error! : date before values do not match at row", row))
            }
        } else if (outputRow[["validators.date.validate_date_before_or_equal_to"]] == "") {
            if (!is.na(inputRow[["date_before"]])) {
                if (inputRow[["date_before"]] != "") {
                    error_f <- TRUE
                    print(paste("!error! : date before values do not match at row", row))
                }
            }
        } else {
            if (inputRow[["date_before"]] != outputRow[["validators.date.validate_date_before_or_equal_to"]]) {
                error_f <- TRUE
                print(paste("!error! : date before or equal to values do not match at row", row))
            }
        }
        if (is.na(outputRow[["validators.date.validate_date_after_or_equal_to"]])) {
            if (!is.na(inputRow[["date_after"]])) {
                error_f <- TRUE
                print(paste("!error! : date after values do not match at row", row))
            }
        } else if (outputRow[["validators.date.validate_date_after_or_equal_to"]] == "") {
            if (!is.na(inputRow[["date_after"]]) && inputRow[["date_after"]] != "") {
                error_f <- TRUE
                print(paste("!error! : date after values do not match at row", row))
            }
        } else {
            if (inputRow[["date_after"]] != outputRow[["validators.date.validate_date_after_or_equal_to"]]) {
                error_f <- TRUE
                print(paste("!error! : date after or equal to values do not match at row", row))
            }
        }
    }
    if (error_f) {
        return(outputValidators)
    } else {
        return(NULL)
    }
}
GetValidatorDates <- function(fieldItem) {
    validators <- fieldItem %>%
        map(~ {
            field_item <- .x
            if (is.null(field_item[["validators"]][["date"]])) {
                return(NA)
            }
            if (length(field_item[["validators"]][["date"]]) == 0) {
                return(NA)
            }
            date_before <- if (!is.null(field_item[["validators"]][["date"]][["validate_date_before_or_equal_to"]]) &&
                !is.na(field_item[["validators"]][["date"]][["validate_date_before_or_equal_to"]])) {
                field_item[["validators"]][["date"]][["validate_date_before_or_equal_to"]]
            } else {
                NA
            }
            date_after <- if (!is.null(field_item[["validators"]][["date"]][["validate_date_after_or_equal_to"]]) &&
                !is.na(field_item[["validators"]][["date"]][["validate_date_after_or_equal_to"]])) {
                field_item[["validators"]][["date"]][["validate_date_after_or_equal_to"]]
            } else {
                NA
            }
            return(list(
                date_before = date_before, date_after = date_after, id = field_item[["id"]],
                sheet_id = field_item[["sheet_id"]], name = field_item[["name"]], label = field_item[["label"]]
            ))
        }) %>%
        keep(~ !(is.atomic(.x) && is.na(.x)))
    return(validators)
}
GetValidatorsDateByTrial <- function(jsonList) {
    jsons <- jsonList
    jpnameAndAliasname <- jsons %>% map_df(~ list(sheet_id = .x[["id"]], jpname = .x[["name"]], alias_name = .x[["alias_name"]]))
    fieldItems <- jsons %>% map(~ .x[["field_items"]])
    df_validators <- fieldItems %>% map_df(~ GetValidatorDates(.x))

    if (nrow(df_validators) == 0) {
        validators <- df_validators
    } else {
        validators <- df_validators %>%
            left_join(jpnameAndAliasname, by = c("sheet_id" = "sheet_id"))
    }
    return(validators)
}
