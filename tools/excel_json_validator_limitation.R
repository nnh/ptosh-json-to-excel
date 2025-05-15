#' test script
#'
#' @file excel_json_validator_limitation.R
#' @author Mariko Ohtsuka
#' @date 2025.5.14
CheckLimitation <- function(sheetList, jsonList) {
    sheetName <- "limitation"
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    outputNormalRanges <- sheet %>% select(jpname, alias_name, name, label, default_value, normal_range.less_than_or_equal_to, normal_range.greater_than_or_equal_to)
    outputValidators <- sheet %>% select(
        jpname, alias_name, name, label, default_value, validators.numericality.validate_numericality_less_than_or_equal_to,
        validators.numericality.validate_numericality_greater_than_or_equal_to
    )

    normalRangeAndValidators <- GetNormalRangeAndValidatorsByTrial(jsonList)
    inputNormalRanges <- normalRangeAndValidators$df_normalRanges
    inputValidators <- normalRangeAndValidators$df_validators
    checkNormalRanges <- CheckNormalRanges(inputNormalRanges, outputNormalRanges)
    checkValidators <- CheckValidators(inputValidators, outputValidators)
    if (is.null(checkNormalRanges) && is.null(checkValidators)) {
        res <- NULL
    } else {
        res <- list()
        res$limitation_normalRange <- checkNormalRanges
        res$limitation_validators <- checkValidators
    }
    return(res)
}
CheckNormalRanges <- function(inputNormalRanges, outputNormalRanges) {
    error_f <- FALSE
    if (length(inputNormalRanges) == 1 && is.na(inputNormalRanges)) {
        inputNormalRanges <- data.frame()
    } else if (length(inputNormalRanges) == 0) {
        inputNormalRanges <- data.frame()
    } else {
        inputNormalRanges <- inputNormalRanges %>% arrange(alias_name, name)
        # Add greater_than column if it does not exist
        if (!"greater_than" %in% colnames(inputNormalRanges)) {
            inputNormalRanges$greater_than <- NA
        }
        # Add less_than column if it does not exist
        if (!"less_than" %in% colnames(inputNormalRanges)) {
            inputNormalRanges$less_than <- NA
        }
        inputNormalRanges <- inputNormalRanges %>%
            filter(!is.na(less_than) | !is.na(greater_than)) %>%
            filter(less_than != "" | greater_than != "")
    }
    outputNormalRanges <- outputNormalRanges %>%
        arrange(alias_name, name) %>%
        filter(!is.na(normal_range.greater_than_or_equal_to) | !is.na(normal_range.less_than_or_equal_to)) %>%
        filter(normal_range.greater_than_or_equal_to != "" | normal_range.less_than_or_equal_to != "")

    if (nrow(inputNormalRanges) != nrow(outputNormalRanges)) {
        error_f <- TRUE
        print("!error! : Number of rows in input and output normal ranges do not match.")
    }
    if (nrow(inputNormalRanges) == 0) {
        print("No normal ranges to check.")
        return(NULL)
    }
    nonDefalutValueFlag <- all(is.na(outputNormalRanges$default_value))
    for (row in 1:nrow(inputNormalRanges)) {
        inputRow <- inputNormalRanges[row, ]
        outputRow <- outputNormalRanges[row, ]
        if (inputRow$jpname != outputRow$jpname) {
            error_f <- TRUE
            print(paste("!error! : Japanese names do not match at row", row))
        }
        if (inputRow$alias_name != outputRow$alias_name) {
            error_f <- TRUE
            print(paste("!error! : Alias names do not match at row", row))
        }
        if (inputRow$name != outputRow$name) {
            error_f <- TRUE
            print(paste("!error! : Names do not match at row", row))
        }
        if (inputRow$label != outputRow$label) {
            error_f <- TRUE
            print(paste("!error! : Labels do not match at row", row))
        }
        if (!nonDefalutValueFlag) {
            if (is.na(outputRow$default_value)) {
                if (!is.na(inputRow$default_value) & inputRow$default_value != "") {
                    error_f <- TRUE
                    print(paste("!error! : Default values do not match at row", row))
                }
            } else if (outputRow$default_value == "") {
                if (!is.na(inputRow$default_value) & inputRow$default_value != "") {
                    error_f <- TRUE
                    print(paste("!error! : Default values do not match at row", row))
                }
            } else {
                if (inputRow$default_value != outputRow$default_value) {
                    error_f <- TRUE
                    print(paste("!error! : Default values do not match at row", row))
                }
            }
        }
        if (inputRow$less_than != outputRow$normal_range.less_than_or_equal_to) {
            error_f <- TRUE
            print(paste("!error! : Less than or equal to values do not match at row", row))
        }
        if (is.na(outputRow$normal_range.greater_than_or_equal_to)) {
            if (!is.na(inputRow$greater_than)) {
                error_f <- TRUE
                print(paste("!error! : Greater than values do not match at row", row))
            }
        } else if (inputRow$greater_than != outputRow$normal_range.greater_than_or_equal_to) {
            error_f <- TRUE
            print(paste("!error! : Greater than or equal to values do not match at row", row))
        }
    }
    if (error_f) {
        return(outputNormalRanges)
    } else {
        return(NULL)
    }
}
CheckValidators <- function(inputValidators, outputValidators) {
    error_f <- FALSE
    inputValidators <- inputValidators %>% arrange(alias_name, name)
    inputValidators <- inputValidators %>%
        filter(!is.na(less_than) | !is.na(greater_than)) %>%
        filter(less_than != "" | greater_than != "")
    inputValidators <- inputValidators %>%
        filter(!(is.na(less_than) & (is.na(greater_than) | greater_than == "")))
    outputValidators <- outputValidators %>%
        arrange(alias_name, name) %>%
        filter(!is.na(default_value) | !is.na(validators.numericality.validate_numericality_less_than_or_equal_to) | !is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) | !is.na(validators.numericality.validate_numericality_greater_than_or_equal_to)) %>%
        filter(validators.numericality.validate_numericality_less_than_or_equal_to != "" | validators.numericality.validate_numericality_greater_than_or_equal_to != "")

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
        if (inputRow$jpname != outputRow$jpname) {
            error_f <- TRUE
            print(paste("!error! : Japanese names do not match at row", row))
        }
        if (inputRow$alias_name != outputRow$alias_name) {
            error_f <- TRUE
            print(paste("!error! : Alias names do not match at row", row))
        }
        if (inputRow$name != outputRow$name) {
            error_f <- TRUE
            print(paste("!error! : Names do not match at row", row))
        }
        if (inputRow$label != outputRow$label) {
            error_f <- TRUE
            print(paste("!error! : Labels do not match at row", row))
        }
        if (is.na(outputRow$validators.numericality.validate_numericality_less_than_or_equal_to)) {
            if (!is.na(inputRow$less_than)) {
                error_f <- TRUE
                print(paste("!error! : Less than values do not match at row", row))
            }
        } else if (outputRow$validators.numericality.validate_numericality_less_than_or_equal_to == "") {
            if (!is.na(inputRow$less_than)) {
                if (inputRow$less_than != "") {
                    error_f <- TRUE
                    print(paste("!error! : Less than values do not match at row", row))
                }
            }
        } else {
            if (inputRow$less_than != outputRow$validators.numericality.validate_numericality_less_than_or_equal_to) {
                error_f <- TRUE
                print(paste("!error! : Less than or equal to values do not match at row", row))
            }
        }
        if (is.na(outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to)) {
            if (!is.na(inputRow$greater_than)) {
                error_f <- TRUE
                print(paste("!error! : Greater than values do not match at row", row))
            }
        } else if (outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to == "") {
            if (!is.na(inputRow$greater_than) && inputRow$greater_than != "") {
                error_f <- TRUE
                print(paste("!error! : Greater than values do not match at row", row))
            }
        } else {
            if (inputRow$greater_than != outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to) {
                error_f <- TRUE
                print(paste("!error! : Greater than or equal to values do not match at row", row))
            }
        }
    }
    if (error_f) {
        return(outputValidators)
    } else {
        return(NULL)
    }
}
GetNormalRanges <- function(fieldItem) {
    normalRanges <- fieldItem %>%
        map(~ {
            if (is.null(.x$normal_range)) {
                return(NA)
            }
            if (length(.x$normal_range) == 0) {
                return(NA)
            }
            lessThan <- .x$normal_range$less_than_or_equal_to
            greaterThan <- .x$normal_range$greater_than_or_equal_to
            return(list(default_value = .x$default_value, less_than = lessThan, greater_than = greaterThan, id = .x$id, sheet_id = .x$sheet_id, name = .x$name, label = .x$label))
        }) %>%
        keep(~ !(is.atomic(.x) && is.na(.x)))
    return(normalRanges)
}
GetValidators <- function(fieldItem) {
    validators <- fieldItem %>%
        map(~ {
            if (is.null(.x$validator)) {
                return(NA)
            }
            if (length(.x$validator) == 0) {
                return(NA)
            }
            return(list(validator = .x$validator, id = .x$id, sheet_id = .x$sheet_id, name = .x$name, label = .x$label))
        }) %>%
        keep(~ !(is.atomic(.x) && is.na(.x)))
    return(validators)
}
GetValidatorNumericality <- function(validators) {
    res <- validators %>%
        map(~ {
            if (is.null(.x$validator$numericality)) {
                return(NA)
            }
            if (length(.x$validator$numericality) == 0) {
                return(NA)
            }
            lessThan <- .x$validator$numericality$validate_numericality_less_than_or_equal_to
            greaterThan <- .x$validator$numericality$validate_numericality_greater_than_or_equal_to
            return(list(less_than = lessThan, greater_than = greaterThan, id = .x$id, sheet_id = .x$sheet_id, name = .x$name, label = .x$label))
        }) %>%
        keep(~ !(is.atomic(.x) && is.na(.x)))
    if (length(res) == 0) {
        return(NA)
    }
    return(res)
}
GetNormalRangeAndValidators <- function(fieldItems) {
    normalRanges <- fieldItems %>% map(~ GetNormalRanges(.x))
    df_normalRanges <- data.frame()
    for (i in 1:length(normalRanges)) {
        df <- normalRanges[[i]] %>% bind_rows()
        df_normalRanges <- df_normalRanges %>% bind_rows(df)
    }
    validators <- fieldItems %>% map(~ GetValidators(.x))
    validatorsNumericality <- validators %>% map(~ GetValidatorNumericality(.x))
    df_validators <- data.frame()
    for (i in 1:length(validatorsNumericality)) {
        if (length(validatorsNumericality[[i]]) == 1 && is.na(validatorsNumericality[[i]])) {
            next
        }
        df <- validatorsNumericality[[i]] %>% bind_rows()
        df_validators <- df_validators %>% bind_rows(df)
    }
    return(list(df_normalRanges = df_normalRanges, df_validators = df_validators))
}
GetNormalRangeAndValidatorsByTrial <- function(jsonList) {
    jsons <- jsonList
    jpnameAndAliasname <- jsons %>% map_df(~ list(sheet_id = .x$id, jpname = .x$name, alias_name = .x$alias_name))
    fieldItems <- jsons %>% map(~ .x$field_items)
    res <- GetNormalRangeAndValidators(fieldItems)
    if (nrow(res$df_normalRanges) == 0) {
        normalRanges <- res$df_normalRanges
    } else {
        normalRanges <- res$df_normalRanges %>%
            left_join(jpnameAndAliasname, by = c("sheet_id" = "sheet_id"))
    }
    if (nrow(res$df_validators) == 0) {
        validators <- res$df_validators
    } else {
        validators <- res$df_validators %>%
            left_join(jpnameAndAliasname, by = c("sheet_id" = "sheet_id"))
    }
    return(list(df_normalRanges = normalRanges, df_validators = validators))
}
