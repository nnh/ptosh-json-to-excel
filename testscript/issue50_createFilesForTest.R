# JSONファイルから出力結果チェック用のCSVファイルを作成するスクリプト
# テスト用ファイルはダウンロードフォルダに出力されます

rm(list = ls())
library(tidyverse)
library(jsonlite)
library(here)
# functions
GetDownloadsPath <- function() {
    downloadsPath <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
    return(normalizePath(downloadsPath, winslash = "/"))
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
    colnames(df_normalRanges) %>% print()
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
GetNormalRangeAndValidatorsByTrial <- function(fortestDir) {
    trialName <- fortestDir %>% str_remove("forTest_input_")
    trialDir <- file.path(outputDir, trialName)
    if (!dir.exists(trialDir)) {
        dir.create(trialDir, recursive = TRUE)
    }
    jsons <- list.files(here(fortestDir), pattern = "\\.json$", full.names = TRUE) %>% map(
        ~ {
            json_file <- .x
            json_file %>%
                read_json()
        }
    )
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
    write_csv(normalRanges, file.path(trialDir, "normal_ranges.csv"))
    write_csv(validators, file.path(trialDir, "validators.csv"))
}
# main
outputPath <- GetDownloadsPath()
fortestDirs <- here() %>%
    list.dirs(recursive = FALSE, full.names = FALSE) %>%
    keep(~ str_starts(., "forTest_"))
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
outputDir <- file.path(outputPath, timestamp)
dir.create(outputDir, showWarnings = FALSE)
for (fortestDir in fortestDirs) {
    GetNormalRangeAndValidatorsByTrial(fortestDir)
}
