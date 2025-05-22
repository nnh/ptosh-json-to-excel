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
CreateExcludeTargetsCsv <- function(fortestDir) {
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
    articles <- fieldItems %>%
        map(~ keep(.x, ~ .x$type == "FieldItem::Article"))
    no_validators_presence <- articles %>%
        list_c() %>%
        keep(~ is.null(.x$validators$presence))
    df_no_validators_presence <- no_validators_presence %>%
        map_df(~ {
            sheet_id <- .x$sheet_id
            name <- .x$name
            label <- .x$label
            return(data.frame(sheet_id = sheet_id, name = name, label = label))
        })
    df_no_validators_presence_jpname_aliasname <- df_no_validators_presence %>%
        inner_join(jpnameAndAliasname, by = c("sheet_id" = "sheet_id")) %>%
        arrange(alias_name, sheet_id)
    cdiscSheetConfigs <- jsons %>%
        map(~ .x$cdisc_sheet_config)
    excludeTargetsStat <- cdiscSheetConfigs %>%
        list_c() %>%
        map_df(~ {
            prefix <- .x$prefix
            sheet_id <- .x$sheet_id
            table <- .x$table
            fields <- NULL
            for (i in seq_along(table)) {
                if (is.null(table[[i]])) {
                    next
                }
                if (table[[i]] != "STAT") {
                    next
                }
                fields <- list(name = names(table)[[i]], value = table[[i]])
            }
            res <- fields %>% map_df(~.x)
            res$prefix <- prefix
            res$sheet_id <- sheet_id
            return(res)
        })

    excludeTargetsIe <- cdiscSheetConfigs %>%
        map(~ keep(.x, ~ .x$prefix == "IE")) %>%
        keep(~ length(.x) > 0) %>%
        list_c()
    excludeTargetsIeorres <- excludeTargetsIe %>%
        map_df(~ {
            prefix <- .x$prefix
            sheet_id <- .x$sheet_id
            table <- .x$table
            fields <- NULL
            for (i in seq_along(table)) {
                if (is.null(table[[i]])) {
                    next
                }
                if (table[[i]] != "ORRES") {
                    next
                }
                fields <- list(name = names(table)[[i]], value = table[[i]])
            }
            res <- fields %>% map_df(~.x)
            res$prefix <- prefix
            res$sheet_id <- sheet_id
            return(res)
        })
    excludeTargets <- excludeTargetsIeorres %>%
        bind_rows(excludeTargetsStat) %>%
        inner_join(jpnameAndAliasname, by = c("sheet_id" = "sheet_id")) %>%
        arrange(alias_name, sheet_id)
    presenceExcludeTargets <- df_no_validators_presence_jpname_aliasname %>% anti_join(excludeTargets, by = c("sheet_id" = "sheet_id", "name" = "name"))
    write_csv(presenceExcludeTargets, file.path(trialDir, "presence.csv"))
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
    CreateExcludeTargetsCsv(fortestDir)
}
