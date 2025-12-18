# json_to_excelを実行してから動かす
testinputPath <- "C:\\Users\\MarikoOhtsuka\\Box\\Datacenter\\Users\\ohtsuka\\2025\\20251107"
csv_files <- list.files(testinputPath, pattern = "\\.csv$", full.names = TRUE)
csv_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)
csv_names <- gsub("\\.csv$", "", basename(csv_files)) %>% str_remove("AML224-FLT3-ITD_")
names(csv_list) <- csv_names
### item_visit
testItemVisit <- csv_list$item_visit
outputItemVisit <- output_checklist$item_visit %>% as.data.frame()
# シート名の一致を確認
outputItemVisitLabels <- outputItemVisit %>%
    colnames() %>%
    setdiff("ラベル") %>%
    setdiff("数値チェック・アラート条件の有無") %>%
    sort()
testItemVisitLabels <- testItemVisit$alias_name %>%
    unique() %>%
    sort()
if (!identical(testItemVisitLabels, outputItemVisitLabels)) {
    stop("item_visit sheet column names do not match")
} else {
    cat("item_visit sheet column names match.\n")
}
# ラベルの一致を確認
outputItemVisitRowItems <- outputItemVisit$ラベル %>%
    unique() %>%
    sort()
testItemVisitRowItems <- testItemVisit$label %>%
    unique() %>%
    sort()
diff_labels <- setdiff(testItemVisitRowItems, outputItemVisitRowItems)
if (length(diff_labels) > 0) {
    cat("testItemVisitRowItems にあって outputItemVisitRowItems にない値:\n")
    print(diff_labels)
}
diff_labels2 <- setdiff(outputItemVisitRowItems, testItemVisitRowItems)
if (length(diff_labels2) > 0) {
    cat("outputItemVisitRowItems にあって testItemVisitRowItems にない値:\n")
    print(diff_labels2)
}
if (!identical(testItemVisitRowItems, outputItemVisitRowItems)) {
    if (!identical(length(testItemVisitRowItems), length(outputItemVisitRowItems))) {
        for (i in 1:max(length(testItemVisitRowItems), length(outputItemVisitRowItems))) {
            if (i > length(testItemVisitRowItems)) {
                stop(paste0(" Row ", i, ": testItemVisitRowItem = <none>, outputItemVisitRowItem = ", outputItemVisitRowItems[i]))
            } else if (i > length(outputItemVisitRowItems)) {
                stop(paste0(" Row ", i, ": testItemVisitRowItem = ", testItemVisitRowItems[i], ", outputItemVisitRowItem = <none>"))
            } else if (!identical(testItemVisitRowItems[i], outputItemVisitRowItems[i])) {
                stop(paste0(" Row ", i, ": testItemVisitRowItem = ", testItemVisitRowItems[i], ", outputItemVisitRowItem = ", outputItemVisitRowItems[i]))
            }
        }
    }
} else {
    cat("item_visit sheet row labels match.\n")
}
# ラベル、シート名毎に値を比較
for (row in 1:nrow(testItemVisit)) {
    testRow <- testItemVisit[row, ]
    sheet_name <- testRow$alias_name
    label <- testRow$label
    count <- testRow$count
    flag <- testRow$flag
    target <- outputItemVisit %>%
        filter(ラベル == label) %>%
        select(all_of(sheet_name))
    output_count <- target[1, 1, drop = TRUE]
    if (!identical(as.character(count), as.character(output_count))) {
        stop(paste0(" Row ", row, ": sheet_name = ", sheet_name, ", label = ", label, ", test count = ", count, ", output count = ", output_count))
    }
    target <- outputItemVisit %>%
        filter(ラベル == label) %>%
        select(`数値チェック・アラート条件の有無`)
    output_flag <- target[1, 1, drop = TRUE]
    if (!identical(as.character(flag), as.character(output_flag))) {
        stop(paste0(" Row ", row, ": sheet_name = ", sheet_name, ", label = ", label, ", test flag = ", flag, ", output flag = ", output_flag))
    }
}
cat("item_visit sheet values match.\n")
### item_visit_old
testItemVisit <- csv_list$item_visit_old
outputItemVisit <- output_checklist$item_visit_old %>% as.data.frame()
colnames(testItemVisit) <- outputItemVisit %>% colnames()
if (!identical(testItemVisit, outputItemVisit)) {
    for (col in colnames(testItemVisit)) {
        if (!identical(testItemVisit[[col]], outputItemVisit[[col]])) {
            for (i in 1:nrow(testItemVisit)) {
                if (!identical(testItemVisit[[col]][i], outputItemVisit[[col]][i])) {
                    stop(paste0(" Row ", i, ": testItemVisit = ", testItemVisit[[col]][i], ", outputItemVisit = ", outputItemVisit[[col]][i]))
                }
            }
        }
    }
} else {
    cat("item_visit_old sheet matches.\n")
}
### item
testItem <- csv_list$item
outputItem <- output_checklist$item
colnames(testItem) <- outputItem %>% colnames()
if (!identical(testItem, as.data.frame(outputItem))) {
    if (identical(nrow(testItem), nrow(outputItem)) & identical(ncol(testItem), ncol(outputItem))) {
        for (col in colnames(testItem)) {
            if (!identical(testItem[[col]], outputItem[[col]])) {
                for (i in 1:nrow(testItem)) {
                    if (!identical(testItem[[col]][i], outputItem[[col]][i])) {
                        stop(paste0(" Row ", i, ": testItem = ", testItem[[col]][i], ", outputItem = ", outputItem[[col]][i]))
                    }
                }
            }
        }
    }
    stop("Difference in item sheet\n")
} else {
    cat("Item sheet matches.\n")
}
### visit
testVisit <- csv_list$visit
testVisit$visitnum <- testVisit$visitnum %>% as.numeric()
outputVisit <- output_checklist$visit
colnames(testVisit) <- output_checklist$visit %>% colnames()
if (!identical(testVisit, as.data.frame(output_checklist$visit))) {
    if (identical(nrow(testVisit), nrow(output_checklist$visit)) & identical(ncol(testVisit), ncol(output_checklist$visit))) {
        for (col in colnames(testVisit)) {
            if (!identical(testVisit[[col]], outputVisit[[col]])) {
                for (i in 1:nrow(testVisit)) {
                    if (!identical(testVisit[[col]][i], outputVisit[[col]][i])) {
                        stop(paste0(" Row ", i, ": testVisit = ", testVisit[[col]][i], ", outputVisit = ", outputVisit[[col]][i]))
                    }
                }
            }
        }
    }
    stop("Difference in visit sheet\n")
} else {
    cat("Visit sheet matches.\n")
}
### allocation
testAllocation <- csv_list$allocation
colnames(testAllocation) <- output_checklist$allocation %>% colnames()
testAllocation$割付責任者メールアドレス <- ifelse(is.na(testAllocation$割付責任者メールアドレス), "", as.character(testAllocation$割付責任者メールアドレス))
testAllocation$割付グループ.論理式 <- ifelse(is.na(testAllocation$割付グループ.論理式), "", as.character(testAllocation$割付グループ.論理式))
testAllocation$割付グループ.エラーメッセージ <- ifelse(is.na(testAllocation$割付グループ.エラーメッセージ), "", as.character(testAllocation$割付グループ.エラーメッセージ))
testAllocation$調整因子フィールド.式 <- ifelse(is.na(testAllocation$調整因子フィールド.式), "", as.character(testAllocation$調整因子フィールド.式))
testAllocation$調整因子フィールド.式の参照先情報 <- ifelse(is.na(testAllocation$調整因子フィールド.式の参照先情報), "", as.character(testAllocation$調整因子フィールド.式の参照先情報))
if (!identical(testAllocation, as.data.frame(output_checklist$allocation))) {
    stop("Difference in allocation sheet\n")
    identical(nrow(testAllocation), nrow(output_checklist$allocation))
    identical(ncol(testAllocation), ncol(output_checklist$allocation))
    for (col in colnames(testAllocation)) {
        if (!identical(testAllocation[[col]], output_checklist$allocation[[col]])) {
            stop("Difference in allocation column:", col, "\n")
        }
    }
} else {
    cat("Allocation sheet matches.\n")
}
### limitation
testLimitation <- csv_list$limitation
# とりあえずデフォルト値をテストデータに入れておく
warning(("Todo:limitationシートのデフォルト値のカラムが必要か確認する"))
limitation_cols_1_to_4 <- testLimitation[, 1:4]
limitation_cols_5_onwards <- testLimitation[, 5:ncol(testLimitation)]
testLimitation <- cbind(limitation_cols_1_to_4, temp = NA, limitation_cols_5_onwards)
testLimitation[1, "temp"] <- ""
colnames(testLimitation) <- output_checklist$limitation %>% colnames()
testLimitation$バリデータ.数値.最大値 <- testLimitation$バリデータ.数値.最大値 %>% as.character()
testLimitation$バリデータ.数値.最小値 <- testLimitation$バリデータ.数値.最小値 %>% as.character()
if (!identical(testLimitation, as.data.frame(output_checklist$limitation))) {
    if (!identical(nrow(testLimitation), nrow(output_checklist$limitation)) | !identical(ncol(testLimitation), ncol(output_checklist$limitation))) {
        stop("Difference in limitation sheet\n")
    }
    for (col in colnames(testLimitation)) {
        if (!identical(testLimitation[[col]], output_checklist$limitation[[col]])) {
            stop("Difference in limitation column:", col, "\n")
        }
    }
} else {
    cat("Limitation sheet matches.\n")
}
### date
testDate <- csv_list$date
colnames(testDate) <- output_checklist$date %>% colnames()
check_date <- output_checklist$date
check_date$日付の最小値 <- ifelse(is.na(check_date$日付の最小値), "", as.character(check_date$日付の最小値))
if (!identical(testDate, as.data.frame(check_date))) {
    if (identical(nrow(testDate), nrow(check_date)) & identical(ncol(testDate), ncol(check_date))) {
        for (col in colnames(testDate)) {
            if (!identical(testDate[[col]], check_date[[col]])) {
                for (i in 1:nrow(testDate)) {
                    if (!identical(testDate[[col]][i], check_date[[col]][i])) {
                        stop(paste0(" Row ", i, ": testDate = ", testDate[[col]][i], ", outputDate = ", check_date[[col]][i]))
                    }
                }
            }
        }
    } else {
        stop("Difference in date sheet dimensions\n")
    }
} else {
    cat("Date sheet matches.\n")
}
### option
testOption <- csv_list$option
colnames(testOption) <- output_checklist$option %>% colnames()
testOption <- testOption %>% arrange(シート名, シート名英数字別名, オプション名, ラベル, `-`)
testOption$コード <- ifelse(is.na(testOption$コード), "NA", as.character(testOption$コード))
outputOption <- output_checklist$option %>%
    arrange(シート名, シート名英数字別名, オプション名, ラベル, `-`) %>%
    as.data.frame()
if (identical(testOption, outputOption)) {
    cat("Option sheet matches.\n")
} else {
    if (identical(nrow(testOption), nrow(outputOption)) & identical(ncol(testOption), ncol(outputOption))) {
        for (col in colnames(testOption)) {
            if (!identical(testOption[[col]], outputOption[[col]])) {
                for (i in 1:nrow(testOption)) {
                    if (!identical(testOption[[col]][i], outputOption[[col]][i])) {
                        stop(paste0(" Row ", i, ": testOption = ", testOption[[col]][i], ", outputOption = ", outputOption[[col]][i]))
                    }
                }
            }
        }
    } else {
        stop("Difference in option sheet dimensions\n")
    }
}

### name
if (!identical(csv_list$name, as.data.frame(output_checklist$name))) {
    str(csv_list$name)
    str(output_checklist$name)
    stop("Difference in name sheet\n")
} else {
    cat("name sheet matches.\n")
}
### master
testMaster <- csv_list$master
colnames(testMaster) <- output_checklist$master %>% colnames()
if (!identical(testMaster, as.data.frame(output_checklist$master))) {
    identical(nrow(testMaster), nrow(output_checklist$master))
    identical(ncol(testMaster), ncol(output_checklist$master))
    stop("Difference in master sheet\n")
} else {
    cat("Master sheet matches.\n")
}
### display
testDisplay <- csv_list$display %>%
    select(-field_item_type) %>%
    select(-is_invisible)
colnames(testDisplay) <- output_checklist$display %>% colnames()
if (!identical(testDisplay, as.data.frame(output_checklist$display))) {
    identical(nrow(testDisplay), nrow(output_checklist$display))
    identical(ncol(testDisplay), ncol(output_checklist$display))
    stop("Difference in display sheet\n")
} else {
    cat("Display sheet matches.\n")
}
### comment
testComment <- csv_list$comment
colnames(testComment) <- output_checklist$comment %>% colnames()
testComment$ラベル <- ifelse(is.na(testComment$ラベル), "", as.character(testComment$ラベル))
if (!identical(testComment, output_checklist$comment %>% as.data.frame())) {
    identical(nrow(testComment), nrow(output_checklist$comment))
    identical(ncol(testComment), ncol(output_checklist$comment))
    stop("Difference in comment sheet\n")
} else {
    cat("Comment sheet matches.\n")
}
### explanation
testExplanation <- csv_list$explanation
colnames(testExplanation) <- output_checklist$explanation %>% colnames()
if (!identical(testExplanation, output_checklist$explanation %>% as.data.frame())) {
    identical(nrow(testExplanation), nrow(output_checklist$explanation))
    identical(ncol(testExplanation), ncol(output_checklist$explanation))
    stop("Difference in explanation sheet\n")
} else {
    cat("Explanation sheet matches.\n")
}
### presence
testPresence <- csv_list$presence
colnames(testPresence) <- output_checklist$presence %>% colnames()
testPresence <- testPresence %>%
    arrange(シート名, シート名英数字別名, フィールドID, ラベル)
outputPresence <- output_checklist$presence %>%
    as.data.frame() %>%
    arrange(シート名, シート名英数字別名, フィールドID, ラベル)
if (!identical(testPresence, outputPresence)) {
    for (col in colnames(testPresence)) {
        if (!identical(testPresence[[col]], outputPresence[[col]])) {
            for (i in 1:nrow(testPresence)) {
                if (!identical(testPresence[[col]][i], outputPresence[[col]][i])) {
                    stop(paste0(" Row ", i, ": testPresence = ", testPresence[[col]][i], ", outputPresence = ", outputPresence[[col]][i]))
                }
            }
        }
    }
} else {
    cat("Presence sheet matches.\n")
}
### title
testTitle <- csv_list$title
outputTitle <- output_checklist$title
# 見出しは無視
warning("Todo:titleシートの見出しが必要か確認する")
outputTitle$見出し <- NULL
colnames(testTitle) <- outputTitle %>% colnames()
if (!identical(testTitle, as.data.frame(outputTitle))) {
    identical(nrow(testTitle), nrow(outputTitle))
    identical(ncol(testTitle), ncol(outputTitle))
    for (col in colnames(testTitle)) {
        if (!identical(testTitle[[col]], outputTitle[[col]])) {
            stop("Difference in title column:", col, "\n")
        }
    }
    stop("Difference in title sheet\n")
} else {
    cat("Title sheet matches.\n")
}
### assigned
testAssigned <- csv_list$assigned
outputAssigned <- output_checklist$assigned
colnames(testAssigned) <- outputAssigned %>% colnames()
if (!identical(testAssigned, as.data.frame(outputAssigned))) {
    identical(nrow(testAssigned), nrow(outputAssigned))
    identical(ncol(testAssigned), ncol(outputAssigned))
    for (col in colnames(testAssigned)) {
        if (!identical(testAssigned[[col]], outputAssigned[[col]])) {
            stop("Difference in assigned column:", col, "\n")
        }
    }
    stop("Difference in assigned sheet\n")
} else {
    cat("Assigned sheet matches.\n")
}

### action
testAction <- csv_list$action
outputAction <- output_checklist$action
colnames(testAction) <- outputAction %>% colnames()
testAction <- testAction %>% arrange(
    シート名, シート名英数字別名, 開閉のトリガーになるフィールドID,
    開閉のトリガーになるラベル, 開閉するフィールドID,
    開閉するラベル
)
outputAction <- outputAction %>% arrange(
    シート名, シート名英数字別名, 開閉のトリガーになるフィールドID,
    開閉のトリガーになるラベル, 開閉するフィールドID,
    開閉するラベル
)
if (!identical(testAction, as.data.frame(output_checklist$action))) {
    identical(nrow(testAction), nrow(output_checklist$action))
    identical(ncol(testAction), ncol(output_checklist$action))
    for (col in colnames(testAction)) {
        if (!identical(testAction[[col]], output_checklist$action[[col]])) {
            stop("Difference in action column:", col, "\n")
        }
    }
    stop("Difference in action sheet\n")
} else {
    cat("Action sheet matches.\n")
}
