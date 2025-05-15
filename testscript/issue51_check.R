rm(list = ls())
library(tidyverse)
library(here)
library(openxlsx)
source(here("tools", "excel_json_validator", "functions", "excel_json_validator_common.R"), encoding = "UTF-8")
kTrialNames <- c("gpower", "bev", "allb19", "tran", "allr23", "blin_b_all")
# functions
GetPresenceSheet <- function(wbPath) {
    presenceSheet <- wbPath %>%
        openxlsx::read.xlsx(sheet = "presence")
    return(presenceSheet)
}
GetInputCsv <- function(inputPath, csvName) {
    filePath <- file.path(inputPath, csvName)
    if (file.info(filePath)$size == 0) {
        return(NA)
    }
    return(inputPath %>%
        file.path(csvName) %>%
        read.csv())
}
GetOutputFiles <- function(outputPath) {
    checkList <- outputPath %>%
        here("list") %>%
        list.files(full.names = T) %>%
        .[1]
    outputPresence <- checkList %>% GetPresenceSheet()
    return(outputPresence)
}
CheckPresence <- function(inputFile, outputFile) {
    error_flag <- FALSE
    if (nrow(inputFile) == 0) {
        print("No items to check")
        if (nrow(outputFile) != 0) {
            error_flag <- TRUE
            print("Output file is not empty")
            return(error_flag)
        }
    }
    if (nrow(inputFile) != nrow(outputFile)) {
        error_flag <- TRUE
        print("Number of rows in input and output files do not match")
        return(error_flag)
    }
    for (row in 1:nrow(inputFile)) {
        inputRow <- inputFile[row, ]
        outputRow <- outputFile[row, ]
        if (inputRow$jpname != outputRow$シート名) {
            error_flag <- TRUE
            print(str_c("Japanese name does not match: ", row))
            return(list(inputRow = inputRow, outputRow = outputRow))
        }
        if (inputRow$alias_name != outputRow$シート名英数字別名) {
            error_flag <- TRUE
            print(str_c("Alias name does not match: ", row))
            return(list(inputRow = inputRow, outputRow = outputRow))
        }
        if (inputRow$name != outputRow$フィールドID) {
            error_flag <- TRUE
            print(str_c("Field name does not match: ", row))
            return(list(inputRow = inputRow, outputRow = outputRow))
        }
        if (inputRow$label != outputRow$ラベル) {
            error_flag <- TRUE
            print(str_c("Label does not match: ", row))
            return(list(inputRow = inputRow, outputRow = outputRow))
        }
    }
    return(error_flag)
}

# main
inputPath <- "C:\\Users\\MarikoOhtsuka\\Downloads\\20250515_163221\\"
for (targetTrial in kTrialNames) {
    print(targetTrial)
    outputPath <- GetTargetFolder(targetTrial)
    trialInputPath <- file.path(inputPath, targetTrial)
    outputFile <- here("output", outputPath) %>% GetOutputFiles()
    inputFile <- trialInputPath %>% GetInputCsv("presence.csv")
    check_f <- CheckPresence(inputFile, outputFile)
    if (check_f) {
        stop("Check failed")
    } else {
        print("Check passed")
    }
}
