rm(list = ls())
library(tidyverse)
library(here)
library(openxlsx)
source(here("tools", "excel_json_validator_common.R"), encoding = "UTF-8")
kTrialNames <- c("gpower", "bev", "allb19", "tran", "allr23", "blin_b_all")
# functions
GetLimitationSheet <- function(wbPath) {
    limitationSheet <- wbPath %>%
        openxlsx::read.xlsx(sheet = "limitation")
    return(limitationSheet)
}
GetInputNormalRanges <- function(inputPath) {
    return(inputPath %>%
        file.path("normal_ranges.csv") %>%
        read.csv())
}
GetInputValidators <- function(inputPath) {
    return(inputPath %>%
        file.path("validators.csv") %>%
        read.csv())
}
GetInputFiles <- function(inputPath) {
    inputNormalRanges <- trialInputPath %>% GetInputNormalRanges()
    inputValidators <- trialInputPath %>% GetInputValidators()
    inputFiles <- list(
        normal_ranges = inputNormalRanges,
        validators = inputValidators
    )
    return(inputFiles)
}
GetOutputFiles <- function(outputPath) {
    checkList <- outputPath %>%
        here("list") %>%
        list.files(full.names = T) %>%
        .[1]
    outputLimitation <- checkList %>% GetLimitationSheet()
    outputNormalRanges <- outputLimitation %>% select(jpname, alias_name, name, label, default_value, normal_range.less_than_or_equal_to, normal_range.greater_than_or_equal_to)
    outputValidators <- outputLimitation %>% select(
        jpname, alias_name, name, label, default_value, validators.numericality.validate_numericality_less_than_or_equal_to,
        validators.numericality.validate_numericality_greater_than_or_equal_to
    )
    return(list(
        normal_ranges = outputNormalRanges,
        validators = outputValidators
    ))
}
CheckNormalRanges <- function(inputNormalRanges, outputNormalRanges) {
    inputNormalRanges <- inputNormalRanges %>% arrange(alias_name, name)
    outputNormalRanges <- outputNormalRanges %>%
        arrange(alias_name, name) %>%
        filter(!is.na(normal_range.greater_than_or_equal_to) | !is.na(normal_range.less_than_or_equal_to))
    if (nrow(inputNormalRanges) != nrow(outputNormalRanges)) {
        stop("Number of rows in input and output normal ranges do not match.")
    }
    nonDefalutValueFlag <- all(is.na(outputNormalRanges$default_value))
    for (row in 1:nrow(inputNormalRanges)) {
        inputRow <- inputNormalRanges[row, ]
        outputRow <- outputNormalRanges[row, ]
        if (inputRow$jpname != outputRow$jpname) {
            stop(paste("Japanese names do not match at row", row))
        }
        if (inputRow$alias_name != outputRow$alias_name) {
            stop(paste("Alias names do not match at row", row))
        }
        if (inputRow$name != outputRow$name) {
            stop(paste("Names do not match at row", row))
        }
        if (inputRow$label != outputRow$label) {
            stop(paste("Labels do not match at row", row))
        }
        if (!nonDefalutValueFlag) {
            if (inputRow$default_value != outputRow$default_value) {
                stop(paste("Default values do not match at row", row))
            }
        }
        if (inputRow$less_than != outputRow$normal_range.less_than_or_equal_to) {
            stop(paste("Less than or equal to values do not match at row", row))
        }
        if (is.na(outputRow$normal_range.greater_than_or_equal_to)) {
            if (!is.na(inputRow$greater_than)) {
                stop(paste("Greater than values do not match at row", row))
            }
        } else if (inputRow$greater_than != outputRow$normal_range.greater_than_or_equal_to) {
            stop(paste("Greater than or equal to values do not match at row", row))
        }
    }
    print("All normal ranges match.")
}
CheckValidators <- function(inputValidators, outputValidators) {
    inputValidators <- inputValidators %>% arrange(alias_name, name)
    outputValidators <- outputValidators %>%
        arrange(alias_name, name) %>%
        filter(!is.na(default_value) | !is.na(validators.numericality.validate_numericality_less_than_or_equal_to) | !is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) | !is.na(validators.numericality.validate_numericality_greater_than_or_equal_to))

    if (nrow(inputValidators) != nrow(outputValidators)) {
        stop("Number of rows in input and output validators do not match.")
    }
    for (row in 1:nrow(inputValidators)) {
        inputRow <- inputValidators[row, ]
        outputRow <- outputValidators[row, ]
        if (inputRow$jpname != outputRow$jpname) {
            stop(paste("Japanese names do not match at row", row))
        }
        if (inputRow$alias_name != outputRow$alias_name) {
            stop(paste("Alias names do not match at row", row))
        }
        if (inputRow$name != outputRow$name) {
            stop(paste("Names do not match at row", row))
        }
        if (inputRow$label != outputRow$label) {
            stop(paste("Labels do not match at row", row))
        }
        if (is.na(outputRow$validators.numericality.validate_numericality_less_than_or_equal_to)) {
            if (!is.na(inputRow$less_than)) {
                stop(paste("Default values do not match at row", row))
            }
        } else if (outputRow$validators.numericality.validate_numericality_less_than_or_equal_to == "") {
            if (!is.na(inputRow$less_than)) {
                stop(paste("Default values do not match at row", row))
            }
        } else {
            if (inputRow$less_than != outputRow$validators.numericality.validate_numericality_less_than_or_equal_to) {
                stop(paste("Less than or equal to values do not match at row", row))
            }
        }
        if (is.na(outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to)) {
            if (!is.na(inputRow$greater_than)) {
                stop(paste("Greater than values do not match at row", row))
            }
        } else if (outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to == "") {
            if (!is.na(inputRow$greater_than)) {
                stop(paste("Greater than values do not match at row", row))
            }
        } else {
            if (inputRow$greater_than != outputRow$validators.numericality.validate_numericality_greater_than_or_equal_to) {
                stop(paste("Greater than or equal to values do not match at row", row))
            }
        }
    }
    print("All validators match.")
}
# main
inputPath <- "C:\\Users\\MarikoOhtsuka\\Downloads\\20250512_165042\\"
for (targetTrial in kTrialNames) {
    print(targetTrial)
    outputPath <- GetTargetFolder(targetTrial)
    trialInputPath <- file.path(inputPath, targetTrial)
    outputFiles <- here("output", outputPath) %>% GetOutputFiles()
    inputFiles <- trialInputPath %>% GetInputFiles()
    CheckNormalRanges(inputFiles$normal_ranges, outputFiles$normal_ranges)
    CheckValidators(inputFiles$validators, outputFiles$validators)
}
