rm(list = ls())
library(tidyverse)
library(here)
library(openxlsx)
gpowerInputPath <- "C:\\Users\\MarikoOhtsuka\\Downloads\\20250512_141825\\gpower"
gpowerOutputPath <- here("output\\output_20250512104938\\list") %>%
    list.files(full.names = T) %>%
    .[1]
gpowerOutput <- gpowerOutputPath %>% openxlsx::read.xlsx(sheet = "limitation")
View(gpowerOutput)
gpowerInputNormalRanges <- gpowerInputPath %>%
    file.path("normal_ranges.csv") %>%
    read.csv()
gpowerInputValidators <- gpowerInputPath %>%
    file.path("validators.csv") %>%
    read.csv()
View(gpowerInputNormalRanges)
View(gpowerInputValidators)
