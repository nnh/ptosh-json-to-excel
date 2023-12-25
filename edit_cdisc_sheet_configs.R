#' Edit the output content for each sheet.
#'
#' @file edit_cdisc_sheet_configs.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ constants ------
kTableName <- "table"
kCdiscSheetConfigsPivotTableField <- kTableName %>% str_c(".field")
kDeleteColnames <- c("uuid", "created_at", "updated_at")
# ------ functions ------
EditNameAndCdiscSheetConfigs <- function(cdisc_sheet_configs){
  if (length(cdisc_sheet_configs) == 0){
    return(cdisc_sheet_configs)
  }
  return(cdisc_sheet_configs %>% select(-all_of(kDeleteColnames)))
}

EditCdiscSheetConfigs <- function(trial_data){
  cdisc_sheet_configs <- trial_data$flattenJson$cdisc_sheet_configs
  if (length(cdisc_sheet_configs) == 0){
    return(list(cdisc_sheet_configs=NA, cdisc_sheet_configs_pivots=NA))
  }
  cdisc_sheet_configs <- cdisc_sheet_configs %>% EditNameAndCdiscSheetConfigs()
  cdisc_sheet_configs_pivots <- trial_data %>% EditCdiscSheetConfigsPivots() %>% EditNameAndCdiscSheetConfigs()
  return(list(cdisc_sheet_configs=cdisc_sheet_configs, cdisc_sheet_configs_pivots=cdisc_sheet_configs_pivots))
}

EditCdiscSheetConfigsPivots <- function(trial_data){
  forGrepCdiscSheetConfigsPivotTableField <- kCdiscSheetConfigsPivotTableField %>% str_replace_all("\\.", "\\\\.") %>%
    str_c("^", ., "\\d+$")
  output_field_name_list <- trial_data$flattenJson$cdisc_sheet_configs %>% names() %>%
    grep(forGrepCdiscSheetConfigsPivotTableField, ., value=T) %>%
    data.frame(temp=.) %>% rename(!!kCdiscSheetConfigsPivotTableField:=temp)
  cdisc_sheet_configs <- trial_data$rawJson$cdisc_sheet_configs %>% map( ~ {
    cdisc_sheet_config <- .
    df_tables <- EditCdiscSheetConfigsPivotsTable(cdisc_sheet_config[[kTableName]]) %>%
      left_join(output_field_name_list, ., by=kCdiscSheetConfigsPivotTableField)
    df_others <- cdisc_sheet_config %>% RemoveListElements(kTableName)
    res <- df_tables %>% cbind(df_others)
    return(res)
  }) %>% bind_rows()
  return(cdisc_sheet_configs)
}
EditCdiscSheetConfigsPivotsTable <- function(cdisc_sheet_config){
  return(imap_dfr(cdisc_sheet_config, ~ tibble(table.field=str_c(kTableName, ".", .y), table.field.value=.x)))
}
