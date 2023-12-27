#' Edit the output content for each sheet.
#'
#' @file edit_cdisc_sheet_configs.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ constants ------
kTableName <- "table"
kCdiscSheetConfigsPivotTableField <- kTableName %>% str_c(".field")
#' Column names to delete from data frames.
kDeleteColnames <- c("uuid", "created_at", "updated_at")
# ------ functions ------
#' Edit the names and remove specified columns from Cdisc Sheet Configs.
#'
#' This function removes specified columns and all columns listed in kDeleteColnames
#' from Cdisc Sheet Configs data frame.
#'
#' @param cdisc_sheet_configs A data frame representing Cdisc Sheet Configs.
#' @return A modified data frame.
#'
#' @importFrom dplyr select
#' @export
EditNameAndCdiscSheetConfigs <- function(cdisc_sheet_configs){
  if (length(cdisc_sheet_configs) == 0){
    return(cdisc_sheet_configs)
  }
  return(cdisc_sheet_configs %>% select(-all_of(kDeleteColnames)))
}
#' Edit Cdisc Sheet Configs data.
#'
#' This function edits the Cdisc Sheet Configs data, removing specified columns.
#'
#' @param trial_data A list representing trial data.
#' @return A list with edited Cdisc Sheet Configs data.
#'
#' @importFrom dplyr select
#' @export
EditCdiscSheetConfigs <- function(trial_data){
  cdisc_sheet_configs <- trial_data$flattenJson$cdisc_sheet_configs
  if (length(cdisc_sheet_configs) == 0){
    return(list(cdisc_sheet_configs=NA, cdisc_sheet_configs_pivots=NA))
  }
  cdisc_sheet_configs <- cdisc_sheet_configs %>% EditNameAndCdiscSheetConfigs()
  cdisc_sheet_configs_pivots <- trial_data %>% EditCdiscSheetConfigsPivots() %>% EditNameAndCdiscSheetConfigs()
  return(list(cdisc_sheet_configs=cdisc_sheet_configs, cdisc_sheet_configs_pivots=cdisc_sheet_configs_pivots))
}
#' Edit Cdisc Sheet Configs Pivots data.
#'
#' This function edits the Cdisc Sheet Configs Pivots data, removing specified columns.
#'
#' @param trial_data A list representing trial data.
#' @return A data frame with edited Cdisc Sheet Configs Pivots data.
#'
#' @importFrom dplyr left_join
#' @export
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
#' Edit Cdisc Sheet Configs Pivots Table data.
#'
#' This function edits the Cdisc Sheet Configs Pivots Table data, generating new columns.
#'
#' @param cdisc_sheet_config A data frame representing Cdisc Sheet Configs Pivots Table.
#' @return A tibble with edited Cdisc Sheet Configs Pivots Table data.
#'
#' @importFrom purrr imap_dfr
#' @importFrom tibble tibble
#' @export
EditCdiscSheetConfigsPivotsTable <- function(cdisc_sheet_config){
  return(imap_dfr(cdisc_sheet_config, ~ tibble(table.field=str_c(kTableName, ".", .y), table.field.value=.x)))
}
