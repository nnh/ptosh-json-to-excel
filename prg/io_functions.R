#' file input-output
#'
#' @file io_functions.R
#' @author Mariko Ohtsuka
#' @date 2023.12.27
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
#' Read JSON files and return a list of raw and flatten JSON data.
#'
#' This function reads JSON files and returns a list of raw and flatten JSON data.
#'
#' @param json_filenames Vector of JSON file names.
#' @param input_folder_name Name of the folder containing the JSON file.
#' @return A list containing raw and flatten JSON data.
#'
#' @importFrom jsonlite fromJSON
#' @export
ReadJsonFiles <- function(json_filenames, input_folder_name){
  json_files <- json_filenames %>% map( ~ {
    rawJson <- here(input_folder_name, .) %>% read_json()
    flattenJson <- here(input_folder_name, .) %>% fromJSON(flatten=T)
    return(list(rawJson=rawJson, flattenJson=flattenJson))
  })
  names(json_files) <- json_filenames %>% str_remove(., ".json")
  return(json_files)
}
#' Write data to Excel file.
#'
#' This function writes the provided data to an Excel file.
#'
#' @param output_list List containing various data frames.
#' @param filename Name of the Excel file.
#' @param output_path Path where the Excel file will be saved.
#' @param outputListIndex List of indexes of the data frame to be output.
#'
#' @seealso common_functions.R/EditDfJpNameAndAliasName
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable setColWidths
#' @export
WriteExcel <- function(output_list, filename, output_path, outputListIndex){
  df_jpname_aliasname <- EditDfJpNameAndAliasName(output_list[[outputListIndex$raw_json]])
  # If the data does not exist, it is not output to Excel.
  write_target_data <- output_list
  # Input source JSON data need not be output.
  write_target_data[[outputListIndex$raw_json]] <- NA
  write_target_index <- which(!is.na(write_target_data))
  # Reorder the columns.
  for (i in 1:length(write_target_data)){
    if (is.data.frame(write_target_data[[i]])){
      write_target_data[[i]] <- write_target_data[[i]] %>% cbind(df_jpname_aliasname, .)
    }
  }
  if (!outputListIndex$cdisc_sheet_configs %in% write_target_index){
    write_target_data[[outputListIndex$cdisc_sheet_configs]] <- df_jpname_aliasname[-1, ]
    write_target_index <- write_target_index %>% c(outputListIndex$cdisc_sheet_configs)
  }
  wb <- createWorkbook()
  for (i in 1:length(write_target_index)){
    sheet_name <- names(write_target_data)[write_target_index[i]]
    df_output <- write_target_data[[write_target_index[i]]]
    addWorksheet(wb=wb, sheet=sheet_name)
    writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                   startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                   tableStyle=kTableStyle, keepNA=F)
    setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
    # Reset the width of the ID column.
    id_index <- which(colnames(df_output) == "id")
    removeColWidths(wb=wb, sheet=sheet_name, cols=id_index)
  }
  saveWorkbook(wb=wb, file=str_c(output_path, "/", filename, ".xlsx"), overwrite=T)
}
