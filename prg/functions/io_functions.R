#' file input-output
#'
#' @file io_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.1.15
# ------ constants ------
kTableStyle <- "TableStyleMedium2"
# ------ functions ------
#' Read JSON files and return a list of raw and flatten JSON data.
#'
#' This function reads JSON files and returns a list of raw and flatten JSON data.
#'
#' @param json_filenames Vector of JSON file names.
#' @param targetTrialFolder The path of the folder containing the JSON file.
#' @return A list containing raw and flatten JSON data.
#'
#' @importFrom jsonlite fromJSON
#' @export
ReadJsonFiles <- function(json_filenames, targetTrialFolder) {
  json_files <- json_filenames %>% map(~ {
    rawJson <- file.path(targetTrialFolder, .) %>% read_json()
    flattenJson <- file.path(targetTrialFolder, .) %>% fromJSON(flatten = T)
    return(list(rawJson = rawJson, flattenJson = flattenJson))
  })
  names(json_files) <- json_filenames %>% str_remove(., ".json")
  return(json_files)
}

WriteExcel <- function(output_list, filename, output_path) {
  future({
    wb <- createWorkbook()
    for (i in 1:length(output_list)) {
      df_output <- output_list[[i]]
      sheet_name <- names(output_list)[i]
      addWorksheet(wb = wb, sheet = sheet_name)
      writeDataTable(
        wb = wb, sheet = sheet_name, x = df_output,
        startRow = 1, startCol = 1, colNames = T, rowNames = F, withFilter = T,
        tableStyle = kTableStyle, keepNA = F
      )
      setColWidths(wb = wb, sheet = sheet_name, cols = 1:ncol(df_output), widths = "auto")
      fontStyle <- setFontStyle()
      addStyle(wb = wb, sheet = sheet_name, style = fontStyle, rows = 1:(nrow(df_output) + 1), cols = 1:ncol(df_output), gridExpand = TRUE)
      # Reset the width of the ID column.
      id_index <- which(colnames(df_output) == "id")
      removeColWidths(wb = wb, sheet = sheet_name, cols = id_index)
    }
    saveWorkbook(wb = wb, file = str_c(output_path, "/", filename, ".xlsx"), overwrite = T)
  })
}
