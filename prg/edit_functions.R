#' Edit the output content for each sheet.
#'
#' @file edit_functions.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ constants ------
source(here("prg", "edit_cdisc_sheet_configs.R"), encoding="UTF-8")
source(here("prg", "edit_field_items.R"), encoding="UTF-8")
# ------ functions ------
#' Edit allocation data from flattened JSON.
#'
#' This function extracts and modifies allocation data from a flattened JSON structure.
#' It renames the column names of a data frame from 'groups' to 'groups.if'.
#'
#' @param flattenJson A list representing flattened JSON data.
#' @return A modified data frame with column names changed and 'groups.if' column added.
#'
#' @examples
#' # Example usage:
#' json_data <- list(allocation = list(groups = data.frame(if = c(1, 2, 3))))
#' edited_data <- EditAllocation(json_data)
#'
#' @importFrom dplyr rename_all cbind
#' @export
EditAllocation <- function(flattenJson){
  allocation <- flattenJson$allocation
  if (is.null(allocation)){
    return(NA)
  }
  kGroupsColname <- "groups"
  # "Rename the column names of a data frame from 'if' to 'groups.if'."
  df_groups <- allocation[[kGroupsColname]] %>% rename_all( ~ str_c(kGroupsColname, ".", .))
  df_others <- allocation %>% RemoveListElements(kGroupsColname)
  res <- df_others %>% cbind(df_groups)
  # Placeholder, adjust as needed
  res$groups.allocatees <- NA
  return(res)
}
