#' edit_title.R
#'
#' @file edit_title.R
#' @author Mariko Ohtsuka
#' @date 2025.11.7
EditTitle <- function(input_field_items, sheet) {
    field_items <- input_field_items %>% GetTargetByType("FieldItem::Heading")
    title <- field_items %>% map_df(~ {
        res <- tibble::tibble(
            name = .x[["name"]],
            label = .x[["label"]],
            level = .x[["level"]] %||% NA,
        )
        return(res)
    })
  res <- JoinJpnameAndAliasNameAndSelectColumns("title", sheet)
  return(res)
}
