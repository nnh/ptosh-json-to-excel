#' Edit the output content for each sheet.
#'
#' @file edit_field_items.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ functions ------
#' Edit options from raw and flatten JSON data.
#'
#' This function edits options data from raw and flatten JSON data.
#'
#' @param raw_json Raw JSON data.
#' @param flatten_json Flatten JSON data.
#' @return A data frame with edited options data.
#'
#' @importFrom dplyr select filter is.na left_join
#' @importFrom purrr map2 names na.omit
#' @importFrom stringr str_c str_replace_all
#' @export
EditOptions <- function(raw_json, flatten_json){
  kOption <- "option"
  kOptionValues <- kOption %>% str_c(".values")
  keyX <- kOption %>% str_c("_id")
  keyY <- kOptionValues %>% str_c(keyX, sep="_")
  checkExistOption <- raw_json$field_items %>% some( ~ has_element(names(.), kOption))
  if (!checkExistOption){
    return(NA)
  }

  input_options <- flatten_json$field_items %>% select(starts_with(kOption)) %>% filter(!is.na(option.id))
  option_values <- input_options[[kOptionValues]] %>% list_rbind() %>%
    rename_with( ~ str_c(kOptionValues, "_", .), everything()) %>% distinct()
  option_others <- input_options %>% select(-all_of(kOptionValues)) %>% distinct()
  output_options <- option_others %>% left_join(option_values, by=setNames(keyY, keyX))
  output_options[ , keyY] <- output_options[ , keyX]
  return(output_options)
}
#' Remove and convert list objects to data frame.
#'
#' This function removes specified columns from a list object and converts it to a data frame.
#'
#' @param field_item A list representing field item data.
#' @return A data frame with specified columns removed.
#'
#' @importFrom purrr map2 names tibble
#' @export
RemoveAndConvertToDataframe <- function(field_item){
  delete_target <- map2(field_item, names(field_item), ~ ifelse(is.list(.x), .y, NA)) %>% unlist() %>% na.omit()
  output_field_item <- RemoveListElements(field_item, delete_target)
  df_field_item <- output_field_item %>% map(~ ifelse(is.null(.), NA, .)) %>% tibble(!!!.)
  return(df_field_item)
}
#' Bind list objects to a data frame.
#'
#' This function binds specified list objects to a data frame.
#'
#' @param output_field_item A data frame representing field item data.
#' @param field_item_flatten A data frame representing flattened field item data.
#' @param target_colname A character vector specifying the target column names to bind.
#' @return A data frame with specified list objects bound.
#'
#' @importFrom dplyr select cbind
#' @export
BindListObjects <- function(output_field_item, field_item_flatten, target_colname){
  temp <- field_item_flatten %>% select(starts_with(target_colname))
  output_field_item <- output_field_item %>% cbind(temp)
  return(output_field_item)
}
#' Edit flip flops data from raw JSON.
#'
#' This function edits flip flops data from raw JSON data.
#'
#' @param raw_json Raw JSON data.
#' @return A data frame with edited flip flops data.
#'
#' @importFrom purrr map some
#' @export
EditFlipFlops <- function(raw_json){
  res <- raw_json$field_items %>% map( ~ {
    flip_flops <- .$flip_flops
    if (length(flip_flops) == 0){
      return(NULL)
    }
    codesAndfields <- flip_flops %>% map( ~ {
      flip_flop <- .
      codes <- flip_flop$codes %>% list_simplify() %>% data.frame(codes=.)
      fields <- flip_flop$fields %>% list_simplify() %>% data.frame(fields=.)
      flip_flops_others <- flip_flop %>% RemoveListElements(c("codes", "fields")) %>% data.frame()
      res <- codes %>% cbind(fields, flip_flops_others)
      return(res)
    })
    return(flip_flops)
  }) %>% bind_rows()
  if (nrow(res) == 0){
    return(NA)
  }
  # Placeholder, adjust as needed
  res$codes <- NA
  return(res)
}
#' Edit output field items data from raw and flatten JSON data.
#'
#' This function edits output field items data from raw and flatten JSON data.
#'
#' @param raw_json Raw JSON data.
#' @param flatten_json Flatten JSON data.
#' @return A list with edited output field items data.
#'
#' @importFrom dplyr select starts_with colnames is.null
#' @export
EditOutputFieldItems <- function(raw_json, flatten_json){
  field_items <- raw_json$field_items
  if (length(field_items) == 0){
    return(list(field_items=NA, options=NA, flip_flops=NA))
  }
  field_items_flatten <- flatten_json$field_items
  options <- EditOptions(raw_json, flatten_json)
  flip_flops <- EditFlipFlops(raw_json)

  output_field_items <- NULL
  for (row_count in 1:length(field_items)){
    field_item <- field_items[[row_count]]
    field_item_flatten <- field_items_flatten[row_count, ]
    output_field_item <- field_item %>% RemoveAndConvertToDataframe()
    output_field_item <- output_field_item %>%
      BindListObjects(field_item_flatten, "normal_range") %>%
        BindListObjects(field_item_flatten, "validators")
    output_field_item$option.name <- field_item_flatten$option.name
    output_field_item$validators.presence <- !is.null(field_item$validators$presence)
    output_field_item$flip_flops <- NA
    output_field_items <- output_field_item %>% bind_rows(output_field_items, .)
  }

  if (is.data.frame(options)){
    # Delete unnecessary columns.
    output_field_items <- output_field_items %>% select(-c("option_id"))
    # If validators exist and all of them have no value, delete them.
    if ("validators" %in% colnames(output_field_items) && all(is.na(output_field_items$validators))) {
      output_field_items <- output_field_items %>% select(-validators)
    }
  }
  return(list(options=options, field_items=output_field_items, flip_flops=flip_flops))
}
