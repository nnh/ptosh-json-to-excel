#' Edit the output content for each sheet.
#' 
#' @file edit_field_items.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
# ------ functions ------
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

RemoveAndConvertToDataframe <- function(field_item){
  # リストオブジェクトを削除するため、削除対象の列名を取得する。
  delete_target <- map2(field_item, names(field_item), ~ ifelse(is.list(.x), .y, NA)) %>% unlist() %>% na.omit()
  # 不要なリストオブジェクトを削除する。
  output_field_item <- RemoveListElements(field_item, delete_target)
  # リストオブジェクトをデータフレームに変換する。
  df_field_item <- output_field_item %>% map(~ ifelse(is.null(.), NA, .)) %>% tibble(!!!.)
  return(df_field_item)
}

BindListObjects <- function(output_field_item, field_item_flatten, target_colname){
  temp <- field_item_flatten %>% select(starts_with(target_colname))
  output_field_item <- output_field_item %>% cbind(temp)
  return(output_field_item)
}

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
  # あとで直す
  res$codes <- NA
  return(res)
}

EditOutputFieldItems <- function(raw_json, flatten_json){
  field_items <- raw_json$field_items
  if (length(field_items) == 0){
    return(list(field_items=NA, options=NA))
  }
  field_items_flatten <- flatten_json$field_items
  options <- EditOptions(raw_json, flatten_json)
  flip_flops <- EditFlipFlops(raw_json)

  output_field_items <- NULL
  for (row_count in 1:length(field_items)){
    field_item <- field_items[[row_count]]
    field_item_flatten <- field_items_flatten[row_count, ]
    output_field_item <- field_item %>% RemoveAndConvertToDataframe()
    # リストオブジェクトをバインド
    output_field_item <- output_field_item %>%
      BindListObjects(field_item_flatten, "normal_range") %>%
        BindListObjects(field_item_flatten, "validators")
    output_field_item$option.name <- field_item_flatten$option.name
    output_field_item$validators.presence <- !is.null(field_item$validators$presence)
    output_field_item$flip_flops <- NA
    # 全ての行をバインド
    output_field_items <- output_field_item %>% bind_rows(output_field_items, .)
  }

  if (is.data.frame(options)){
    # 不要な列を削除
    output_field_items <- output_field_items %>% select(-c("option_id"))
    # validatorsが存在し、かつすべて値が無ければ削除
    if ("validators" %in% colnames(output_field_items) && all(is.na(output_field_items$validators))) {
      output_field_items <- output_field_items %>% select(-validators)
    }
  }
  return(list(options=options, field_items=output_field_items, flip_flops=flip_flops))
}
