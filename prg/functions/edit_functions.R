#' Edit the output content for each sheet.
#'
#' @file edit_functions.R
#' @author Mariko Ohtsuka
#' @date 2024.1.19
# ------ constants ------
kSheetItemsKeys <- list(id="id", jpname="jpname", alias_name="alias_name")
kFieldItemsKeys <- list(id="id", sheet_id="sheet_id", name="name", label="label", option.id="option.id")
kNames <- c(kSheetItemsKeys$jpname, kSheetItemsKeys$alias_name)
kNameAndLabelList <- c(kFieldItemsKeys$name, kFieldItemsKeys$label)
kKeySheetIdAndId <- c(kFieldItemsKeys$sheet_id, kFieldItemsKeys$id)
kNamesAndSheetIdAndId <- c(kNames, kFieldItemsKeys$id, kFieldItemsKeys$sheet_id)
kGroups <- "groups"
kMatchesOption. <- "^option\\."
kOptionName <- "option.name"
kOption.values <- "option.values"
kOption_id <- "option_id"
kOption.values_option_id <- kOption.values %>% str_c("_", kOption_id)
kFieldText <- "field"
kSheetItems <- "sheet_items"
kFlipFlopsField <- "fields"
kInputList <- list(sheet_items="df_sheet_items",
                   field_items="df_field_items",
                   option="df_option",
                   cdisc_sheet_config="list_cdisc_sheet_config",
                   flip_flops="df_flip_flops",
                   allocation="df_allocation")
# ------ functions ------
EditInputDataList <- function(json_files){
  function_list <- list(sheet_items=GetDfSheetItems,
                        field_items=GetDfFieldItems,
                        option=GetDfOptions,
                        cdisc_sheet_config=GetListCdiscSheetConfig,
                        flip_flops=GetDfFlipFlops,
                        allocation=GetDfAllocations)
  if (!all(names(function_list) == names(kInputList))){
    return(NULL)
  }
  input_list <- list()
  for (target in names(kInputList)){
    df_name <- kInputList[[target]]
    input_list[[df_name]] <- function_list[[target]](json_files)
    if (target != kSheetItems & is.data.frame(input_list[[df_name]])){
      df_sheet_items <- kInputList[[kSheetItems]] %>% input_list[[.]]
      input_list[[df_name]] <- input_list[[df_name]] %>%
        JoinJpnameAndAliasName(df_sheet_items) %>% select(all_of(kNames), everything())
    }
  }
  return(input_list)
}
GetDfFieldItems <- function(json_files){
  fieldItems <- json_files %>% map_df( ~ .$flattenJson$field_items)
  validators_presence <- GetValidatorsPresence(json_files)
  res <- fieldItems %>% inner_join(validators_presence, by=kKeySheetIdAndId)
  return(res)
}
GetDfSheetItems <- function(json_files){
  res <- json_files %>% map_df( ~ .$flattenJson %>% RemoveNestedLists() %>% map( ~ ReplaceText(.))) %>%
    rename(!!kSheetItemsKeys$jpname:="name")
  return(res)
}
GetListCdiscSheetConfig <- function(json_files){
  cdisc_sheet_config_list <- json_files %>% map( ~ {
    cdisc_sheet_config <- .$flattenJson$cdisc_sheet_config
    if (length(cdisc_sheet_config) == 0){
      return(NULL)
    }
    cdisc_sheet_config[[kSheetItemsKeys$jpname]] <- .$flattenJson$name
    cdisc_sheet_config[[kSheetItemsKeys$alias_name]] <- .$flattenJson$alias_name
    return(cdisc_sheet_config)
  }) %>% discard(is.null)
  return(cdisc_sheet_config_list)
}
GetDfFlipFlops <- function(json_files){
  df_flip_flops <- json_files %>% map( ~ {
    flatten_json <- .$flattenJson
    flip_flops <- flatten_json$field_items$flip_flops
    if (is.null(flip_flops)){
      return(NULL)
    }
    flip_flops <- flip_flops %>% list_c()
    if (length(flip_flops) == 0){
      return(NULL)
    }
    field_items <- flatten_json$field_items %>% select(any_of(c(kFieldItemsKeys$id, kNameAndLabelList)))
    fields <- NULL
    for (i in 1:length(flip_flops$id)){
      temp_fieldItems <- flip_flops$fields[[i]] %>% data.frame(field=.)
      temp_fields <- data.frame(id=flip_flops$id[i],
                                codes=flip_flops$codes[[i]]) %>% cbind(temp_fieldItems)
      fields <- rbind(fields, temp_fields)
    }
    df_field_label <- field_items %>% select(any_of(kNameAndLabelList)) %>% rename(fields.label=label)
    kJoinKeyId <- kFieldItemsKeys$id
    names(kJoinKeyId) <- kFieldText %>% str_c("_item_", kJoinKeyId)
    kJoinKeyName <- kFieldItemsKeys$name
    names(kJoinKeyName) <- kFieldText
    flip_flops <- flip_flops %>% RemoveListElements("codes") %>% RemoveListElements("fields") %>%
      inner_join(fields, by=c(kFieldItemsKeys$id)) %>%
      inner_join(field_items, by=c(kJoinKeyId)) %>%
      inner_join(df_field_label, by=c(kJoinKeyName)) %>%
      rename(c(field_item_id.name=name, field_item_id.label=label))
    flip_flops$sheet_id <- flatten_json$id
    return(flip_flops)
  }) %>% compact() %>% bind_rows()
  df_flip_flops <- df_flip_flops %>% rename(!!kFlipFlopsField:=all_of(kFieldText))
  return(df_flip_flops)
}
GetDfAllocations <- function(json_files){
  res <- json_files %>% map_df( ~ {
    json_file <- .
    flatten_json <- json_file$flattenJson
    if (is.null(flatten_json$allocation)){
      return(NULL)
    }
    groups <- flatten_json$allocation$groups %>%
      rename_with(~ paste0(kGroups, ".", .), everything())
    others <- flatten_json$allocation %>% RemoveListElements(c(kGroups)) %>% data.frame()
    allocation <- groups %>% cbind(others) %>% select(any_of(kNamesAndSheetIdAndId), everything())
    return(allocation)
  })
  return(res)
}
GetDfOptions <- function(json_files){
  kOptionHeadRegexp <- "^option(\\.|_)"
  join_key <- kOption.values_option_id
  names(join_key) <- kFieldItemsKeys$option.id

  df_option <- json_files %>% map_df( ~ {
    field_items <- .$flattenJson$field_items
    if (!is.data.frame(field_items)){
      return(NULL)
    }
    field_items_colnames <- field_items %>% colnames()
    if (!any(field_items_colnames == kOption.values)){
      return(NULL)
    }
    target_colnames <- field_items_colnames %>% keep(grepl(kOptionHeadRegexp, .) | . == kFieldItemsKeys$sheet_id)
    df_option <- field_items %>% select(all_of(target_colnames))
    option_values <- field_items[[kOption.values]] %>% list_rbind() %>% distinct() %>%
      rename_with(~ paste0(kOption.values, "_", .x), everything())
    res <- df_option %>% filter(!is.na(!!sym(kFieldItemsKeys$option.id))) %>% distinct() %>%
      inner_join(option_values, by=join_key)
    res[[kOption.values_option_id]] <- res[[kFieldItemsKeys$option.id]]
    return(res)
  })
  return(df_option)
}
GetValidatorsPresence <- function(json_files){
  validators_presence <- json_files %>%
    map_df( ~ {
      field_items <- .$rawJson$field_items
      if (length(field_items) == 0){
        return(NULL)
      }
      res <- field_items %>% map_df( ~ {
        field_item <- .
        df_validators_presence <- data.frame(
          id=field_item$id,
          sheet_id=field_item$sheet_id,
          validators.presence=!is.null(field_item$validators$presence))
        return(df_validators_presence)
      })
      return(res)
    })
  return(validators_presence)
}
EditFieldItemsBySheet <- function(target_id){
  field_items <- input_list[[kInputList$field_items]] %>% filter(sheet_id == target_id)
  if (nrow(field_items) == 0){
    return(NULL)
  }
  field_items$flip_flops <- ""
  return(field_items)
}
SelectFieldItemsBySheet <- function(field_items, options){
  if (is.null(field_items)){
    return(NULL)
  }
  keep_colnames <- field_items %>% colnames() %>%
        discard(grepl(kMatchesOption., .) &  . != kOptionName)
  if (is.null(options)){
    keep_colnames <- keep_colnames %>% discard(. == kOptionName)
  } else {
    keep_colnames <- keep_colnames %>% discard(. == kOption_id)
  }
  keep_colnames <- "^normal_range\\." %>% GetDiscardTargetColnames(field_items, ., keep_colnames)
  keep_colnames <- "^validators" %>% GetDiscardTargetColnames(field_items, ., keep_colnames)
  res <- field_items %>% select(all_of(keep_colnames)) %>% select(all_of(kNamesAndSheetIdAndId), everything())
  return(res)
}
GetDiscardTargetColnames <- function(target_df, target_name, keep_colnames){
  check_target <- target_df %>%
    select(matches(target_name)) %>% select(where( ~ all(is.na(.)))) %>% colnames()
  if (length(check_target) > 0){
    keep_colnames <- keep_colnames %>% setdiff(check_target)
  }
  return(keep_colnames)
}
EditOptionsBySheet <- function(id){
  options <- FilterDfByID(kInputList$option, id)
  if (nrow(options) == 0){
    return(NULL)
  }
  output_colnames <- options %>% colnames() %>% .[. != kFieldItemsKeys$sheet_id]
  options <- options %>% select(all_of(output_colnames)) %>% RemoveListElements(kOption.values)
  return(options)
}
EditCdiscSheetConfigsBySheet <- function(alias_name){
  cdisc_sheet_config <- input_list[[kInputList$cdisc_sheet_config]][[alias_name]]
  if (is.null(cdisc_sheet_config)){
    return(NULL)
  }
  res <- cdisc_sheet_config %>% select(-c("uuid", "created_at", "updated_at")) %>% select(all_of(kNamesAndSheetIdAndId), everything())
  return(res)
}
EditCdiscSheetConfigsPivotBySheet <- function(cdisc_sheet_config){
  kTable <- "table."
  kTableField <- kTable %>% str_c(kFieldText)
  kTableFieldValue <- kTableField %>% str_c(".value")
  if (is.null(cdisc_sheet_config)){
    return(NULL)
  }
  cdisc_sheet_config_pivot <- data.frame()
  for (i in 1:nrow(cdisc_sheet_config)){
    df_base <- cdisc_sheet_config[i, ] %>% select(!starts_with(kTable))
    df_table <- cdisc_sheet_config[i, ] %>% select(starts_with(kTable))
    df_temp <- data.frame()
    for (j in 1:ncol(df_table)){
      df_temp[j, kTableField] <- colnames(df_table)[j]
      df_temp[j, kTableFieldValue] <- df_table[1, j]
    }
    rownames(df_base) <- NULL
    cdisc_sheet_config_pivot <- df_base %>% cbind(df_temp) %>%
      bind_rows(cdisc_sheet_config_pivot, .)
  }
  return(cdisc_sheet_config_pivot)
}
EditAllocationBySheet <- function(id){
  allocation <- FilterDfByID(kInputList$allocation, id)
  if (nrow(allocation) == 0){
    return(NULL)
  }
  allocation <- allocation %>% select(all_of(kNames), everything())
  allocation$groups.allocatees <- ""
  return(allocation)
}
EditFlipFlopsBySheet <- function(id){
  flip_flops <- FilterDfByID(kInputList$flip_flops, id)
  if (nrow(flip_flops) == 0){
    return(NULL)
  }
  res <- flip_flops %>% select(any_of(c(kNames, "id", "field_item_id", "codes", "fields", "created_at", "updated_at")))
  res$codes <- ""
  return(res)
}
FilterDfByID <- function(target_name, target_id){
  return(input_list[[target_name]] %>% filter(sheet_id == target_id))
}
JoinJpnameAndAliasName <- function(target, sheet_items){
  df_sheet_items <- sheet_items %>% select(kSheetItemsKeys %>% unlist()) %>% rename(sheet_id=id)
  res <- target %>% inner_join(df_sheet_items, ., by=c(kFieldItemsKeys$sheet_id))
  return(res)
}
EditOutputColumns <- function(df_target, target_column){
  res <- df_target %>%
    RemoveNestedLists() %>%
      select(any_of(target_column))
  return(res)
}
ExecEditOutputData <- function(id, jpname, alias_name){
  options <- EditOptionsBySheet(id)
  field_items <- EditFieldItemsBySheet(id) %>% SelectFieldItemsBySheet(options)
  cdisc_sheet_config <- EditCdiscSheetConfigsBySheet(alias_name)
  cdisc_sheet_config_pivot <- EditCdiscSheetConfigsPivotBySheet(cdisc_sheet_config)
  allocation <- EditAllocationBySheet(id)
  flip_flops <- EditFlipFlopsBySheet(id)
  res <- list(
    Field_Items=field_items,
    Option=options,
    Flip_Flops=flip_flops,
    Cdisc_Sheet_Configs=cdisc_sheet_config,
    Cdisc_Sheet_Configs_Pivot=cdisc_sheet_config_pivot,
    Allocation=allocation
  ) %>% discard(is.null)
  if (!exists("Cdisc_Sheet_Configs", res)){
    res$Cdisc_Sheet_Configs <- df_dummyNames
  }
  return(res)
}
