#' Functions for checklist output.
#'
#' @file edit_checklist_function.R
#' @author Mariko Ohtsuka
#' @date 2024.8.28
# ------ constants ------
kReferenceSearchColname <- "input_text"
kReferenceJoinColname <- "input_text_2"
kReferenceOutputColname <- "output_text"
# ------ functions ------
GetTargetColumns <- function(input_list){
  kNamesAndNameAndLabel <- c(kNames, kNameAndLabelList)
  res <- list()
  res$name <- c("name", kSheetItemsKeys$alias_name , "images_count")
  res$item <- c(kNamesAndNameAndLabel, "option.name", "default_value", "validators.presence.validate_presence_if", "presence_if_references", "validators.formula.validate_formula_if", "formula_if_references", "validators.formula.validate_formula_message", "validators.date.validate_date_after_or_equal_to", "references_after", "validators.date.validate_date_before_or_equal_to", "references_before")
  res$option <- c(kNames, "option.name", "option.values_name", "option.values_seq", "option.values_code", "option.values_is_usable")
  res$visit <- c(kNames, "name", "default_value")
  res$number <- c(kNamesAndNameAndLabel, "default_value", "validators.numericality.validate_numericality_less_than_or_equal_to", "validators.numericality.validate_numericality_greater_than_or_equal_to")
  res$master <- c(kNamesAndNameAndLabel, "link_type")
  res$alert <- c(kNamesAndNameAndLabel, kAlertTargetColnames)
  res$action <- c(kNames, "id", "field_item_id", "field_item_id.name", "field_item_id.label", "codes", "fields", "fields.label")
  res$allocation <- c(kNames, "is_zelen", "zelen_imbalance", "is_double_blinded", "double_blind_emails", "allocation_method", "groups.code", "groups.label", "groups.if", "references", "groups.message")
  res$presence <- kNamesAndNameAndLabel
  res$display <- kNamesAndNameAndLabel
  res$comment <- c(kNamesAndNameAndLabel, "content")
  res$explanation <- c(kNamesAndNameAndLabel, "description")
  res$title <- c(kNamesAndNameAndLabel, "level")
  res$assigned <- c(kNamesAndNameAndLabel, "default_value")
  return(res)
}
OutputChecklistSheet <- function(df_output, wb, sheet_name){
  output_colnames <- df_output %>% colnames()
  addWorksheet(wb=wb, sheet=sheet_name)
  writeDataTable(wb=wb, sheet=sheet_name, x=df_output,
                 startRow=1, startCol=1, colNames=T, rowNames=F, withFilter=T,
                 tableStyle=kTableStyle, keepNA=F)
  setColWidths(wb=wb, sheet=sheet_name, cols=1:ncol(df_output), widths="auto")
  fontStyle <- setFontStyle()  
  addStyle(wb = wb, sheet = sheet_name, style = fontStyle, rows = 1:(nrow(df_output) + 1), cols = 1:ncol(df_output), gridExpand = TRUE)
  
  return(wb)
}
OutputChecklistXlsx <- function(output_list, output_checklist_path){
  wb <- createWorkbook()
  for (i in 1:length(output_list)){
    wb <- OutputChecklistSheet(df_output=output_list[[i]], wb=wb, sheet_name=names(output_list)[i])
  }
  saveWorkbook(wb=wb, file=str_c(output_checklist_path, "/", kOutputChecklistName), overwrite=T)
}
GetFieldForReference <- function(json_files){
  res <- json_files %>% map_df( ~ {
    field_items <- .$flattenJson$field_items
    if (length(field_items) == 0){
      return(NULL)
    }
    field_items$field_id <- field_items$name %>% str_remove(kFieldText)
    res <- field_items %>% select(any_of(c(kFieldItemsKeys$sheet_id, "field_id", "label")))
    return(res)
  })
  return(res)
}
GetSheetnameList <- function(json_files){
  res <- json_files %>% map_df( ~ c(sheet_id=.$flattenJson$id,
                                    jpname=.$flattenJson$name,
                                    alias_name=.$flattenJson$alias_name))
  res$sheet_id <- as.numeric(res$sheet_id)
  return(res)
}
GetSheetnameAndFieldForReference <- function(json_files){
  sheetid_aliasname <- GetSheetnameList(json_files)
  field_for_reference <- GetFieldForReference(json_files)
  df_for_reference <- sheetid_aliasname %>%
    left_join(field_for_reference, by=c(kFieldItemsKeys$sheet_id))
  df_for_reference[[kReferenceOutputColname]] <- GetDfForReferenceOutputText(df_for_reference)
  return(df_for_reference)
}
GetDfForReferenceOutputText <- function(df_for_reference){
  return(str_c("(",
               df_for_reference$alias_name, ",",
               kFieldText, df_for_reference$field_id, ",",
               df_for_reference$label,
               ")",
               sep=""))
}
ReplaceReferenceText <- function(df_target, input_colname, output_colname){
  input_target <- df_target %>% filter(!!sym(input_colname) %>% str_detect("\\d"))
  if (nrow(input_target) == 0){
    df_target[[output_colname]] <- ""
    return(df_target)
  }
  input_target[[output_colname]] <- ""
  output_target <- input_target %>% select(all_of(c(kSheetItemsKeys$alias_name, input_colname)))
  field_head <- "f"
  ref_head <- "ref\\('[a-z0-9]+'\\D+"
  condition_foot <- "\\d+"
  ref_index <- list(alias_name=2, field_id=3)
  # "fieldx" or "fx" or "ref(sheetname, x)"
  condition <- c(field_head, kFieldText, ref_head) %>% str_c(condition_foot) %>% paste0(collapse="|")
  for (i in 1:nrow(output_target)){
    alias <- output_target[i, kSheetItemsKeys$alias_name, drop=T]
    target <- NULL
    extract_text <- output_target[i, input_colname, drop=T]
    while (str_detect(extract_text, condition)){
      target <- c(target, str_extract(extract_text, condition))
      extract_text <- extract_text %>% str_remove(condition) %>% str_remove_all("\\s")
    }
    output_value <- target %>% map( ~ {
      target_text <- .
      if (str_detect(target_text, ref_head)){
        temp <- target_text %>% str_split("'") %>% list_c()
        alias <- temp[ref_index$alias_name]
        target_field_id <- temp[ref_index$field_id] %>% str_extract(condition_foot) %>% as.numeric()
      } else {
        target_field_id <- target_text %>% str_remove(kFieldText) %>% str_remove(field_head)
      }
      result <- df_reference %>% filter(alias_name == alias & field_id == target_field_id) %>% .[[kReferenceOutputColname]]
    })
    output_target[i, output_colname] <- output_value %>% unique() %>% paste0(collapse="")
  }
  output_target <- output_target %>% distinct()
  res <- df_target %>% left_join(output_target, by=c(kSheetItemsKeys$alias_name, input_colname))
  return(res)
}
EditAllocation <- function(allocation){
  if (nrow(allocation) == 0) {
    empty_df <- data.frame(matrix(ncol = length(target_columns$allocation), nrow = 0))
    colnames(empty_df) <- target_columns$allocation
    return(empty_df)
  }
  res <- allocation %>% ReplaceReferenceText("groups.if", "references")
  res <- res %>% EditOutputColumns(target_columns$allocation)
  return(res)
}
EditOutputFieldItemsSum <- function(df_field_items){
  res <- df_field_items %>%
    ReplaceReferenceText("validators.presence.validate_presence_if", "presence_if_references") %>%
    ReplaceReferenceText("validators.formula.validate_formula_if", "formula_if_references") %>%
    ReplaceReferenceText("validators.date.validate_date_after_or_equal_to", "references_after") %>%
    ReplaceReferenceText("validators.date.validate_date_before_or_equal_to", "references_before")
  return(res)
}
EditOutputItem <- function(df_field_items, df_sheet_items){
  conditions <- c(item='type == "FieldItem::Article"')
  df_sheet_field <- df_sheet_items %>%
    inner_join(df_field_items, by=c(kFieldItemsKeys$sheet_id, kSheetItemsKeys$jpname, kSheetItemsKeys$alias_name))
  output_list <- FilterDataByConditions(df_sheet_field, conditions)
  return(output_list$item)
}
EditOutputData <- function(target){
  df_input <- input_list[[kInputList[[target]]]]
  exec_function <- c(get(str_c("EditOutputData_", target)))
  output_list <- list()
  output_list <- output_list %>% exec_function[[1]](df_input, .)
  return(output_list)
}
EditOutputData_alert <- function(df_input, output_list){
  res <- df_input %>%
    filter(if_any(all_of(kAlertTargetColnames), ~ . != "")) %>%
    select(target_columns$alert)
  output_list[["alert"]] <- res
  return(output_list)
}
EditOutputData_sheet_items <- function(df_input, output_list){
  return(NULL)
}
EditOutputData_field_items <- function(df_input, output_list){
  conditions <- c(
    visit='label == "Visit Number"',
    number='(!is.na(validators.numericality.validate_numericality_less_than_or_equal_to) & validators.numericality.validate_numericality_less_than_or_equal_to !="") |
            (!is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) & validators.numericality.validate_numericality_greater_than_or_equal_to !="")',
    master='!is.na(link_type) & link_type != ""',
    presence='type == "FieldItem::Article" & !validators.presence',
    display='(type == "FieldItem::Assigned" & !is_invisible) | (type == "FieldItem::Article" & is_invisible)',
    comment='!is.na(content)',
    explanation='!is.na(description) & description != ""',
    title='type == "FieldItem::Heading"',
    assigned='type == "FieldItem::Assigned"'
  )
  output_list <- FilterDataByConditions(df_input, conditions)
  return(output_list)
}
EditOutputData_option <- function(df_input, output_list){
  conditions <- c(option="option.values_is_usable")
  df_target_field_items <- input_list$df_field_items %>%
    select(all_of(c(kFieldItemsKeys$sheet_id, kOption_id, "type"))) %>%
      filter(type == "FieldItem::Article" & !is.na(get(kOption_id))) %>%
        distinct()
  df_target_option <- df_input %>% inner_join(df_target_field_items, by=c(kFieldItemsKeys$sheet_id, kOption_id))
  output_list <- FilterDataByConditions(df_target_option, conditions)
  return(output_list)
}
EditOutputData_cdisc_sheet_config <- function(df_input, output_list){
  return(NULL)
}
EditOutputData_flip_flops <- function(df_input, output_list){
  if (is.null(df_input)) {
    # Create an empty dataframe
    df_input <- tibble(!!!set_names(rep("", length(target_columns$action)), target_columns$action))
  }
  conditions <- c(action=NA)
  output_list <- FilterDataByConditions(df_input, conditions)
  return(output_list)
}
EditOutputData_allocation <- function(df_input, output_list){
  output_list$allocation <- df_input %>% EditAllocation()
  return(output_list)
}
FilterDataByConditions <- function(df_input, conditions){
  res <- map2(names(conditions), conditions, ~ {
    condition <- .y
    names(condition) <- .x
    filter_data <- FilterDataByCondition(df_input, condition)
    return(filter_data)
  })
  names(res) <- names(conditions)
  return(res)
}
FilterDataByCondition <- function(df_input, condition_str){
  target_col <- target_columns[[names(condition_str)]]
  if (!is.na(condition_str) && condition_str != ""){
    condition <- parse_expr(condition_str)
    df_filter <- df_input %>% filter(!!condition)
  } else {
    df_filter <- df_input
  }
  result <- df_filter %>% EditOutputColumns(target_col)
  return(result)
}
EditOutputDataList <- function(input_list){
  target_names <- names(kInputList)
  res <- target_names %>% map( ~ EditOutputData(.)) %>% discard(is.null) %>% list_flatten()
  output_list <- list()
  df_sheet_items <- input_list[[kInputList$sheet_items]] %>% rename(!!kFieldItemsKeys$sheet_id:=id)
  name <- df_sheet_items %>% rename(name:=!!sym(kSheetItemsKeys$jpname)) %>% EditOutputColumns(target_columns$name)
  fielditems <- input_list[[kInputList$field_items]] %>% EditOutputFieldItemsSum()
  item <- fielditems %>% EditOutputItem(df_sheet_items)
  temp_output_list <- c(list(name=name, item=item), res)
  output_list <- temp_output_list[names(target_columns)] %>% keep( ~ !is.null(.))
  if (!is.null(output_list$action)) {
    output_list$action <- output_list$action %>% distinct()
  }
  return(output_list)
}
GetTargetJsonForChecklist <- function(raw_json_files) {
  rawJsons <- raw_json_files %>% map( ~ .$rawJson)
  visitJsons <- rawJsons %>% map( ~ {
    rawJson <- .
    if (.$category != "visit") {
      return(NULL)
    }
    return(rawJson)
  }) %>% keep( ~ !is.null(.))
  visitJsonsNames <- names(visitJsons)
  visitJsonFileNamesHead <- visitJsonsNames %>% str_remove("_[0-9]+$") %>% unique()
  targetFileNameHead <- visitJsonFileNamesHead %>% map( ~ {
    targetHead <- .
    filecount <- names(visitJsons) %>% str_detect(str_c("^", targetHead, "_[0-9]+$")) %>% sum()
    if (filecount == 1) {
      return(NULL)
    }
    return(targetHead) 
  }) %>% keep( ~ !is.null(.))
  if (length(targetFileNameHead) == 0) {
    return(raw_json_files)
  }
  targetVisitRawJson <- targetFileNameHead %>% map( ~ {
    targetHead <- .
    for (i in 1:length(visitJsonsNames)) {
      if (str_detect(visitJsonsNames[[i]], str_c("^", targetHead, "_[0-9]+$"))) {
        target <- raw_json_files[[i]]
        newAliasname <- str_c(targetHead, "_xxx")
        target$rawJson$alias_name <- newAliasname
        target$flattenJson$alias_name <- newAliasname
        return(target)
        break
      }
    }
  })
  names(targetVisitRawJson) <- targetFileNameHead
  notVisitJsons <- raw_json_files
  raw_json_files_names <- names(notVisitJsons)
  for (i in 1:length(raw_json_files_names)) {
    filename <- raw_json_files_names[i]
    for (j in 1:length(targetFileNameHead)) {
      if (str_detect(filename, str_c(targetFileNameHead[[j]], "_[0-9]+$"))) {
        notVisitJsons[[i]] <- NA
        break
      }
    }
  }
  notVisitJsonList <- notVisitJsons %>% map( ~ .[!is.na(.)]) %>% discard( ~ length(.) == 0)
  res <- append(notVisitJsonList, targetVisitRawJson)
  sorted_indices <- res %>% names() %>% order()
  res <- res[sorted_indices]  
  return(res)
}
