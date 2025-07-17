#' test script
#'
#' @file excel_json_validator_allocation.R
#' @author Mariko Ohtsuka
#' @date 2025.7.17
GetAllocation <- function(sheetList, jsonList, fieldItems, jpNameAndAliasName, sheetName) {
    sheet <- sheetList[[sheetName]] |>
        rename(!!!engToJpnColumnMappings[[sheetName]])
    json <- GetAllocationFromJson(jsonList, fieldItems, jpNameAndAliasName)
    sheet[["groups.if_references"]] <- ifelse(is.na(sheet[["groups.if_references"]]), "", sheet[["groups.if_references"]])
    json[["groups.if_references"]] <- ifelse(is.na(json[["groups.if_references"]]), "", json[["groups.if_references"]])
    sheet[["formula_field_references"]] <- ifelse(is.na(sheet[["formula_field_references"]]), "", sheet[["formula_field_references"]])
    json[["formula_field_references"]] <- ifelse(is.na(json[["formula_field_references"]]), "", json[["formula_field_references"]])
    sheet[["formula_field_references"]] <- sheet[["formula_field_references"]] |> str_remove_all(" ")
    json[["formula_field_references"]] <- json[["formula_field_references"]] |> str_remove_all(" ")
    sheet[["formula_field"]] <- as.character(json[["formula_field"]])
    json[["formula_field"]] <- as.character(sheet[["formula_field"]])
    return(list(sheet = sheet, json = json))
}
CheckAllocation <- function(sheetList, jsonList, fieldItems, jpNameAndAliasName, sheetName) {
    temp <- GetAllocation(sheetList, jsonList, fieldItems, jpNameAndAliasName, sheetName)
    sheet <- temp[["sheet"]]
    json <- temp[["json"]]
    return(CheckTarget(sheet, json))
}
GetAllocationFromJson <- function(jsonList, fieldItems, jpNameAndAliasName) {
    allocationColnames <- c(
        "jpname", "alias_name", "is_zelen", "zelen_imbalance", "is_double_blinded",
        "double_blind_emails", "allocation_method", "groups.if", "groups.if_references", "groups.code", "groups.label",
        "groups.message", "formula_field", "formula_field_references"
    )
    allocationList <- jsonList |>
        keep(~ .[["alias_name"]] |> str_detect("(?i)^allocation([0-9]+)?$"))
    if (length(allocationList) == 0) {
        df <- tibble(!!!setNames(vector("list", length(allocationColnames)), allocationColnames))
    } else {
        aliasnameAndFieldIdAndLabel <- GetAliasnameAndFieldIdAndLabel(fieldItems)
        df <- allocationList |>
            map(~ {
                name <- .[["name"]]
                aliasName <- .[["alias_name"]]
                allocation <- .[["allocation"]]
                groups <- allocation[["groups"]] |> map_df(~ c(groups.code = .[["code"]], groups.label = .[["label"]], groups.if = .[["if"]], groups.message = .[["message"]]))
                groups[["alias_name"]] <- aliasName
                groups[["is_zelen"]] <- allocation[["is_zelen"]]
                groups[["zelen_imbalance"]] <- allocation[["zelen_imbalance"]] |> as.numeric()
                groups[["is_double_blinded"]] <- allocation[["is_double_blinded"]]
                groups[["double_blind_emails"]] <- allocation[["double_blind_emails"]]
                groups[["allocation_method"]] <- allocation[["allocation_method"]]
                return(groups)
            }) |>
            bind_rows()
        temp_ref <- df[["groups.if"]] |>
            str_remove_all(" ") |>
            str_extract_all("ref\\('[a-zA-Z0-9]+',[0-9]+\\)")
        temp_ref2 <- temp_ref |>
            map(~ {
                if (length(.) == 0) {
                    return(c(groups.if_references = ""))
                }
                temp <- . |> map_chr(~ {
                    temp <- . |>
                        str_split(",") |>
                        list_c()
                    aliasName <- temp[1] |>
                        str_remove("ref") |>
                        str_extract("[a-zA-Z0-9]+") |>
                        unlist()
                    fieldName <- temp[2] |>
                        str_extract("[0-9]+") %>%
                        str_c("field", .)
                    label <- aliasnameAndFieldIdAndLabel |>
                        filter(alias_name == aliasName & fields == fieldName) %>%
                        .[1, "fields.label"] |>
                        unlist()
                    res <- str_c("(", aliasName, ",", fieldName, ",", label, ")")
                    return(res)
                })
                res <- temp |>
                    unique() |>
                    str_c(collapse = "")
                return(c(groups.if_references = res))
            }) |>
            bind_rows()
        df <- df |> cbind(temp_ref2)
        temp_formula_fields <- allocationList |>
            map(~ {
                myAliasName <- .[["alias_name"]]
                fieldItems <- .[["field_items"]] |> keep(~ .[["type"]] == "FieldItem::Allocation")
                if (length(fieldItems) == 0) {
                    return(NULL)
                }
                formula_field <- fieldItems |>
                    map(~ {
                        input_text <- .[["formula_field"]]
                        temp <- input_text |>
                            str_remove_all(" ") |>
                            str_extract_all("ref\\('[a-zA-Z0-9]+',[0-9]+\\)") |>
                            list_c()
                        if (length(temp) == 0) {
                            return(tibble(alias_name = myAliasName, formula_field = .[["formula_field"]], formula_field_references = .[["formula_field"]]))
                        }
                        for (i in 1:length(temp)) {
                            temp2 <- temp[[i]] |>
                                str_split(",") |>
                                list_c()
                            aliasName <- temp2[1] |>
                                str_remove("ref") |>
                                str_extract("[a-zA-Z0-9]+") |>
                                unlist()
                            fieldName <- temp2[2] |>
                                str_extract("[0-9]+") %>%
                                str_c("field", .)
                            label <- aliasnameAndFieldIdAndLabel |>
                                filter(alias_name == aliasName & fields == fieldName) %>%
                                .[1, "fields.label"] |>
                                unlist()
                            res <- str_c("(", aliasName, ",", fieldName, ",", label, ")")
                            temp3 <- temp[i] |>
                                str_replace(fixed("ref("), "ref\\(") |>
                                str_replace(fixed(")"), "\\)")
                            input_text <- input_text |>
                                str_remove_all(" ") |>
                                str_replace(temp3, res)
                        }
                        return(tibble(alias_name = myAliasName, formula_field = .[["formula_field"]], formula_field_references = input_text))
                    }) |>
                    bind_rows()
                if (nrow(formula_field) == 0) {
                    return(NULL)
                }
                temp0 <- formula_field[1, "alias_name"] |> as.character()
                temp1 <- formula_field[["formula_field"]] |> paste(collapse = ", ")
                temp2 <- formula_field[["formula_field_references"]] |> paste(collapse = ", ")
                res <- tibble(alias_name = temp0, formula_field = temp1, formula_field_references = temp2)
                return(res)
            }) |>
            bind_rows()
        if (is.null(temp_formula_fields) || nrow(temp_formula_fields) == 0) {
            temp_formula_fields <- tibble(
                alias_name = character(),
                formula_field = character(),
                formula_field_references = character()
            )
        }
        df <- df |> left_join(temp_formula_fields, by = "alias_name")
    }
    res <- GetItemsSelectColnames(df, allocationColnames, jpNameAndAliasName)
    return(res)
}
