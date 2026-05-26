#' 定数をグローバル環境にセットする
#'
#' @file constants.R
#' @author Mariko Ohtsuka
#' @date 2026.5.26
#' @dependencies here, purrr, edit_checklist_convert_column_name.R
SetConstants <- function() {
    .const[["kOutputJapanaseNameEnglish"]] <- "jpname"
    .const[["kInputFolderName"]] <- "input"
    .const[["kOutputFolderName"]] <- "output"
    .const[["kOutputPath"]] <- here(.const[["kOutputFolderName"]])
    .const[["kNameJapaneseColumnName"]] <- "シート名"
    .const[["kAliasNameJapaneseColumnName"]] <- "シート名英数字別名"
    .const[["kItemVisitConditionalFormattingColumnName"]] <- "数値チェック・アラート条件の有無"
    .const[["kReferenceColnames"]] <- c("条件の参照先情報", "論理式の参照先情報", "最小値の参照先情報", "最大値の参照先情報")
    .const[["kEngToJpnColumnMappings"]] <- GetEngToJpnColumnMappings()
    .const[["kEngColumnNames"]] <- .const[["kEngToJpnColumnMappings"]] %>% map(names)

    # jsonファイルの要素名
    .const[["kSheetJapaneseName"]] <- "name"
    .const[["kAliasName"]] <- "alias_name"
    .const[["kSheets"]] <- "sheets"
    ## sheet_orders
    .const[["kSheetOrders"]] <- "sheet_orders"
    .const[["kSheetOrdersAliasName"]] <- "sheet"
    .const[["kSheetSeq"]] <- "seq"
    ## options
    .const[["kOptions"]] <- "options"
    .const[["kOptionSeq"]] <- "seq"
    ## sheet_groups
    .const[["kSheetGroups"]] <- "sheet_groups"
    .const[["kSheetGroupsName"]] <- "name"
    .const[["kSheetGroupAllocationGroup"]] <- "allocation_group"
    ## allocation
    .const[["kAllocation"]] <- "allocation"
    .const[["kAllocationSheet"]] <- "allocation_sheet"
    .const[["kAllocationGroups"]] <- "groups"
    .const[["kAllocationGroupsCode"]] <- "code"
    .const[["kAllocationGroupsLabel"]] <- "label"
    ## sheets
    .const[["kCategory"]] <- "category"
    ## sheets$field_items
    .const[["kFieldItems"]] <- "field_items"
    .const[["kFieldItemsType"]] <- "type"
    .const[["kFieldItemsFieldId"]] <- "name"
    .const[["kFieldItemsFieldName"]] <- "label"
    .const[["kFieldItemsSeq"]] <- "seq"
    .const[["kFieldItemDefaultValue"]] <- "default_value"
    .const[["kValidateDateAfterOrEqualTo"]] <- c("validators", "date", "validate_date_after_or_equal_to")
    .const[["kValidateDateBeforeOrEqualTo"]] <- c("validators", "date", "validate_date_before_or_equal_to")
    .const[["kValidatePresenceIf"]] <- c("validators", "presence", "validate_presence_if")
    .const[["kValidateFormulaIf"]] <- c("validators", "formula", "validate_formula_if")
    .const[["kValidatorsNumericality"]] <- c("validators", "numericality")
    .const[["kValidatorsNumericalityLessThanOrEqualTo"]] <- c(.const[["kValidatorsNumericality"]], "validate_numericality_less_than_or_equal_to")
    .const[["kValidatorsNumericalityGreaterThanOrEqualTo"]] <- c(.const[["kValidatorsNumericality"]], "validate_numericality_greater_than_or_equal_to")
    .const[["kNormalRangeGreaterThanOrEqualTo"]] <- c("normal_range", "greater_than_or_equal_to")
    .const[["kNormalRangeLessThanOrEqualTo"]] <- c("normal_range", "less_than_or_equal_to")

    ## visits
    .const[["kVisits"]] <- "visits"
    .const[["kVisitGroup"]] <- "visit_groups"
    .const[["kSheetAliasName"]] <- "sheet_alias_name"

    # 条件コンスタント
    .const[["kArticle"]] <- "FieldItem::Article"
    .const[["kCategoryAllocation"]] <- "allocation"
    .const[["kCategoryVisit"]] <- "visit"

    # 出力シート名
    .const[["kItemNonVisit"]] <- "item_nonvisit"
    .const[["kItemVisit"]] <- "item_visit"
    .const[["kItemVisit_old"]] <- "item_visit_old"
    .const[["kVisit"]] <- "visit"
    .const[["kSheetGroupsVisit"]] <- "sheet_groups_visit"
    .const[["kSheetGroupsNonVisit"]] <- "sheet_groups_non_visit"
    .const[["kTargetSheetNames"]] <- c(
        .const[["kItemVisit"]],
        .const[["kItemVisit_old"]],
        .const[["kItemNonVisit"]],
        .const[["kVisit"]],
        .const[["kAllocation"]], 
        .const[["kSheetGroupsVisit"]],
        .const[["kSheetGroupsNonVisit"]],
        "limitation", "date", "option", "master", "assigned"
    )
    .const[["kSortOrderSheetNames"]] <- c(
        .const[["kItemVisit"]],
        .const[["kItemNonVisit"]],
        .const[["kVisit"]],
        .const[["kAllocation"]], 
        .const[["kSheetGroupsVisit"]],
        .const[["kSheetGroupsNonVisit"]],
        "limitation", "date", "option", "name", "master", "assigned"
    )
}
.const <- new.env(parent = emptyenv())
SetConstants()
