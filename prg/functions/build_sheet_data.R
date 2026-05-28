#' 1シートのデータを構築する
#'
#' @file build_sheet_data.R
#' @author Mariko Ohtsuka
#' @date 2026.5.28
#'
#' @param sheet シートオブジェクト（json_files$sheets の1要素）
#' @param is_visit VISIT対応試験かどうかのフラグ
#' @return 各シートのデータをまとめたリスト
BuildSheetData <- function(sheet, is_visit, options_flag, options_json) {
    sheet_name  <- sheet[[.const[["kAliasName"]]]]
    field_items <- sheet %>% GetFieldItems()

    temp           <- EditItemAndItemVisit(field_items, sheet_name)
    item_nonvisit  <- temp[[.const[["kItemNonVisit"]]]]
    item_visit_old <- temp[[.const[["kItemVisit"]]]]

    allocation <- sheet %>% GetAllocation()
    master     <- field_items %>% GetMaster(sheet)
    visit      <- if (!is_visit) field_items %>% GetVisit(sheet) else NULL
    name       <- tibble(
        name         = sheet[[.const[["kSheetJapaneseName"]]]],
        alias_name   = sheet_name,
        images_count = sheet[["images_count"]]
    )
    option     <- field_items %>% GetOptions(sheet, options_flag, options_json)
    assigned   <- field_items %>% EditAssigned(sheet)
    limitation <- field_items %>% EditLimitation(sheet)
    date       <- field_items %>% EditDate(sheet)

    item_nonvisit  <- JoinJpnameAndAliasNameAndSelectColumns(item_nonvisit, .const[["kItemNonVisit"]], sheet)
    item_visit_old <- JoinJpnameAndAliasNameAndSelectColumns(item_visit_old, .const[["kItemVisit_old"]], sheet)

    list(
        name           = name,
        item_nonvisit  = item_nonvisit,
        allocation     = allocation,
        master         = master,
        visit          = visit,
        item_visit_old = item_visit_old,
        option         = option,
        assigned       = assigned,
        limitation     = limitation,
        date           = date
    )
}
