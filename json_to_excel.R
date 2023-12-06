# ------ libraries ------
library(jsonlite)
library(tidyverse)
library(writexl)

# ------ constants ------
# windows
#user_profile <- Sys.getenv("USERPROFILE")
#input_path <- gsub("/", "\\", paste0(file.path(Sys.getenv("USERPROFILE"), "Box", "Projects", "NMC ISR 情報システム研究室", "Ptosh", "JSON"), "\\"), fixed = TRUE)
input_path <- "C:\\works\\input\\"
output_path <- "C:\\works\\output\\"
# Mac
#input_path <- "~/Library/CloudStorage/Box-Box/Projects/NMC ISR 情報システム研究室/Ptosh/JSON/ALL-B19"
#input_path <- "/Users/works/input"
#output_path <- "/Users/works/output"

# ------ functions ------
get_info_for_field <- function(field, crfname, df_lookup) {
  field_name <- sub("^f([0-9]+)$", "field\\1", field)
  row <- df_lookup[df_lookup$name == field_name & df_lookup$alias_name == crfname, ]
  if (nrow(row) == 0) {
    return(character(0))
  }
  sprintf("(%s,%s,%s)", row$alias_name, row$name, row$label)
}

extract_references <- function(df_data, logic_column_name, df_lookup) {
  sapply(1:nrow(df_data), function(i) {
    logic <- df_data[[logic_column_name]][i]
    fields <- unique(unlist(regmatches(logic, gregexpr("\\b(f[0-9]+|field[0-9]+)", logic))))

    refs_matches <- regmatches(logic, gregexpr("ref\\(\\s*'([a-zA-Z0-9]+)',\\s*([0-9]+)\\s*\\)", logic))
    results_ref <- character(0)
    if (length(refs_matches[[1]]) > 0) {
      for (j in 1:length(refs_matches[[1]])) {
        match_info <- regmatches(refs_matches[[1]][j], regexec("ref\\(\\s*'([a-zA-Z0-9]+)',\\s*([0-9]+)\\s*\\)", refs_matches[[1]][j]))
        refs_crfname <- match_info[[1]][2]
        refs_field <- paste0("field", match_info[[1]][3])
        row <- df_lookup[df_lookup$alias_name == refs_crfname & df_lookup$name == refs_field, ]
        if (nrow(row) > 0) {
          results_ref <- c(results_ref, sprintf("(%s,%s,%s)", row$alias_name, row$name, row$label))
        }
      }
    }
    results_field <- sapply(fields, function(field) {
      get_info_for_field(field, df_data$alias_name[i], df_lookup)
    })
    result <- unique(c(results_field, results_ref))
    paste(result, collapse = "")
  })
}

PresenceChecker <- function(path) {
  field_items <- read_json(path, simplifyVector=FALSE) %>% .$field_items
  validators.presence <- NULL
  for (i in 1:length(field_items)) {
    presence <- field_items[[i]]$validators$presence
    validators.presence[i] <- !is.null(presence)
  }
  return(validators.presence)
}

# ------ main ------
# フォルダ内の全ての.jsonファイルを読み込む
json_files <- list.files(input_path, pattern = "*.json", full.names = TRUE)

df_crfinfo <- data.frame()
df_field_items_sum <- data.frame()
df_allocation_sum <- data.frame()
df_option_sum <- data.frame()
df_flip_flops_sum <- data.frame()

for (json_file in json_files) {

  # JSONファイルの読み込み
  data <- fromJSON(json_file, flatten = TRUE)

  # 必要な要素のみを保持する
  subset_list <- list(name = data$name,
                      alias_name = data$alias_name,
                      stylesheet = data$stylesheet,
                      fax_stylesheet = data$fax_stylesheet,
                      javascript = data$javascript,
                      category = data$category,
                      images_count = data$images_count,
                      lab_department_id = data$lab_department_id)
  # NULLをNAに変換
  subset_list <- lapply(subset_list, function(x) ifelse(is.null(x), NA, x))
  # 最上位の情報で必要な要素のみのデータフレームを作成
  df_crfinfo <- rbind(df_crfinfo,subset_list)

  presenceExists <- NULL
  # 'field_items'が存在する場合、そのデータフレームを生成し、列を展開
  # 'field_items'が存在しない場合、data全体をdf_field_itemsにセット
  if ("field_items" %in% names(data)) {
    df_field_items <- data$field_items
    if (is.data.frame(df_field_items)) {
      presenceExists <- PresenceChecker(json_file)
      df_field_items <- df_field_items %>% mutate("validators.presence" = presenceExists)
    }
  } else {
    df_field_items <- data
  }


  # jsonファイル名取得
  #crf_name <- gsub(".*/|\\.json$", "", json_file)
  crf_name <- gsub("^.*[\\\\/]|\\.json$", "", json_file) #Windows,mac両パターン
  # 日本語名取得
  jp_name <- subset_list$name

  # jsonファイル名のデータフレームを作成
  assign(paste0("df_", crf_name), df_field_items)

  # optionで始まる列名を取得
  option_columns <- grep("^option", names(df_field_items), value = TRUE)

  # optionで始まる列名が存在する場合
  if (any(length(option_columns) > 0)) {
    # optionで始まる列を抽出して新たなデータフレームを作成
    df_option <- df_field_items[ , option_columns]
    # 'option.values'列が存在し、それがネストされたデータフレームまたはリストであるかを確認
    if ("option.values" %in% names(df_option) && is.list(df_option$option.values)) {
      # option.values列のデータをフラット化し、複数の列に分解します
      df_option <- df_option %>%
        unnest(cols = option.values, names_sep = "_")
#      # df_field_itemsデータフレームからoptionで始まる列を削除
#      df_field_items <- df_field_items[ , !(names(df_field_items) %in% option_columns)]

      # "option.name" 列以外の "option" で始まる列を削除
      df_field_items <- df_field_items[, !(names(df_field_items) %in% option_columns & !grepl("^option.name", names(df_field_items)))]

      # 重複を除去
      df_option <- df_option %>%
        distinct()
    }
  }

  # flip_flopsという列名が存在する場合
  df_flip_flops <- NULL
  if ("flip_flops" %in% names(df_field_items)) {
      if (any(lengths(df_field_items$flip_flops) > 0)) {
        # リスト形式のデータをデータフレームとして展開
        df_flip_flops <- df_field_items %>%
          select(flip_flops) %>%
          unnest(cols = flip_flops)
        # さらに、df_flip_flopsのfields列を展開
        if ("fields" %in% names(df_flip_flops) && any(lengths(df_flip_flops$fields) > 0)) {
          df_flip_flops <- df_flip_flops %>%
            unnest(cols = fields)
        }
      }
  }

  # 'cdisc_sheet_configs'が存在する場合、そのデータフレームを生成し、列を展開
  # 'cdisc_sheet_configs'が存在しない場合、NULLをセット
  df_cdisc_sheet_configs <- NULL
  df_cdisc_sheet_configs_pivot <- NULL
  if ("cdisc_sheet_configs" %in% names(data)) {
    df_cdisc_sheet_configs <- data.frame(data$cdisc_sheet_configs)

    if (nrow(df_cdisc_sheet_configs) > 0) {
      # `table.fieldXXX` の列を行に展開のデータフレーム
      df_cdisc_sheet_configs_pivot <- df_cdisc_sheet_configs %>%
        pivot_longer(cols = starts_with("table.field"), names_to = "table.field", values_to = "table.field.value")
    }
    # 存在確認ののち指定した列を削除
    if (ncol(df_cdisc_sheet_configs) > 0 && all(c("uuid", "created_at", "updated_at") %in% names(df_cdisc_sheet_configs))) {
      df_cdisc_sheet_configs <- df_cdisc_sheet_configs %>%
        select(-all_of(c("uuid","created_at","updated_at")))
    }
    if (ncol(df_cdisc_sheet_configs_pivot) > 0 && all(c("uuid", "created_at", "updated_at") %in% names(df_cdisc_sheet_configs_pivot))) {
      df_cdisc_sheet_configs_pivot <- df_cdisc_sheet_configs_pivot %>%
        select(-all_of(c("uuid","created_at","updated_at")))
    }
  }

  # 'allocation'が存在する場合、そのデータフレームを生成し、列を展開
  # 'allocation'が存在しない場合、NULLをセット
  df_allocation <- NULL
  if ("allocation" %in% names(data)) {
    df_allocation <- data.frame(data$allocation)
    # 'groups'が存在する場合、そのデータを展開
    if ("groups" %in% names(df_allocation) && is.list(df_allocation$groups)) {
      df_allocation <- df_allocation %>%
        unnest(cols = groups, names_sep = "_")
    }
  }

  # 'cdisc_domain_configs'が存在する場合、そのデータフレームを生成し、列を展開
  # 'cdisc_domain_configs'が存在しない場合、NULLをセット
  df_cdisc_domain_configs <- NULL
  if ("cdisc_domain_configs" %in% names(data$trial)) {
    df_cdisc_domain_configs <- data.frame(data$trial$cdisc_domain_configs)
    # 'variables'が存在する場合、そのデータを展開
    if ("variables" %in% names(df_cdisc_domain_configs) && is.list(df_cdisc_domain_configs$variables)) {
      df_cdisc_domain_configs <- df_cdisc_domain_configs %>%
        unnest(cols = variables, names_sep = "_")
      # 不要な列を除外する
      df_cdisc_domain_configs <- df_cdisc_domain_configs %>%
        select(-all_of(c("uuid", "created_at", "updated_at")))
    }
  }

    # エクセルファイル名を作成
  excel_file_name <- tools::file_path_sans_ext(basename(json_file))
  excel_file_path <- file.path(output_path, paste0(excel_file_name, ".xlsx"))

  # 空のデータフレームリストを作成
  data_to_export <- list()

  # 'df_field_items'が存在する場合
  if (is.data.frame(df_field_items)) {
    df_field_items <- df_field_items %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Field_Items <- df_field_items

　  # 縦結合
    # 列名が一致する列にデータを追加。一致しない列は新しい列が作成され存在しないデータはNAで埋められる
    df_field_items_sum <- bind_rows(df_field_items_sum, df_field_items)
  }

  # 'df_option'が存在する場合
  if (is.data.frame(df_option)) {
    df_option <- df_option %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Option <- df_option
    df_option_sum <- bind_rows(df_option_sum, df_option)
  }

  # 'df_flip_flops'が存在する場合
  if (is.data.frame(df_flip_flops)) {
    df_flip_flops <- df_flip_flops %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Flip_Flops <- df_flip_flops
    df_flip_flops_sum <- bind_rows(df_flip_flops_sum, df_flip_flops)
  }

  # 'df_cdisc_sheet_configs'が存在する場合
  if (is.data.frame(df_cdisc_sheet_configs)) {
    df_cdisc_sheet_configs <- df_cdisc_sheet_configs %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Cdisc_Sheet_Configs <- df_cdisc_sheet_configs
  }
  if (is.data.frame(df_cdisc_sheet_configs_pivot)) {
    df_cdisc_sheet_configs_pivot <- df_cdisc_sheet_configs_pivot %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Cdisc_Sheet_Configs_Pivot <- df_cdisc_sheet_configs_pivot
  }

  # 'df_allocation'が存在する場合
  if (is.data.frame(df_allocation)) {
    df_allocation <- df_allocation %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Allocation <- df_allocation
    df_allocation_sum <- rbind(df_allocation_sum,df_allocation)
  }

  # 'df_cdisc_domain_configs'が存在する場合
  if (is.data.frame(df_cdisc_domain_configs)) {
    # 一列目にファイル名入れる
    df_cdisc_domain_configs <- df_cdisc_domain_configs %>%
      mutate(jpname = jp_name, alias_name = crf_name) %>%
      select(jpname, alias_name, everything())
    data_to_export$Cdisc_Domain_Configs <- df_cdisc_domain_configs
  }

  # 結果の書き出し
  write_xlsx(data_to_export, excel_file_path)
}

# バリデーション用の表を作成
# name
df_name <- df_crfinfo

# item
df_item <- df_field_items_sum %>%
  select(jpname, alias_name, name, label, option.name,
         default_value,
         validators.presence.validate_presence_if,
         validators.formula.validate_formula_if, validators.formula.validate_formula_message,
         validators.date.validate_date_after_or_equal_to, validators.date.validate_date_before_or_equal_to)

column_name <- "validators.presence.validate_presence_if"
references_data <- extract_references(df_item, column_name, df_field_items_sum)
df_item <- df_item %>%
  mutate(presence_if_references = references_data) %>%
  relocate(presence_if_references, .after = all_of(column_name))
column_name <- "validators.formula.validate_formula_if"
references_data <- extract_references(df_item, column_name, df_field_items_sum)
df_item <- df_item %>%
  mutate(formula_if_references = references_data) %>%
  relocate(formula_if_references, .after = all_of(column_name))
column_name <- "validators.date.validate_date_after_or_equal_to"
references_data <- extract_references(df_item, column_name, df_field_items_sum)
df_item <- df_item %>%
  mutate(references_after = references_data) %>%
  relocate(references_after, .after = all_of(column_name))
column_name <- "validators.date.validate_date_before_or_equal_to"
references_data <- extract_references(df_item, column_name, df_field_items_sum)
df_item <- df_item %>%
  mutate(references_before = references_data) %>%
  relocate(references_before, .after = all_of(column_name))

# # item
# df_item <- df_field_items_sum %>%
#   filter(type == "FieldItem::Article") %>%
#   select(jpname, alias_name, name, label)

 # option
 #df_option <- df_option_sum
 df_option <- df_option_sum %>%
   filter(option.values_is_usable == TRUE) %>%
   select(jpname, alias_name, option.name, option.values_name, option.values_seq, option.values_code, option.values_is_usable)

# # default
# df_default <- df_field_items_sum %>%
#   filter(
#     type == "FieldItem::Article" &
#     !is.na(default_value)
#   ) %>%
#   select(jpname, alias_name, name, label, default_value)
#
# # conditional
# df_conditional <- df_field_items_sum %>%
#   filter(
#     type == "FieldItem::Article" &
#     (!is.na(validators.presence.validate_presence_if) & validators.presence.validate_presence_if !="")
#   ) %>%
#   select(jpname, alias_name, name, label, validators.presence.validate_presence_if)
# # 関数の呼び出し
# column_name <- "validators.presence.validate_presence_if"
# references_data <- extract_references(df_conditional, column_name, df_field_items_sum)
# df_conditional <- df_conditional %>%
#   mutate(references = references_data) %>%
#   relocate(references, .after = all_of(column_name))


# visit
df_visit <- df_field_items_sum %>%
  filter(label == "Visit Number") %>%
  select(jpname, alias_name, name, default_value)

# # logic
# df_logic <- df_field_items_sum %>%
#   filter(!is.na(validators.formula.validate_formula_if) | !is.na(validators.formula.validate_formula_message)) %>%
#   select(jpname, alias_name, name, label, validators.formula.validate_formula_if, validators.formula.validate_formula_message)
# # 関数の呼び出し
# column_name <- "validators.formula.validate_formula_if"
# references_data <- extract_references(df_logic, column_name, df_field_items_sum)
# df_logic <- df_logic %>%
#   mutate(references = references_data) %>%
#   relocate(references, .after = all_of(column_name))

# number
df_number <- df_field_items_sum %>%
  filter(
    (!is.na(validators.numericality.validate_numericality_less_than_or_equal_to) & validators.numericality.validate_numericality_less_than_or_equal_to !="") |
    (!is.na(validators.numericality.validate_numericality_greater_than_or_equal_to) & validators.numericality.validate_numericality_greater_than_or_equal_to !="")
  ) %>%
  select(jpname, alias_name, name, label, default_value, validators.numericality.validate_numericality_less_than_or_equal_to, validators.numericality.validate_numericality_greater_than_or_equal_to)

# # date
# df_date <- df_field_items_sum %>%
#   filter(
#     field_type == "date" &
#       !is.na(validators.date.validate_date_after_or_equal_to) | !is.na(validators.date.validate_date_before_or_equal_to)
#   ) %>%
#   select(jpname, alias_name, name, label, validators.date.validate_date_after_or_equal_to, validators.date.validate_date_before_or_equal_to)
# # 関数の呼び出し
# column_name <- "validators.date.validate_date_after_or_equal_to"
# references_data <- extract_references(df_date, column_name, df_field_items_sum)
# df_date <- df_date %>%
#   mutate(references_after = references_data) %>%
#   relocate(references_after, .after = all_of(column_name))
# # 関数の呼び出し
# column_name <- "validators.date.validate_date_before_or_equal_to"
# references_data <- extract_references(df_date, column_name, df_field_items_sum)
# df_date <- df_date %>%
#   mutate(references_before = references_data) %>%
#   relocate(references_before, .after = all_of(column_name))

# master
df_master <- df_field_items_sum %>%
  filter(
    !is.na(link_type) & link_type !=""
  ) %>%
  select(jpname, alias_name, name, label, link_type)

# alert
df_alert <- df_field_items_sum %>%
  filter(!is.na(normal_range.less_than_or_equal_to) | !is.na(normal_range.greater_than_or_equal_to)) %>%
  select(jpname, alias_name, name, label, normal_range.less_than_or_equal_to, normal_range.greater_than_or_equal_to)

# action
df_action <- df_flip_flops_sum %>%
  select(jpname, alias_name, id, field_item_id, codes, fields) %>%
  left_join(df_field_items_sum %>%
  select(alias_name, id, name, label),
        by = c("alias_name", "field_item_id" = "id")) %>%
  rename(field_item_id.label = label,
        field_item_id.name = name) %>%
  select(jpname, alias_name, id, field_item_id, field_item_id.name, field_item_id.label, codes, fields)

df_action <- df_action %>%
  left_join(df_field_items_sum %>%
  select(alias_name, name, label),
        by = c("alias_name", "fields" = "name")) %>%
  rename(fields.label = label) %>%
  select(jpname, alias_name, id, field_item_id, field_item_id.name, field_item_id.label, codes, fields, fields.label)


# allocation
df_alloc <- df_allocation_sum %>%
  select(jpname, alias_name, is_zelen, zelen_imbalance, is_double_blinded, double_blind_emails, allocation_method, groups.code, groups.label, groups.if, groups.message)
 # 関数の呼び出し
   column_name <- "groups.if"
   references_data <- extract_references(df_alloc, column_name, df_field_items_sum)
   df_alloc <- df_alloc %>%
   mutate(references = references_data) %>%
   relocate(references, .after = all_of(column_name))

# presence
df_presence <- df_field_items_sum %>%
  filter(
    type == "FieldItem::Article" & validators.presence == "FALSE"
  ) %>%
  select(jpname, alias_name, name, label)

# display
df_display <- df_field_items_sum %>%
  filter(
    (type == "FieldItem::Assigned" & is_invisible =="FALSE") |
    (type == "FieldItem::Article" & is_invisible == "TRUE")
  ) %>%
  select(jpname, alias_name, name, label)


# comment
df_comment <- df_field_items_sum %>%
  filter(!is.na(content)) %>%
  select(jpname, alias_name, name, label, content)

# explanation
df_explanation <- df_field_items_sum %>%
  filter(!is.na(description) & description !="") %>%
  select(jpname, alias_name, name, label, description)

# title
df_titile <- df_field_items_sum %>%
  filter(type == "FieldItem::Heading") %>%
  select(jpname, alias_name, name, label, level)

# assigned
df_assigned <- df_field_items_sum %>%
  filter(type == "FieldItem::Assigned") %>%
  select(jpname, alias_name, name, label, default_value)


# データが存在するかどうかを確認
if (nrow(df_crfinfo) > 0) {
    # 'list'フォルダのフルパスを作成し、存在しない場合はフォルダを作成
    list_folder_path <- file.path(output_path, "list")
    if (!dir.exists(list_folder_path)) dir.create(list_folder_path)
    # ファイルのフルパスを作成
    file_path <- file.path(list_folder_path, "checklist.xlsx")
    # データフレームをExcelファイルに書き出す
    df_list <- list(
      name = df_name,
      item = df_item,
      option = df_option,
#      default = df_default,
#      conditional = df_conditional,
      visit = df_visit,
#      logic = df_logic,
      number = df_number,
#      date = df_date,
      master = df_master,
      alert = df_alert,
      action = df_action,
      allocation = df_alloc,
      presence = df_presence,
      display = df_display,
      comment = df_comment,
      explanation = df_explanation,
      title = df_titile,
      assigned = df_assigned,
      fielditems_sum = df_field_items_sum,
      option_sum = df_option_sum,
      allocation_sum = df_allocation_sum
    )
    write_xlsx(df_list, path = file_path)
}
