# Show just the variables that are used in a given file in a given year
show_variable_names <- function(file_name, year){
  def_variables <- read_excel("data/CE_data/ce-pumd-interview-diary-dictionary.xlsx", sheet = 2)
  file_name <- toupper(file_name)

  table_of_variables <- def_variables |>
    filter(`First year` <= year) |>
    filter(`Last year` >= year | is.na(`Last year`)) |>
    filter(`File` == file_name) |>
    select(`Variable Name`,
           `Variable description`,
           Formula,
           `Flag name`)

  return(table_of_variables)
}

# Filter the definition sheet by file name and year pertinence
filter_definition_data <- function(def_data_frame, file_name = NULL, year = 2023){


  filtered_table <- def_data_frame |>
    filter(`First year` <= year) |>
    filter(`Last year` >= year | is.na(`Last year`))

  if (is.null(file_name)){
    return(filtered_table)
  } else{
    file_name <- toupper(file_name)
    filtered_table <- filtered_table |>
      filter(`File` == file_name)
    return(filtered_table)
  }
}

# use the code dictionary to upudate the coded cell values in the data frame
decode_cells <- function(df, code_dictionary_df){
  # Your original data frame with different codes in different columns
  # Make them all characters
  df[] <- lapply(df, as.character)

  # Your definitions data frame
  # make them characters and create easier col names
  code_dictionary_df[] <- lapply(code_dictionary_df, as.character)
  def_codes_selected <- code_dictionary_df |>
    select(Variable, value = `Code value`, description = `Code description`)

  # loop through each row and column, look it up in the def_codes, and replace the code
  for (row in 1:nrow(df)){
    for (col in 1:ncol(df)){
      variable_name <- names(df)[[col]]
      code_value <- df[[row,col]]

      code_definition_df <- def_codes_selected |>
        filter(Variable == variable_name,
               value == code_value)

      if (nrow(code_definition_df) == 0){
        next
      } else{
        code_definition <- code_definition_df[[1,3]]
        df[[row,col]] <- code_definition
      }
    }
  }

  return(df)
}

# take the file path of definitions excel and the folder path to the data
# and make a csv with the descriptions of the col names
create_col_name_csv <- function(def_file_path, data_folder_path){
  def_variables_df <- read_excel(def_file_path, sheet = 2) |>
    mutate(repeated = duplicated(`Variable Name`)) |>
    filter(repeated == FALSE)

  def_variables_df_changed <- def_variables_df |>
    mutate(`Variable Name` = paste0(`Variable Name`, "_")) |>
    mutate(`Variable description` = paste(`Variable description`, "(flag)"))

  comb_def_variables <- rbind(def_variables_df, def_variables_df_changed)

  data_folder <- list.files(data_folder_path, full.names = T)

  save_dir <- paste0(data_folder_path, "/column_descriptions")
  dir.create(save_dir)
  for (file in data_folder){
    file_name <- basename(file)
    new_file_name <- paste0(strsplit(file_name,"[.]")[[1]][[1]], "_col_descr.csv")
    df <- read_csv(file)
    col_names_df <- data.frame(col_names_old = names(df))
    col_desc_df <- left_join(col_names_df, comb_def_variables, by = join_by(col_names_old == `Variable Name`)) |>
      select(col_names_old, description = `Variable description`)
    # now save it to a common folder.
    write_csv(col_desc_df, paste0(save_dir, "/",new_file_name))
  }
}

# Use the manually edited file to change the col names of the data
get_wanted_cols <- function(df, col_desc_file_path){
  col_df <- read_csv(col_desc_file_path, col_types = "c") |>
    filter(!is.na(keep)) |>
    mutate(changed_col_name = ifelse(is.na(col_name_new),col_names_old, col_name_new))

  df <- df |>
    select(any_of(col_df$col_names_old))

  names(df) <- col_df$changed_col_name

  return(df)
}

# use NEWID to create other a unique member_IDs
create_IDs <- function(df_with_NEWID){
  added_ids <- df_with_NEWID |>
    mutate(HH_ID = substring(NEWID,1,nchar(NEWID) - 1),
           WEEK_ID = substring(NEWID,nchar(NEWID),nchar(NEWID))) |>
    group_by(NEWID) |>
    mutate(MEM_NUM = row_number()) |>
    ungroup() |>
    mutate(MEM_ID = paste0(HH_ID,MEM_NUM)) |>
    mutate(ID = paste0(NEWID,MEM_NUM))

  return(added_ids)
}
