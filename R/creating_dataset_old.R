library(tidyverse)
library(readxl)

#### I created some functions to help ###################################################

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


#### Input Files #####################################################################################

# here are all the files we are working with
## CHANGE THESE TO THE PROPER PATHS ##

expd_csv_path <- "data/diary24_combined/expd.csv"
fmld_csv_path <- "data/diary24_combined/fmld.csv"
col_def_csv_path <- "data/data_prep_input/fmld_col_descr.csv"
pumd_dictionary_xlsx_path <- "data/data_prep_input/ce-pumd-interview-diary-dictionary.xlsx"
ucc_categories_xlsx_path <- "data/data_prep_input/ucc_categories.xlsx"
binary_columns_xlsx_path <- "data/data_prep_input/fmld_col_binary.csv"



# expd.csv file
# This is the detailed expenditure and non-expenditure data
# All four quarters have been combined into one file for the year
d_exp <- read_csv(expd_csv_path)

# fmld.csv file
# This is the data with Consumer Unit Demographics and summary expenditures
# All four quarters have been combined into one file for the year
d_family <- read_csv(fmld_csv_path)

# our file with the column names of the fmld file and whether we want to keep them
# must only have column names = c("col_names_old", "description","keep","col_name_new")
# only the demographic columns can be kept
fam_col_defs <- read_csv(col_def_csv_path) |>
  filter(!is.na(keep))

# definitions files
# these come from the CEX dictionary excel file
def_variables <- read_excel(pumd_dictionary_xlsx_path, sheet = 2)
def_codes <- read_excel(pumd_dictionary_xlsx_path, sheet = 3)
def_variables_filtered <- filter_definition_data(def_variables, year = 2023) # get only they variables pertenent to 2023
def_codes_filtered <- filter_definition_data(def_codes, year = 2023) # get only they codes pertinent to 2023

# ucc categories
categories_df <- read_excel(ucc_categories_xlsx_path)


#### Prep Demographic Data ###########################################################################

d_family <- d_family |>

  # Filter to just the CUs that were given the survey twice
  filter(WEEKN == 2) |>

  # select only the columns we want to work with
  select(fam_col_defs$col_names_old) |>

  # filter so only one row per CU
  mutate(duplicate = duplicated(CUID)) |>
  filter(duplicate == FALSE)


# decode the cell values
# this is not a necessary step, but it helped me look at the data better
# d_family_decoded <- decode_cells(d_family, def_codes_filtered)

#### Widen Demographic Data ###########################################################################

# get a csv with just the code descriptions of the columns we have in our family data set.
# this will help us organize a file to create the binary column names and calculations
# def_codes_filtered |> filter(File == "FMLD") |>
#   filter(Variable %in% names(d_family_decoded)) |>
#   write_csv("data/CE_data/2023/diary23_combined/column_creation_binary/filtered_codes_fmld_cols.csv")

# this is the file that helps create the binary data
binary_reference_data <- read_csv(binary_columns_xlsx_path) |>
  filter(!is.na(col_name))

d_family_binary <- d_family

# this loops through each of the rows in the binary
# data to create a new column in the data frame
for (row_num in 1:nrow(binary_reference_data)){
  new_col_name <- binary_reference_data$col_name[[row_num]]
  variable_col <- binary_reference_data$Variable[[row_num]]
  value <- binary_reference_data$`Code value`[[row_num]]
  inequality <- binary_reference_data$inequality[[row_num]]
  connector <- binary_reference_data$connector[[row_num]]
  inequality_extra <- binary_reference_data$inequality_extra[[row_num]]
  value_extra <- binary_reference_data$value_extra[[row_num]]

  d_family_binary[[variable_col]] <- as.numeric(d_family_binary[[variable_col]])

  # if it has and or or
  if (!is.na(connector)){
    if (connector == "or"){
      expression <- paste(variable_col,inequality,value, "|",variable_col, inequality_extra, value_extra)
    }

    if (connector == "and"){
      expression <- paste(variable_col,inequality,value, "&",variable_col, inequality_extra, value_extra)
    }

    # use expression to create the new binary column
    d_family_binary <- d_family_binary |>
      mutate(new_col = ifelse(eval(parse(text = expression)), 1, 0))

  } else if (!is.na(value)){
    expression <- paste(variable_col,inequality,value)

    # use expression to create the new binary column
    d_family_binary <- d_family_binary |>
      mutate(new_col = ifelse(eval(parse(text = expression)), 1, 0))
  } else{
    # duplicate the column as is
    d_family_binary <- d_family_binary |>
      mutate(new_col = d_family_binary[[variable_col]])
  }

  # change the name of the new column to the one we wanted
  col_names_update <- names(d_family_binary)
  col_names_update[[length(names(d_family_binary))]] <- new_col_name
  names(d_family_binary) <- col_names_update

  # add a column to record NA values (I don't think I need this anymore)
  # d_family_binary[[paste0(new_col_name, "_NA")]] <- ifelse(is.na(d_family_binary[[variable_col]]), 1,0)
}

d_family_binary <- d_family_binary |>
  select(CUID,binary_reference_data$col_name)

#### Prep Expenditure Data ###########################################################################

# group expenditure data
d_exp <- d_exp |>

  # separate the NEWID to get CUID and WEEKI
  mutate(CUID = substr(NEWID,2,7),
         WEEKI = substr(NEWID,8,8)) |>

  # group by each week, sum the costs
  group_by(CUID, UCC) |>
  summarize(COST = sum(COST))

# get categories file ready
categories_df <- categories_df |>

  # filter to only the ucc codes in expenditure data
  filter(ucc_code %in% d_exp$UCC) |>

  # select just the UCC and category columns to make joining easier
  select(UCC = ucc_code,
         category)

# group by category
d_exp_categories <- d_exp |>

  # join to add the category column
  left_join(categories_df) |>

  # group by category and sum the costs
  group_by(CUID, category) |>
  summarize(COST = sum(COST)) |>

  # pivot wider to have a col for each category
  pivot_wider(names_from = category, values_from = COST)

#### Join The Data Frames ###########################################################################

# make sure the CUID column are the same classes
d_exp_categories$CUID <- as.numeric(d_exp_categories$CUID)
d_family_binary$CUID <- as.numeric(d_family_binary$CUID)
# join the datasets by CUID
organized_data <- left_join(d_exp_categories, d_family_binary, by = "CUID")

# get rid of any rows with no transportation expenses
organized_data <- organized_data |>
  filter(!((`Car Fuel` == 0 | is.na(`Car Fuel`)) &
             (`Car Maintenance` == 0 | is.na(`Car Fuel`)) &
             (`Car Purchase` == 0 | is.na(`Car Purchase`)) &
             # (`Car Other` == 0 | is.na(`Car Other`)) &
             (`Car Payment` == 0 | is.na(`Car Payment`)) &
           (`Taxi or Limo Travel` == 0 | is.na(`Taxi or Limo Travel`)) &
           (`Bus Travel` == 0 | is.na(`Bus Travel`)) &
           (`Train Travel` == 0 | is.na(`Train Travel`)) &
           (`Air Travel` == 0 | is.na(`Air Travel`)) &
           (`Bike or Scooter or other Single Rider Travel` == 0 | is.na(`Bike or Scooter or other Single Rider Travel`))))

# getting rid of rows with no demographic data
empty_rows <- c()
for (i in 1:nrow(organized_data)){
  row_data <- as.numeric(organized_data[i,])
  demographic_nas <- is.na(row_data[20:length(row_data)])
  if (sum(demographic_nas) == length(demographic_nas)){
    empty_rows <- c(empty_rows, i)
  }
}

organized_data <- organized_data[!(1:nrow(organized_data)) %in% empty_rows,]

# getting rid of zeros
organized_data <- replace(organized_data, is.na(organized_data), 0)



# save to a file
save_to_path <- "data/diary24_combined/created_data/exp_fmly_data.csv"
write_csv(organized_data, save_to_path)


