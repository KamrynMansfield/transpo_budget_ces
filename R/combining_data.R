library(tidyverse)
library(readxl)

# takes the folder with CES diary files and combines each quarter
# outputs a list of the combined data frames for each table type
combine_ce_files <- function(folder_path, save = FALSE, save_to_folder = NULL){
  full_files_listed <- list.files(folder_path, full.names = T, pattern = "\\.csv$")
  file_names_listed <- list.files(folder_path, pattern = "\\.csv$")

  unique_names <- gsub("\\d+|\\.csv","",file_names_listed) |>
    unique()

  combined_df_list <- list()
  for (i in 1:length(unique_names)){
    name <- unique_names[[i]]
    grouped_files <- full_files_listed[grep(name,full_files_listed)]

    listed_dfs <- lapply(grouped_files, function(x) read_csv(x, guess_max = Inf, col_types = cols(.default = col_character())))

    grouped_dfs <- bind_rows(listed_dfs)

    combined_df_list[[name]] <- grouped_dfs
  }

  if (save){

    if (is.null(save_to_folder)){
      current_folder_name <- basename(folder_path)
      new_folder_name <- paste0(current_folder_name, "_combined")
      new_folder_path <- paste0(dirname(folder_path),"/",new_folder_name)
      save_to_folder <- new_folder_path
    }

    if (!file.exists(save_to_folder)){
      dir.create(save_to_folder)
    }

    for (df_num in 1:length(combined_df_list)){
      df <- combined_df_list[[df_num]]
      df_name <- names(combined_df_list)[[df_num]]
      save_file <- paste0(save_to_folder,"/",df_name,".csv")
      write_csv(df, save_file)
    }
    cat(paste0(length(combined_df_list)," files saved to folder:\n",save_to_folder))
  } else{
    return(combined_df_list)
  }

}

combine_ce_files(folder_path = "data/CE_data/2024/diary24",
                 save = FALSE)

combine_ce_files(folder_path = "data/CE_data/2024/diary24",
                 save = TRUE)

combine_ce_files(folder_path = "data/CE_data/2024/intrvw24",
                 save = TRUE)


## take in data with a ucc variable and filter them
# - ce_data has to be a list of dataframes
# - ucc_data must be a file path to a csv stating the ucc values and the categories
filter_by_ucc <- function(ce_data, ucc_data, save = FALSE, save_to_folder = NULL){
  ucc <- read_csv(ucc_data)
  class(ucc$ucc_code) <- "numeric"

  filtered_data_list <- list()
  warning <- 0
  empty <- 0
  for (i in 1:length(ce_data)){
    ce_df <- ce_data[[i]]
    ce_df_name <- names(ce_data)[[i]]

    if ("UCC" %in% names(ce_df)){
      class(ce_df$UCC) <- "numeric"
      filtered_data <- ce_df |>
        filter(UCC %in% ucc$ucc_code)

      filtered_data <- filtered_data |>
        left_join(ucc,
                  by = join_by(UCC == ucc_code),
                  relationship = "many-to-many")

      if (nrow(filtered_data) == 0){
        empty <- empty + 1
      }

    } else{
      filtered_data <- ce_df
      warning <- warning + 1
    }

    filtered_data_list[[ce_df_name]] <- filtered_data

  }

  if (warning > 0 & empty > 0){
    message(paste0(warning, " data frames left as is with no UCC column. ",empty," data frames with no matching UCC values"))
  } else if (warning > 0){
    message(paste0(warning, " data frames left as is with no UCC column."))
  } else if (empty > 0){
    message(paste0(empty," data frames returned empty because no UCC values match"))
  }

  if (save){

    if (is.null(save_to_folder)){
      current_folder_name <- dirname(ucc_data)
      new_folder_name <- "filtered_by_ucc"
      new_folder_path <- paste0(current_folder_name,"/",new_folder_name)
      save_to_folder <- new_folder_path
    }

    if (!file.exists(save_to_folder)){
      dir.create(save_to_folder)
    }

    saved_files <- 0
    for (df_num in 1:length(filtered_data_list)){
      df <- filtered_data_list[[df_num]]

      if (nrow(df) > 0){
        df_name <- names(filtered_data_list)[[df_num]]
        save_file <- paste0(save_to_folder,"/",df_name,".csv")
        write_csv(df, save_file)
        saved_files <- saved_files + 1
      }
    }
    cat(paste0(saved_files," files saved to folder:\n",save_to_folder))
  } else{
    return(filtered_data_list)
  }


}

# combined_diary_dfs <- combine_ce_files("data/CE_data/2023/diary23")
# combined_intrvw <- combine_ce_files("data/CE_data/2023/intrvw23")

filter_by_ucc(ce_data = combined_diary_dfs,
              ucc_data = "data/CE_data/2023/ucc_clothing_filter.csv",
              save = FALSE)

filter_by_ucc(ce_data = combined_diary_dfs,
              ucc_data = "data/CE_data/2023/ucc_clothing_filter.csv",
              save = TRUE,
              save_to_folder = "data/CE_data/2023/diary_filtered")












### finding which UCC codes are in the 2023 data ####
combined_diary_dfs
combined_intrvw
lapply(combined_intrvw, names)
diary_ucc <- c(combined_diary_dfs[[1]]$UCC,
               combined_diary_dfs[[2]]$UCC,
               combined_diary_dfs[[3]]$UCC) |>
  unique()

intrvw_ucc <- c(combined_intrvw[[2]]$UCC,
                combined_intrvw[[3]]$UCC,
                combined_intrvw[[5]]$UCC) |>
  unique()

all_ucc <- c(diary_ucc, intrvw_ucc) |> unique()

ces_ucc_classified <- read_excel("data/CES_UCC_classified.xlsx")

ces_ucc_new_col <- ces_ucc_classified |>
  mutate(in_2023 = ifelse(`UCC CODE` %in% all_ucc,1,0))

# write_csv(ces_ucc_new_col, "data/CES_UCC_updated.csv")
