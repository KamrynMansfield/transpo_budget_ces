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

create_probs_table <- function(model, percent = FALSE, lcm_data){
  mult <- ifelse(percent, 100, 1)
  rnd <- ifelse(percent, 2, 4)
  
  df_totals <- matrix(model$P * mult) |>
    round(rnd) |>
    t() |>
    as.data.frame()
  
  names(df_totals) <- paste0("class_",c(1:ncol(df_totals)))
  df_totals$variable <- "Class Probability"
  
  probs <- model$probs
  df_list <- list()
  for (var_name in names(probs)){
    mtx <- probs[[var_name]]
    
    df <- as.data.frame(round(t(mtx * mult),rnd))
    
    names(df) <- paste0("class_",c(1:ncol(df)))
    row.names(df) <- NULL
    
    df$variable <- var_name
    df$level <- 1:nrow(df)
    
    df_list[[var_name]] <- df
  }
  
  combined_df <- bind_rows(df_list)
  combined_df <- bind_rows(df_totals, combined_df)
  
  level_meaning <- "NA"
  for (row_num in 2:nrow(combined_df)){
    
    col_str <- gsub("1","",combined_df[[row_num,"variable"]])
    new_df <- lcm_data[lcm_data[[paste0(col_str,"1")]] == combined_df[[row_num,"level"]],grep(col_str,names(lcm_data))]
    meaning <- new_df[[gsub("1","",names(new_df)[[1]])]][[1]] |> as.character()
    
    level_meaning <- c(level_meaning, meaning)
  }
  
  combined_df$level_def <- level_meaning
  
  combined_df <- combined_df |>
    dplyr::select(!level) |>
    dplyr::select(any_of(c("variable","level_def",names(combined_df))))
  
  return(combined_df)
}

create_regr_tbl <- function(model){
  
  coeffs <- model$coeff
  se <- model$coeff.se
  tval <- coeffs / se
  
  df <- data.frame(exogenous_var = row.names(coeffs))
  df[[paste0("coeff_",1)]] <- round(coeffs[,1], 3)
  df[[paste0("t_stat_",1)]] <- round(tval[,1], 3)
  
  if (ncol(coeffs) < 2){
    return(df)
  }
  
  for (i in 2:ncol(coeffs)){
    new_df <- data.frame(exogenous_var = row.names(coeffs))
    new_df[[paste0("coeff_",i)]] <- round(coeffs[,i], 3)
    new_df[[paste0("t_stat_",i)]] <- round(tval[,i], 3)
    
    df <- left_join(df, new_df, by = "exogenous_var")
  }
  
  return(df)
  
}

# create a nice looking table of the model coefficients
# model_df is created from create_regr_tbl() function
# and exog_name_key is a df with the exogenous variable names and their categories
create_nice_coeff_tbl <- function(model_df, 
                                  exog_name_key,
                                  highlighted = TRUE,
                                  highlight_color = "#FF8200",
                                  t_threshold = 1.4,
                                  only_significant = FALSE,
                                  two_vs_one_name = "Class 2 vs. Class 1",
                                  three_vs_one_name = "Class 3 vs. Class 1",
                                  row_group_order = NULL){
  
  model_df <- left_join(exog_name_key, model_df, by = "exogenous_var") |>
    dplyr::mutate(coeff_1 = paste0(coeff_1, " (",t_stat_1,")"),
                  coeff_2 = paste0(coeff_2, " (",t_stat_2,")"))
  
  
  if (only_significant){
    model_gt <- model_df |>
      dplyr::mutate(coeff_1 = ifelse(t_stat_1 >= t_threshold | t_stat_1 <= -t_threshold,coeff_1, "-"),
                    coeff_2 = ifelse(t_stat_2 >= t_threshold | t_stat_2 <= -t_threshold,coeff_2, "-")) |>
      dplyr::select(category, exogenous_var_name, coeff_1, t_stat_1, coeff_2, t_stat_2) |>
      gt::gt(rowname_col = "exogenous_var_name",
         groupname_col = "category") |>
      gt::tab_footnote(footnote = "Dashes indicate an insignificant result")
    
  } else{
    model_gt <- model_df |>
      dplyr::select(category, exogenous_var_name, coeff_1, t_stat_1, coeff_2, t_stat_2) |>
      gt::gt(rowname_col = "exogenous_var_name",
         groupname_col = "category")
  }
  
  
  
  if (highlighted){
    model_gt <- model_gt |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = highlight_color), # Highlights the cell background
          gt::cell_text(weight = "bold")     # Makes the text bold
        ),
        locations = gt::cells_body(
          columns = coeff_1,                   # Target the 'hp' column
          rows = t_stat_1 >= t_threshold | t_stat_1 <= -t_threshold              # Condition: highlight cells where hp is > 150
        )
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = highlight_color), # Highlights the cell background
          gt::cell_text(weight = "bold")     # Makes the text bold
        ),
        locations = gt::cells_body(
          columns = coeff_2,                   # Target the 'hp' column
          rows = t_stat_2 >= t_threshold | t_stat_2 <= -t_threshold            # Condition: highlight cells where hp is > 150
        )
      ) |>
      gt::tab_footnote(footnote = "Statistically significant cells are highlighted")
  }
  
  model_gt <- model_gt |>
    gt::cols_hide(columns = c(t_stat_1, t_stat_2)) |>
    gt::cols_align(
      align = "center",
      columns = c(coeff_1, coeff_2)) |>
    gt::cols_label(
      coeff_1 = two_vs_one_name,
      coeff_2 = three_vs_one_name) |>
    gt::opt_row_striping(row_striping = FALSE)
  
  if (!is.null(row_group_order)){
    
    model_gt <- model_gt  |>
      row_group_order(groups = row_group_order)
  }
  
  return(model_gt)
}

# this taks in the model and two dfs
# a df with the names of the expense variable categories and
# a df with the names of the expense variable levels
create_nice_probs_table <- function(model, 
                                    expense_variable_names, 
                                    expense_level_names, 
                                    lcm_data,
                                    c1_name = "Class 1",
                                    c2_name = "Class 2",
                                    c3_name = "Class 3",
                                    row_group_order = NULL){
  
  probs_df <- create_probs_table(model, lcm_data = lcm_data) |> 
    dplyr::mutate(class_1 = round(class_1 * 100,2),
           class_2 = round(class_2 * 100,2),
           class_3 = round(class_3 * 100,2)) |>
    dplyr::left_join(expense_variable_names, by = "variable") |>
    dplyr::left_join(expense_level_names, by = "level_def") |>
    dplyr::select(nice_var_name, nice_level_name, class_1, class_2, class_3)
  
  final_table <- probs_df |>
    gt::gt(rowname_col = "nice_level_name",
       groupname_col = "nice_var_name") |>
    gt::cols_label(
      class_1 = c1_name,
      class_2 = c2_name,
      class_3 = c3_name)
  
  if (!is.null(row_group_order)){
    final_table <- final_table |>
      row_group_order(row_group_order)  
  }
  
  return(final_table)
}

# Define a function for weighted mean and variance
weighted_summary <- function(data, weights) {
  weighted_mean <- sum(data * weights) / sum(weights)
  weighted_var <- sum(weights * (data - weighted_mean)^2) / sum(weights)
  c(mean = weighted_mean, variance = weighted_var)
}

# takes the model results, the lcm_data, and a vector of the covariate variable names
# and creates a list of data frames for each class
get_covariate_pcts <- function(lcm_results, lcm_data, covariates){
  posterior_probs <- lcm_results$posterior
  lcm_data_with_probs <- cbind(lcm_data, posterior_probs)
  lcm_data_with_probs$assigned_class <- apply(posterior_probs, 1, which.max)
  
  # Initialize a list to store results
  class_summaries <- list()
  
  # Loop through each class
  for (class in 1:3) {
    # Subset data for the current class
    weights <- posterior_probs[, class]
    
    # Compute weighted summary for each covariate
    summaries <- sapply(covariates, function(cov) {
      weighted_summary(as.numeric(lcm_data_with_probs[[cov]]), weights)
    })
    
    # Store results
    class_summaries[[class]] <- summaries
  }
  
  # Convert results to a more readable format
  class_summaries_df <- lapply(class_summaries, function(x) {
    t(as.data.frame(x))
  })
  names(class_summaries_df) <- paste("Class", 1:3)
  
  return(class_summaries_df)
  
}

# takes the same inputs as get_covariates_pcts and just makes a nice-looking table
create_nice_covariate_tbl <- function(model, lcm_data, covariates){
  
  covariate_pcts <- get_covariate_pcts(model, lcm_data, covariates)
  
  combined_cov_pcts <- as.data.frame(covariate_pcts)
  combined_cov_pcts$exogenous_var <- row.names(combined_cov_pcts)
  
  combined_cov_pcts <- combined_cov_pcts |> 
    left_join(exog_name_key, by = "exogenous_var") |>
    select(!exogenous_var)
  
  
  gt_tbl <- combined_cov_pcts |>
    mutate(Class.1.mean = round(Class.1.mean, 3),
           Class.2.mean = round(Class.2.mean, 3),
           Class.3.mean = round(Class.3.mean, 3)) |>
    select(category,exogenous_var_name, Class.1.mean, Class.2.mean, Class.3.mean) |>
    gt(rowname_col = "exogenous_var_name",
       groupname_col = "category",
       row_group_as_column = TRUE)
  
  return(gt_tbl)
}