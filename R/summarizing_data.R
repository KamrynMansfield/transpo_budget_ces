library(tidyverse)
library(readxl)

##### DIARY SURVEY #####
############################

## diary files ##
# memd.csv = member level income and characteristics
d_member <- read_csv("data/CE_data/2024/diary24_combined/memd.csv", guess_max = 100000)
# dtbd.csv = detailed income
d_income <- read_csv("data/CE_data/2024/diary24_combined/dtbd.csv", guess_max = 100000)
# dtid.csv = Income imputation iterations
d_income_itter <- read_csv("data/CE_data/2024/diary24_combined/dtid.csv", guess_max = 100000)
# expd.csv = Detailed expenditure and non-expenditure data
d_exp <- read_csv("data/CE_data/2024/diary24_combined/expd.csv", guess_max = 100000)
# fmld.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
d_family <- read_csv("data/CE_data/2024/diary24_combined/fmld.csv", guess_max = 100000)

# the combined data we put in the model
exp_data_filtered <- read.csv("data/CE_data/2024/diary24_combined/created_data/exp_fmly_data_filtered.csv")

exp_data_full <- read.csv("data/CE_data/2024/diary24_combined/created_data/exp_fmly_data.csv")


nrow(exp_data_filtered)
nrow(exp_data_full)

update_data_for_model <- function(lcm_data){

  lcm_data$FoodAlc = lcm_data$Food.Related + lcm_data$Alcohol
  lcm_data$Leisure = lcm_data$Recreational.Related + lcm_data$At..Home.Entertainment
  lcm_data$School = lcm_data$School.Supplies + lcm_data$Education
  lcm_data$Mandatory = lcm_data$Family.Spending + lcm_data$Hygiene + lcm_data$Medical + lcm_data$Clothing.Related
  lcm_data$Housing = lcm_data$House.Related
  lcm_data$Bike.or.Scooter.Travel = lcm_data$Bike.or.Scooter.or.other.Single.Rider.Travel
  lcm_data$Transpo = (lcm_data$Car.Fuel + lcm_data$Car.Maintenance + lcm_data$Taxi.or.Limo.Travel
                      + lcm_data$Bus.Travel + lcm_data$Train.Travel +lcm_data$Air.Travel + lcm_data$Bike.or.Scooter.Travel)

  quantile(lcm_data$Leisure)
  quantile(lcm_data$FoodAlc)
  quantile(lcm_data$School)
  quantile(lcm_data$Mandatory)
  quantile(lcm_data$Housing)

  quantile(lcm_data$Car.Fuel)
  quantile(lcm_data$Car.Maintenance)


  #Yes Categories
  lcm_data$LeiY = as.integer(factor(ifelse(lcm_data$Leisure>0,1,0)))
  lcm_data$FoodY = as.integer(factor(ifelse(lcm_data$FoodAlc>0,1,0)))
  lcm_data$SchoolY = as.integer(factor(ifelse(lcm_data$School>0,1,0)))
  lcm_data$ManY = as.integer(factor(ifelse(lcm_data$Mandatory>0,1,0)))
  lcm_data$HouseY = as.integer(factor(ifelse(lcm_data$Housing>0,1,0)))
  lcm_data$TranspoY = as.integer(factor(ifelse(lcm_data$Transpo>0,1,0)))
  #lcm_data$carY = as.integer(factor(ifelse(lcm_data$Car.Travel>0,1,0)))
  lcm_data$cargasY = as.integer(factor(ifelse(lcm_data$Car.Fuel>0,1,0)))
  lcm_data$carmaintY = as.integer(factor(ifelse(lcm_data$Car.Maintenance>0,1,0)))
  # lcm_data$carotherY = as.integer(factor(ifelse(lcm_data$Car.Other>0,1,0)))
  lcm_data$taxiY = as.integer(factor(ifelse(lcm_data$Taxi.or.Limo.Travel>0,1,0)))
  lcm_data$busY = as.integer(factor(ifelse(lcm_data$Bus.Travel>0,1,0)))
  lcm_data$trainY = as.integer(factor(ifelse(lcm_data$Train.Travel>0,1,0)))
  lcm_data$airY = as.integer(factor(ifelse(lcm_data$Air.Travel>0,1,0)))
  lcm_data$microY = as.integer(factor(ifelse(lcm_data$Bike.or.Scooter.Travel>0,1,0)))
  sjmisc::frq(x=lcm_data[c("microY")], out="viewer")

  lcm_data$sum = lcm_data$LeiY +lcm_data$SchoolY + lcm_data$ManY + lcm_data$HouseY - 4
  sjmisc::frq(x=lcm_data[c("sum")], out="viewer")

  ##cuts for non transpo
  cutslei <- c(-Inf, 0, 100, Inf)
  labslei <- c("Zero", "HundredB", "HighHunB")
  lcm_data$Lei_cat <- cut(lcm_data$Leisure,
                          breaks = cutslei,
                          labels = labslei,
                          right = TRUE,
                          ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("Lei_cat")], out="viewer")
  lcm_data$Lei_cat1 = as.integer(lcm_data$Lei_cat)

  cutsfood <- c(-Inf, 100, 500, Inf)
  labsfood <- c("OneHudB", "FiveHundredB", "HighFiveHunB")
  lcm_data$Food_cat <- cut(lcm_data$FoodAlc,
                           breaks = cutsfood,
                           labels = labsfood,
                           right = TRUE,
                           ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("Food_cat")], out="viewer")
  lcm_data$Food_cat1 = as.integer(lcm_data$Food_cat)

  cutsman <- c(-Inf, 0, 200, 500, 1000, Inf)
  labsman <- c("Zero", "two", "five", "onehund", "over")
  lcm_data$Man_cat <- cut(lcm_data$Mandatory,
                          breaks = cutsman,
                          labels = labsman,
                          right = TRUE,
                          ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("Man_cat")], out="viewer")
  lcm_data$Man_cat1 = as.integer(lcm_data$Man_cat)

  cutshous <- c(-Inf, 0, 200, 500, 1000, Inf)
  labshous <- c("Zero", "two", "five", "onehund", "over")
  lcm_data$House_cat <- cut(lcm_data$Housing,
                            breaks = cutshous,
                            labels = labshous,
                            right = TRUE,
                            ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("House_cat")], out="viewer")
  lcm_data$House_cat1 = as.integer(lcm_data$House_cat)


  ###chagne v3
  cutsC1 <- c(-Inf, 0, 50, 100, Inf)
  labsC1 <- c("Zero", "FiftyB", "HundredB", "HighHunB")

  cutsC2 <- c(-Inf, 0, 100, Inf)
  labsC2 <- c("Zero", "HundredB", "HighHunB")

  cutsC3 <- c(-Inf, 0, 100, Inf)
  labsC3 <- c("Zero", "HundredB", "HighHunB")

  cutsT <- c(-Inf, 0, 20, 50, Inf)
  labsT <- c("Zero", "TwentyB", "FiftyB", "HighFifB")

  cutsB <- c(-Inf, 0, Inf)
  labsB <- c("Zero", "Any")

  cutsTR <- c(-Inf, 0, 20, Inf)
  labsTR <- c("Zero", "TwentyB", "HighTwB")

  cutsA <- c(-Inf, 0, 500, Inf)
  labsA <- c("Zero", "FiveHB","HighFive")

  cutsM <- c(-Inf, 0, Inf)
  labsM <- c("Zero", "Any")


  lcm_data$carFuelP_cat <- cut(lcm_data$Car.Fuel,
                               breaks = cutsC1,
                               labels = labsC1,
                               right = TRUE,
                               ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("carFuelP_cat")], out="viewer")
  lcm_data$carMainP_cat <- cut(lcm_data$Car.Maintenance,
                               breaks = cutsC2,
                               labels = labsC2,
                               right = TRUE,
                               ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("carMainP_cat")], out="viewer")
  # lcm_data$carOthP_cat <- cut(lcm_data$Car.Other,
  #                          breaks = cutsC3,
  #                          labels = labsC3,
  #                          right = TRUE,
  #                          ordered_result = TRUE)
  # sjmisc::frq(x=lcm_data[c("carOthP_cat")], out="viewer")
  lcm_data$taxiP_cat <- cut(lcm_data$Taxi.or.Limo.Travel,
                            breaks = cutsT,
                            labels = labsT,
                            right = TRUE,
                            ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("taxiP_cat")], out="viewer")
  lcm_data$busP_cat <- cut(lcm_data$Bus.Travel,
                           breaks = cutsB,
                           labels = labsB,
                           right = TRUE,
                           ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("busP_cat")], out="viewer")
  lcm_data$trainP_cat <- cut(lcm_data$Train.Travel,
                             breaks = cutsTR,
                             labels = labsTR,
                             right = TRUE,
                             ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("trainP_cat")], out="viewer")
  lcm_data$airP_cat <- cut(lcm_data$Air.Travel,
                           breaks = cutsA,
                           labels = labsA,
                           right = TRUE,
                           ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("airP_cat")], out="viewer")
  lcm_data$microP_cat <- cut(lcm_data$Bike.or.Scooter.Travel,
                             breaks = cutsM,
                             labels = labsM,
                             right = TRUE,
                             ordered_result = TRUE)
  sjmisc::frq(x=lcm_data[c("microP_cat")], out="viewer")

  lcm_data$carFuelP_cat1 = as.numeric(lcm_data$carFuelP_cat)
  lcm_data$carMainP_cat1 = as.numeric(lcm_data$carMainP_cat)
  # lcm_data$carOthP_cat1 = as.integer(lcm_data$carOthP_cat)
  lcm_data$taxiP_cat1 = as.integer(lcm_data$taxiP_cat)
  lcm_data$busP_cat1 = as.integer(lcm_data$busP_cat)
  lcm_data$trainP_cat1 = as.integer(lcm_data$trainP_cat)
  lcm_data$airP_cat1 = as.integer(lcm_data$airP_cat)
  lcm_data$microP_cat1 = as.integer(lcm_data$microP_cat)

  sjmisc::frq(x=lcm_data[c("carFuelP_cat1")], out="viewer")
  sjmisc::frq(x=lcm_data[c("carMainP_cat1")], out="viewer")




  #demogrpahic changes
  lcm_data$veh3more = lcm_data$veh_3 + lcm_data$veh_more_3
  lcm_data$one_or_more_over_64 = lcm_data$one_pers_more_64_yd +
    lcm_data$two_pers_more_64_yd +
    lcm_data$more_two_pers_more_64_yd
  lcm_data$children_in_hh <- lcm_data$children_1 + lcm_data$children_2 + lcm_data$children_3plus
  lcm_data$spouse_in_hh <- lcm_data$married
  lcm_data$no_spouse_in_hh <- lcm_data$widowed + lcm_data$divorced + lcm_data$separated +lcm_data$never_married
  lcm_data$married_inc_under_50k <- ifelse(lcm_data$married + lcm_data$income_less_50k == 2, 1, 0)
  lcm_data$married_inc_over_50k <- ifelse(lcm_data$married
                                          + lcm_data$income_50k_to_75k
                                          + lcm_data$income_75k_to_100k
                                          + lcm_data$income_100k_to_200k
                                          + lcm_data$income_more_200k == 2,
                                          1, 0)
  lcm_data$children_and_under_50k <- ifelse(lcm_data$children_in_hh + lcm_data$income_less_50k == 2, 1, 0)
  lcm_data$children_and_over_50k <- ifelse(lcm_data$children_in_hh
                                           + lcm_data$income_50k_to_75k
                                           + lcm_data$income_75k_to_100k
                                           + lcm_data$income_100k_to_200k
                                           + lcm_data$income_more_200k == 2,
                                           1, 0)
  lcm_data$college <- lcm_data$edu_assoc_degree + lcm_data$edu_bachelors + lcm_data$edu_masters_doctorate
  lcm_data$no_college <- lcm_data$edu_elem + lcm_data$edu_high_school + lcm_data$edu_none + lcm_data$edu_some_college + lcm_data$edu_some_high_school
  lcm_data$college_less_50k <- ifelse(lcm_data$college + lcm_data$income_less_50k == 2, 1, 0)
  lcm_data$college_more_50k <- ifelse(lcm_data$college
                                      + lcm_data$income_50k_to_75k
                                      + lcm_data$income_75k_to_100k
                                      + lcm_data$income_100k_to_200k
                                      + lcm_data$income_more_200k == 2,
                                      1, 0)
  lcm_data$no_college_less_50k <- ifelse(lcm_data$no_college + lcm_data$income_less_50k == 2, 1, 0)
  lcm_data$fam_single_parent <- lcm_data$fam_single_dad + lcm_data$fam_single_mom
  lcm_data$married_college <- lcm_data$married + lcm_data$college
  lcm_data$married_no_college <- lcm_data$married + lcm_data$no_college
  lcm_data$single_college <- lcm_data$no_spouse_in_hh + lcm_data$college
  lcm_data$single_no_college <- lcm_data$no_spouse_in_hh + lcm_data$no_college

  lcm_data$single_no_kids <- ifelse(lcm_data$no_spouse_in_hh + lcm_data$children_none == 2, 1, 0)
  lcm_data$single_kids <- ifelse(lcm_data$no_spouse_in_hh + lcm_data$children_in_hh == 2, 1, 0)
  lcm_data$married_no_kids <- ifelse(lcm_data$married + lcm_data$children_none == 2, 1, 0)
  lcm_data$married_kids <- ifelse(lcm_data$married + lcm_data$children_in_hh == 2, 1, 0)

  lcm_data$single_no_kids_no_65 <- ifelse(lcm_data$single_no_kids + lcm_data$no_pers_more_64_yd == 2, 1, 0)
  lcm_data$single_kids_no_65 <- ifelse(lcm_data$single_kids + lcm_data$no_pers_more_64_yd == 2, 1, 0)
  lcm_data$married_no_kids_no_65 <- ifelse(lcm_data$married_no_kids + lcm_data$no_pers_more_64_yd == 2, 1, 0)
  lcm_data$married_kids_no_65 <- ifelse(lcm_data$married_kids + lcm_data$no_pers_more_64_yd == 2, 1, 0)
  lcm_data$single_no_kids_65 <- ifelse(lcm_data$single_no_kids + lcm_data$one_or_more_over_64 == 2, 1, 0)
  lcm_data$single_kids_65 <- ifelse(lcm_data$single_kids + lcm_data$one_or_more_over_64 == 2, 1, 0)
  lcm_data$married_no_kids_65 <- ifelse(lcm_data$married_no_kids + lcm_data$one_or_more_over_64 == 2, 1, 0)
  lcm_data$married_kids_65 <- ifelse(lcm_data$married_kids + lcm_data$one_or_more_over_64 == 2, 1, 0)

  return(lcm_data)
}


exp_data_filtered <- update_data_for_model(exp_data_filtered)
exp_data_full <- update_data_for_model(exp_data_full)

# the column names we want to use
# the value is the col_name
# the name is the new name you want to be on the summary table
col_names_to_use <- c(`Rural` = "city_type_rural",
                      `Urban` = "city_type_urban",
                      `Income: Below $50K` = "income_less_50k",
                      `Income: Between $50K - $75K` = "income_50k_to_75k",
                      `Income: Between $75K - $100K` = "income_75k_to_100k",
                      `Income: Between $100K - $200K` = "income_100k_to_200k",
                      `Income: Above $200K` = "income_more_200k",
                      `Age: 18 - 34` = "age_ref_18_to_34",
                      `Age: 35 - 44` = "age_ref_35_to_44",
                      `Age: 45 - 54` = "age_ref_45_to_54",
                      `Age: 54+` = "age_ref_more_54",
                      `Region: Northeast` = "region_northeast",
                      `Region: Midwest` = "region_midwest",
                      `Region: South` = "region_south",
                      `Region: West` = "region_wes",
                      `Has College Degree` = "college",
                      `No College Degree` = "no_college",
                      `Married` = "married",
                      `Widowed` = "widowed",
                      `Divorced` = "divorced",
                      `Separated` = "separated",
                      `Never Married` = "never_married",
                      `Children: None` = "children_none",
                      `Children: 1` = "children_1",
                      `Children: 2` = "children_2",
                      `Children: 3+` = "children_3plus",
                      `Elderly in HH: None` = "no_pers_more_64_yd",
                      `Elderly in HH: 1` = "one_pers_more_64_yd",
                      `Elderly in HH: 2` = "two_pers_more_64_yd",
                      `Elderly in HH: 3+` = "more_two_pers_more_64_yd")

# This is a list of the variables that are factored in the data.
# Each list item is the name of the column
# and within each list is the name of each factor
as_factor_cols <- list(Man_cat1 = c("Mandatory: Zero",
                                    "Mandatory: Between $0 - $100",
                                    "Mandatory: Between $200 - $500",
                                    "Mandatory: Between $500 - $1,000",
                                    "Mandatory: Above $1,000"),
                       House_cat1 = c("House: Zero",
                                      "House: Between $0 - $100",
                                      "House: Between $200 - $500",
                                      "House: Between $500 - $1,000",
                                      "House: Above $1,000"),
                       Food_cat1 = c("Food: Below $100",
                                     "Food: Between $100 - $500",
                                     "Food: Above $500"),
                       Lei_cat1 = c("Leisure: Zero",
                                    "Leisure: Between $0 - $100",
                                    "Leisure: Above $100"),
                       carFuelP_cat1 = c("Car Fuel: Zero",
                                         "Car Fuel: Between $0 - $50",
                                         "Car Fuel: Between $50 - $100",
                                         "Car Fuel: Above $100"),
                       carMainP_cat1 = c("Car Maintenance: Zero",
                                         "Car Maintenance: Between $0 - $100",
                                         "Car Maintenance: Above $100"),
                       taxiP_cat1 = c("Taxi: Zero",
                                      "Taxi: Between $0 - $20",
                                      "Taxi: Between $20 - $50",
                                      "Taxi: Above $50"),
                       busP_cat1 = c("Bus: Zero",
                                     "Bus: Any"),
                       trainP_cat1 = c("Train: Zero",
                                       "Train: Between $0 - $20",
                                       "Train: Above $20"),
                       airP_cat1 = c("Air: Zero",
                                     "Air: Between $0 - $500",
                                     "Air: Above $500"),
                       microP_cat1 = c("Micromobility: Zero",
                                       "Micromobility: Any"))

# function that gets the data frame put into the model,
# the column names you want to use, and any columns that are factors,
# and spits out a df with each variable and its percent of the whole sample
get_demographic_shares <- function(df, col_names_to_use, as_factor_cols, file_to_save = NULL){

  samplesize <- nrow(df)

  variables <- c()
  shares <- c()

  # getting the percents for col_names_to_use
  for (i in 1:length(col_names_to_use)){
    variable_val <- col_names_to_use[[i]]
    variable_name <- names(col_names_to_use)[[i]]
    variables <- c(variables, variable_name)
    shares <- c(shares, round(100 * sum(df[variable_val]) / samplesize, 1))
  }


  # getting the percents for as_factor_cols
  for (fctr_item in 1:length(as_factor_cols)){
    col_name <- names(as_factor_cols)[[fctr_item]]
    num_fctrs <- as_factor_cols[[fctr_item]] |> length()

    for (fctr_num in 1:num_fctrs){
      count <- length(df[df[[col_name]] == fctr_num,col_name])
      new_col_name <- as_factor_cols[[fctr_item]][[fctr_num]]
      variables <- c(variables, new_col_name)
      shares <- c(shares, round(100 * count / samplesize, 1))
    }

  }

  new_df <- data.frame(Variable = variables,
                       Percent = shares)

  if (is.null(file_to_save)){
    return(new_df)
  } else{
    write_csv(new_df, file = file_to_save)
    cat(paste0("File saved to \n",file_to_save))
  }

}


perc_summ_filtered <- get_demographic_shares(exp_data_filtered,
                         col_names_to_use,
                         as_factor_cols,
                         file_to_save = "data/CE_data/data_summaries/1_exp_data_filtered.csv")

perc_summ_full <- get_demographic_shares(exp_data_full,
                         col_names_to_use,
                         as_factor_cols,
                         file_to_save = "data/CE_data/data_summaries/1_exp_data_full.csv")




types <- c("single_no_kids_no_65",
           "single_kids_no_65",
           "married_no_kids_no_65",
           "married_kids_no_65",
           "single_no_kids_65",
           "single_kids_65",
           "married_no_kids_65",
           "married_kids_65")

get_demographic_shares_by_type <- function(df,
                                           col_names_to_use,
                                           as_factor_cols,
                                           types,
                                           csv_path_save = NULL){

  # add a column to state the type / category
  df$type <- apply(df[types], 1, function(x) names(x)[x == 1])

  separate_dfs <- list()
  for (type in types){
    filtered_df <- df[df$type == type, ]

    samplesize <- nrow(filtered_df)

    variables <- c()
    shares <- c()
    counts <- c()

    # getting the percents for col_names_to_use
    for (i in 1:length(col_names_to_use)){
      variable_val <- col_names_to_use[[i]]
      variable_name <- names(col_names_to_use)[[i]]
      variables <- c(variables, variable_name)
      shares <- c(shares, round(100 * sum(filtered_df[variable_val]) / samplesize, 1))
      counts <- c(counts, sum(filtered_df[variable_val]))
    }


    # getting the percents for as_factor_cols
    for (fctr_item in 1:length(as_factor_cols)){
      col_name <- names(as_factor_cols)[[fctr_item]]
      num_fctrs <- as_factor_cols[[fctr_item]] |> length()

      for (fctr_num in 1:num_fctrs){
        count <- length(filtered_df[filtered_df[[col_name]] == fctr_num,col_name])
        new_col_name <- as_factor_cols[[fctr_item]][[fctr_num]]
        variables <- c(variables, new_col_name)
        shares <- c(shares, round(100 * count / samplesize, 1))
        counts <- c(counts, count)
      }

    }

    new_df <- data.frame(Type = type,
                         Variable = variables,
                         Percent = shares,
                         Count = counts)

    separate_dfs[[type]] <- new_df

  }

  final_df <- bind_rows(separate_dfs)

  if (is.null(csv_path_save)){
    return(final_df)
  } else{
    write_csv(final_df, file = csv_path_save)
    cat(paste0("File saved to \n",csv_path_save))
  }

}


get_demographic_shares_by_type(exp_data_full,
                               col_names_to_use,
                               as_factor_cols,
                               types,
                               csv_path_save = "data/CE_data/data_summaries/4_shares_split_by_hh_structure.csv")

get_demographic_shares_by_type(exp_data_filtered,
                               col_names_to_use,
                               as_factor_cols,
                               types,
                               csv_path_save = "data/CE_data/data_summaries/4_shares_split_by_hh_structure_filtered.csv")



exp_data_full |>
  mutate(category = case_when(
    single_no_kids_no_65 == 1 ~ "single_no_kids_no_65",
    single_kids_no_65 == 1 ~ "single_kids_no_65",
    married_no_kids_no_65 == 1 ~ "married_no_kids_no_65",
    married_kids_no_65 == 1 ~ "married_kids_no_65",
    single_no_kids_65 == 1 ~ "single_no_kids_65",
    single_kids_65 == 1 ~ "single_kids_65",
    married_no_kids_65 == 1 ~ "married_no_kids_65",
    married_kids_65 == 1 ~ "married_kids_65",
    TRUE ~ "other"
  )) |>

  group_by(category) |>
  summarize(
    n = n(),
    `Car Fuel` = mean(Car.Fuel, na.rm = TRUE),
    `Family` = mean(Family.Spending, na.rm = TRUE),
    `Food` = mean(Food.Related, na.rm = TRUE),
    `Alcohol` = mean(Alcohol, na.rm = TRUE),
    `At..Home.Entertainment` = mean(At..Home.Entertainment, na.rm = TRUE),
    `House.Related` = mean(House.Related, na.rm = TRUE),
    `Hygiene` = mean(Hygiene, na.rm = TRUE),
    `Medical` = mean(Medical, na.rm = TRUE),
    `Recreational.Related` = mean(Recreational.Related, na.rm = TRUE),
    `School.Supplies` = mean(School.Supplies, na.rm = TRUE),
    `no.category.specified` = mean(no.category.specified, na.rm = TRUE),
    `Car.Maintenance` = mean(Car.Maintenance, na.rm = TRUE),
    `Clothing.Related` = mean(Clothing.Related, na.rm = TRUE),
    `Education` = mean(Education, na.rm = TRUE),
    `Train.Travel` = mean(Train.Travel, na.rm = TRUE),
    `Air.Travel` = mean(Air.Travel, na.rm = TRUE),
    `Taxi.or.Limo.Travel` = mean(Taxi.or.Limo.Travel, na.rm = TRUE),
    `Bus.Travel` = mean(Bus.Travel, na.rm = TRUE),
    `Car.Purchase` = mean(Car.Purchase, na.rm = TRUE),
    `Bike.or.Scooter.or.other.Single.Rider.Travel` = mean(Bike.or.Scooter.or.other.Single.Rider.Travel, na.rm = TRUE),
    `Ship.Travel` = mean(Ship.Travel, na.rm = TRUE),
    `Car.Payment` = mean(Car.Payment, na.rm = TRUE))



##### INTERVIEW SURVEY #####
############################

## interview files ##
# fmli.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
i_family <- read_csv("data/CE_data/2024/intrvw24_combined/fmli.csv")
# itbi.csv = Detailed income
i_income <- read_csv("data/CE_data/2024/intrvw24_combined/itbi.csv")
# itii.csv = imputed income itterations
i_income_itter <- read_csv("data/CE_data/2024/intrvw24_combined/itii.csv")
# memi.csv = member level income and characteristics
i_member <- read_csv("data/CE_data/2024/intrvw24_combined/memi.csv")
# mtbi.csv = monthly expenditures
i_month_exp <- read_csv("data/CE_data/2024/intrvw24_combined/mtbi.csv")
# ntaxi.csv = estimated federal and state income taxes
# i_income_tax <- read_csv("data/CE_data/2024/intrvw24_combined/ntaxi.csv")


# count is how many times the household was surveyed in this round
# number_of_hh is how many households are in the dataset with that count
i_family |>
  mutate(ID = substr(NEWID, 1,7)) |>
  group_by(ID) |>
  summarize(count = n()) |>
  group_by(count) |>
  summarize(number_of_hh = n())








