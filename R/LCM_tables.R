library(poLCA)
library(tidyverse)
library(writexl)
library(gt)

#### Initial things ####
lcm_data <- read.csv("data/diary24_combined/created_data/exp_fmly_data.csv",header=TRUE)

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
# sjmisc::frq(x=lcm_data[c("microY")], out="viewer")

lcm_data$sum = lcm_data$LeiY +lcm_data$SchoolY + lcm_data$ManY + lcm_data$HouseY - 4
# sjmisc::frq(x=lcm_data[c("sum")], out="viewer")

#### cuts for non transpo ####
cutslei <- c(-Inf, 0, 100, Inf)
labslei <- c("Zero", "HundredB", "HighHunB")
lcm_data$Lei_cat <- cut(lcm_data$Leisure,
                        breaks = cutslei,
                        labels = labslei,
                        right = TRUE,
                        ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("Lei_cat")], out="viewer")
lcm_data$Lei_cat1 = as.integer(lcm_data$Lei_cat)

cutsfood <- c(-Inf, 100, 500, Inf)
labsfood <- c("OneHudB", "FiveHundredB", "HighFiveHunB")
lcm_data$Food_cat <- cut(lcm_data$FoodAlc,
                         breaks = cutsfood,
                         labels = labsfood,
                         right = TRUE,
                         ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("Food_cat")], out="viewer")
lcm_data$Food_cat1 = as.integer(lcm_data$Food_cat)

cutsman <- c(-Inf, 0, 200, 500, Inf)
labsman <- c("Zero", "two", "five", "over")
lcm_data$Man_cat <- cut(lcm_data$Mandatory,
                        breaks = cutsman,
                        labels = labsman,
                        right = TRUE,
                        ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("Man_cat")], out="viewer")
lcm_data$Man_cat1 = as.integer(lcm_data$Man_cat)

cutshous <- c(-Inf, 0, 200, 500, Inf)
labshous <- c("Zero", "two", "five", "over")
lcm_data$House_cat <- cut(lcm_data$Housing,
                          breaks = cutshous,
                          labels = labshous,
                          right = TRUE,
                          ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("House_cat")], out="viewer")
lcm_data$House_cat1 = as.integer(lcm_data$House_cat)


#### cuts for transpo ####
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
# sjmisc::frq(x=lcm_data[c("carFuelP_cat")], out="viewer")
lcm_data$carMainP_cat <- cut(lcm_data$Car.Maintenance,
                             breaks = cutsC2,
                             labels = labsC2,
                             right = TRUE,
                             ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("carMainP_cat")], out="viewer")
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
# sjmisc::frq(x=lcm_data[c("taxiP_cat")], out="viewer")
lcm_data$busP_cat <- cut(lcm_data$Bus.Travel,
                         breaks = cutsB,
                         labels = labsB,
                         right = TRUE,
                         ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("busP_cat")], out="viewer")
lcm_data$trainP_cat <- cut(lcm_data$Train.Travel,
                           breaks = cutsTR,
                           labels = labsTR,
                           right = TRUE,
                           ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("trainP_cat")], out="viewer")
lcm_data$airP_cat <- cut(lcm_data$Air.Travel,
                         breaks = cutsA,
                         labels = labsA,
                         right = TRUE,
                         ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("airP_cat")], out="viewer")
lcm_data$microP_cat <- cut(lcm_data$Bike.or.Scooter.Travel,
                           breaks = cutsM,
                           labels = labsM,
                           right = TRUE,
                           ordered_result = TRUE)
# sjmisc::frq(x=lcm_data[c("microP_cat")], out="viewer")

lcm_data$carFuelP_cat1 = as.numeric(lcm_data$carFuelP_cat)
lcm_data$carMainP_cat1 = as.numeric(lcm_data$carMainP_cat)
# lcm_data$carOthP_cat1 = as.integer(lcm_data$carOthP_cat)
lcm_data$taxiP_cat1 = as.integer(lcm_data$taxiP_cat)
lcm_data$busP_cat1 = as.integer(lcm_data$busP_cat)
lcm_data$trainP_cat1 = as.integer(lcm_data$trainP_cat)
lcm_data$airP_cat1 = as.integer(lcm_data$airP_cat)
lcm_data$microP_cat1 = as.integer(lcm_data$microP_cat)

# sjmisc::frq(x=lcm_data[c("carFuelP_cat1")], out="viewer")
# sjmisc::frq(x=lcm_data[c("carMainP_cat1")], out="viewer")




#### Adding demographic columns ####
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

lcm_data$urban <- 1 - lcm_data$in_rural

lcm_data$urban_northeast <- ifelse(lcm_data$urban + lcm_data$region_northeast == 2, 1, 0)
lcm_data$urban_midwest <- ifelse(lcm_data$urban + lcm_data$region_midwest == 2, 1, 0)
lcm_data$urban_south <- ifelse(lcm_data$urban + lcm_data$region_south == 2, 1, 0)
lcm_data$urban_wes <- ifelse(lcm_data$urban + lcm_data$region_wes == 2, 1, 0)

lcm_data$rural_northeast <- ifelse(lcm_data$in_rural + lcm_data$region_northeast == 2, 1, 0)
lcm_data$rural_midwest <- ifelse(lcm_data$in_rural + lcm_data$region_midwest == 2, 1, 0)
lcm_data$rural_south <- ifelse(lcm_data$in_rural + lcm_data$region_south == 2, 1, 0)
lcm_data$rural_wes <- ifelse(lcm_data$in_rural + lcm_data$region_wes == 2, 1, 0)

lcm_data$pop_more_1_mil <- lcm_data$pop_1_mil_to_5_mil +lcm_data$pop_more_5_mil
lcm_data$pop_100k_to_1_mil <- lcm_data$pop_100k_to_500k +lcm_data$pop_500k_to_1_mil

lcm_data$pop_more_1_mil_northeast <- ifelse(lcm_data$pop_more_1_mil + lcm_data$region_northeast == 2, 1, 0)

lcm_data$cu_size_more_2 <- lcm_data$cu_size_3 + lcm_data$cu_size_4 + 
  lcm_data$cu_size_5 + lcm_data$cu_size_6 + lcm_data$cu_size_7+ lcm_data$cu_size_more_7

lcm_data$cu_size_more_3 <- lcm_data$cu_size_4 + 
  lcm_data$cu_size_5 + lcm_data$cu_size_6 + lcm_data$cu_size_7+ lcm_data$cu_size_more_7

lcm_data$cu_size_more_4 <- lcm_data$cu_size_5 + lcm_data$cu_size_6 + lcm_data$cu_size_7+ lcm_data$cu_size_more_7


lcm_data$mandatory2 <- ifelse(lcm_data$Man_cat == "two", 1, 0)
lcm_data$mandatory3 <- ifelse(lcm_data$Man_cat == "five", 1, 0)
lcm_data$mandatory4 <- ifelse(lcm_data$Man_cat == "over", 1, 0)

lcm_data$house2 <- ifelse(lcm_data$House_cat == "two", 1, 0)
lcm_data$house3 <- ifelse(lcm_data$House_cat == "five", 1, 0)
lcm_data$house4 <- ifelse(lcm_data$House_cat == "over", 1, 0)

lcm_data$food2 <- ifelse(lcm_data$Food_cat == "FiveHundredB", 1, 0)
lcm_data$food3 <- ifelse(lcm_data$Food_cat == "HighFiveHunB", 1, 0)

lcm_data$leisure2 <- ifelse(lcm_data$Lei_cat == "HundredB", 1, 0)
lcm_data$leisure3 <- ifelse(lcm_data$Lei_cat == "HighHunB", 1, 0)


#### PULLING IN MODEL AND MAKING TABLES AND THINGS ####
model_3 <- readRDS("data/final_model.rds")

#### functions to create tables ####
create_probs_table <- function(model, percent = FALSE, lcm_data){
  mult <- ifelse(percent, 100, 1)
  rnd <- ifelse(percent, 2, 4)

  df_totals <- matrix(model$P * mult) |>
    round(rnd) |>
    t() |>
    as.data.frame()

  names(df_totals) <- paste0("class_",c(1:ncol(df_totals)))
  df_totals$variable <- "Class Probablilty"

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


model_df <- create_regr_tbl(model_3)

new_exog_names_list <- list("Intercept" = list("Intercept" = "(Intercept)"),
                       "Income (more than $50,000)" = list("$50,000 or less" = "income_less_50k"),
                       "Reference Age (55 or older)" = list("18 - 34" = "age_ref_18_to_34",
                                                            "35 - 44" = "age_ref_35_to_44",
                                                            "45 - 54" = "age_ref_45_to_54"),
                       "Region (not Northeast)" = list("Northeast" = "region_northeast"),
                       "Education (No college degree)" = list("Masters or above" = "edu_masters_doctorate",
                                                             "Bachelors" = "edu_bachelors"),
                       "Household Structure (Single without children)" = list("Single with children" = "single_kids",
                                                    "Married without children" = "married_no_kids",
                                                    "Married with children" = "married_kids"),
                       "Population Size (Below 1 million)" = list("Above 1 million" = "pop_more_1_mil"),
                       "Mandatory Expenses (none)" = list("$0.01 - $200" = "mandatory2",
                                                          "$200.01 - $500" = "mandatory3",
                                                          "> $500" = "mandatory4"),
                       "House Expenses (none)" = list("$0.01 - $200" = "house2",
                                                      "$200.01 - $500" = "house3 ",
                                                      "> $500" = "house4"),
                       "Food Expenses (<$100)" = list("$100.01 - $500" = "food2",
                                                      ">$500" = "food3"),
                       "Leisure Expenses (none)" = list("$0.01 - $100" = "leisure2",
                                                        ">$100" = "leisure3"))

new_exog_names <- list("Intercept" = "(Intercept)",
                       "$50,000 or less" = "income_less_50k",
                       "18 - 34" = "age_ref_18_to_34",
                       "35 - 44" = "age_ref_35_to_44",
                       "45 - 54" = "age_ref_45_to_54",
                       "Northeast" = "region_northeast",
                       "Masters or above" = "edu_masters_doctorate",
                       "Bachelors" = "edu_bachelors",
                       "Single with children" = "single_kids",
                       "Married without children" = "married_no_kids",
                       "Married with children" = "married_kids",
                       "Above 1 million" = "pop_more_1_mil",
                       "$0.01 - $200" = "mandatory2",
                       "$200.01 - $500" = "mandatory3",
                       "> $500" = "mandatory4",
                       "$0.01 - $200" = "house2",
                       "$200.01 - $500" = "house3 ",
                       "> $500" = "house4",
                       "$100.01 - $500" = "food2",
                       ">$500" = "food3",
                       "$0.01 - $100" = "leisure2",
                       ">$100" = "leisure3")

exog_name_key <- matrix(c("Intercept" , "(Intercept)", "Intercept",
     "$50,000 or less" , "income_less_50k", "Income (more than $50,000)",
     "18 - 34" , "age_ref_18_to_34","Reference Age (55 or older)",
     "35 - 44" , "age_ref_35_to_44","Reference Age (55 or older)",
     "45 - 54" , "age_ref_45_to_54","Reference Age (55 or older)",
     "Northeast" , "region_northeast","Region (not Northeast)",
     "Masters or above" , "edu_masters_doctorate","Education (No college degree)",
     "Bachelors" , "edu_bachelors","Education (No college degree)",
     "Single with children" , "single_kids", "Household Structure (Single without children)",
     "Married without children" , "married_no_kids","Household Structure (Single without children)",
     "Married with children" , "married_kids","Household Structure (Single without children)",
     "Above 1 million" , "pop_more_1_mil","Population Size (Below 1 million)",
     "$0.01 - $200" , "mandatory2","Mandatory Expenses (none)",
     "$200.01 - $500" , "mandatory3","Mandatory Expenses (none)",
     "> $500" , "mandatory4","Mandatory Expenses (none)",
     "$0.01 - $200" , "house2","House Expenses (none)",
     "$200.01 - $500" , "house3","House Expenses (none)",
     "> $500" , "house4","House Expenses (none)",
     "$100.01 - $500" , "food2","Food Expenses (<$100)",
     ">$500" , "food3","Food Expenses (<$100)",
     "$0.01 - $100" , "leisure2","Leisure Expenses (none)",
     ">$100" , "leisure3","Leisure Expenses (none)"),
     byrow = T, ncol = 3) |>
  as.data.frame()
names(exog_name_key) <- c("exogenous_var_name","exogenous_var","category")


model_df <- create_regr_tbl(model_3)

create_nice_coeff_tbl <- function(model_df, 
                                  exog_name_key,
                                  highlighted = TRUE,
                                  highlight_color = "#FF8200",
                                  t_threshold = 1.4,
                                  only_significant = FALSE,
                                  two_vs_one_name = "Class 2 vs. Class 1",
                                  three_vs_one_name = "Class 3 vs. Class 1"){
  
  model_df <- left_join(exog_name_key, model_df, by = "exogenous_var") |>
    dplyr::mutate(coeff_1 = paste0(coeff_1, " (",t_stat_1,")"),
                  coeff_2 = paste0(coeff_2, " (",t_stat_2,")"))
  
  
  if (only_significant){
    model_gt <- model_df |>
      dplyr::mutate(coeff_1 = ifelse(t_stat_1 >= t_threshold | t_stat_1 <= -t_threshold,coeff_1, "-"),
                    coeff_2 = ifelse(t_stat_2 >= t_threshold | t_stat_2 <= -t_threshold,coeff_2, "-")) |>
      dplyr::select(category, exogenous_var_name, coeff_1, t_stat_1, coeff_2, t_stat_2) |>
      gt(rowname_col = "exogenous_var_name",
         groupname_col = "category") |>
      tab_footnote(footnote = "Dashes indicate an insignificant result")
    
  } else{
    model_gt <- model_df |>
      dplyr::select(category, exogenous_var_name, coeff_1, t_stat_1, coeff_2, t_stat_2) |>
      gt(rowname_col = "exogenous_var_name",
         groupname_col = "category")
  }
  
  
  
  if (highlighted){
    model_gt <- model_gt |>
      tab_style(
        style = list(
          cell_fill(color = highlight_color), # Highlights the cell background
          cell_text(weight = "bold")     # Makes the text bold
        ),
        locations = cells_body(
          columns = coeff_1,                   # Target the 'hp' column
          rows = t_stat_1 >= t_threshold | t_stat_1 <= -t_threshold              # Condition: highlight cells where hp is > 150
        )
      ) |>
      tab_style(
        style = list(
          cell_fill(color = highlight_color), # Highlights the cell background
          cell_text(weight = "bold")     # Makes the text bold
        ),
        locations = cells_body(
          columns = coeff_2,                   # Target the 'hp' column
          rows = t_stat_2 >= t_threshold | t_stat_2 <= -t_threshold            # Condition: highlight cells where hp is > 150
        )
      ) |>
      tab_footnote(footnote = "Statistically significant cells are highlighted")
  }
  
  model_gt <- model_gt |>
    cols_hide(columns = c(t_stat_1, t_stat_2)) |>
    cols_align(
      align = "center",
      columns = c(coeff_1, coeff_2)) |>
    cols_label(
      coeff_1 = two_vs_one_name,
      coeff_2 = three_vs_one_name)
    
  return(model_gt)
}

create_nice_coeff_tbl(model_df, 
                      exog_name_key,
                      highlighted = TRUE,
                      highlight_color = "#FF8200",
                      t_threshold = 1.4,
                      only_significant = FALSE,
                      two_vs_one_name = "Class 2 vs. Class 1",
                      three_vs_one_name = "Class 3 vs. Class 1")


#### Simple viewing and saving results to excel ####

create_probs_table(model_3, lcm_data = lcm_data) |> 
  mutate(class_1 = round(class_1 * 100,2),
         class_2 = round(class_2 * 100,2),
         class_3 = round(class_3 * 100,2)) |>
  gt()
probs_df$variable
probs_df$level_def

expense_variable_names <- matrix(c("Class Probablilty", "Class Probablilty",
         "carFuelP_cat1"  , "Car Fuel Expense",
         "carMainP_cat1"  , "Car Maintenance Expense",
         "taxiP_cat1"       , "Taxi Travel Expense",
         "busP_cat1"     , "Bus Travel Expense",
         "trainP_cat1"   , "Train Travel Expense" ,
         "airP_cat1"        , "Air Travel Expense",
         "microP_cat1"      , "Microtransit Travel Expense"),
 byrow = T, ncol = 2) |>
  as.data.frame()

names(expense_variable_names) <- c("variable","nice_var_name")

expense_level_names <- matrix(c("NA"  ,"",  
                                "Zero", "None",  
                                "FiftyB" ,"$0.00 - $50.00",
                                "HundredB", "$50.00 - $100.00",
                                "HighHunB" , "$100.00 +",
                                "TwentyB" , "$0.00 - $20.00",
                                "TwentyFifty" , "$20.00 - $50.00",
                                "HighFifB", "$50.00 +",
                                "Any", "Any",
                                "HighTwB", "$20.00 +",
                                "FiveHB" , "$0.00 - $500.00",
                                "HighFive", "$500.00 +"),
                              byrow = T, ncol = 2) |>
  as.data.frame()
 
names(expense_level_names) <- c("level_def","nice_level_name")

create_nice_probs_table <- function(model, 
                                    expense_variable_names, 
                                    expense_level_names,
                                    lcm_data){
  probs_df <- create_probs_table(model, lcm_data = lcm_data) |> 
    mutate(class_1 = round(class_1 * 100,2),
           class_2 = round(class_2 * 100,2),
           class_3 = round(class_3 * 100,2)) |>
    left_join(expense_variable_names, by = "variable") |>
    left_join(expense_level_names, by = "level_def") |>
    select(nice_var_name, nice_level_name, class_1, class_2, class_3)
  
  probs_df |>
    gt(rowname_col = "nice_level_name",
       groupname_col = "nice_var_name")
}

create_nice_probs_table(model_3, expense_variable_names, expense_level_names)

create_regr_tbl(model_3) |> 
  # mutate(coeff_1 = ifelse(t_stat_1 >= 1.4 | t_stat_1 <= -1.4,coeff_1, "-"),
  #        t_stat_1 = ifelse(t_stat_1 >= 1.4 | t_stat_1 <= -1.4,t_stat_1, "-"),
  #        coeff_2 = ifelse(t_stat_2 >= 1.4 | t_stat_2 <= -1.4,coeff_2, "-"),
  #        t_stat_2 = ifelse(t_stat_2 >= 1.4 | t_stat_2 <= -1.4,t_stat_2, "-")) |>
  gt() |>
  tab_style(
    style = list(
      cell_fill(color = "yellow"), # Highlights the cell background
      cell_text(weight = "bold")     # Makes the text bold
    ),
    locations = cells_body(
      columns = t_stat_1,                   # Target the 'hp' column
      rows = t_stat_1 >= 1.4 | t_stat_1 <= -1.4              # Condition: highlight cells where hp is > 150
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "yellow"), # Highlights the cell background
      cell_text(weight = "bold")     # Makes the text bold
    ),
    locations = cells_body(
      columns = t_stat_2,                   # Target the 'hp' column
      rows = t_stat_2 >= 1.4 | t_stat_2 <= -1.4              # Condition: highlight cells where hp is > 150
    )
  )

write_model_to_excel <- function(model, lcm_data, save_file){
  probs_table <- create_probs_table(model, TRUE, lcm_data)
  regr_table <- create_regr_tbl(model)

  listed_dfs <- list(probability = probs_table,
                     regression = regr_table)

  write_xlsx(listed_dfs, path = save_file)

  return(cat("model results saved to\n",save_file))
}

# write_model_to_excel(model_3, lcm_data, "data/lcm_results/lcm1.xlsx")


# calculating how many households make up the percentage in the percentage table. 
probs_tbl <- create_probs_table(model_3, lcm_data = lcm_data)

c1 <- (probs_tbl[1,3:5] * 6004)[["class_1"]]
c2 <- (probs_tbl[1,3:5] * 6004)[["class_2"]]
c3 <- (probs_tbl[1,3:5] * 6004)[["class_3"]]

c1_row <- probs_tbl[2:nrow(probs_tbl),"class_1"]
c2_row <- probs_tbl[2:nrow(probs_tbl),"class_2"]
c3_row <- probs_tbl[2:nrow(probs_tbl),"class_3"]

percentage_counts <- data.frame(class_1_cnt = c(c1,c1_row * c1),
                     class_2_cnt = c(c2,c2_row * c2),
                     class_3_cnt = c(c3,c3_row * c3))


# # quickly viewing a big of hou
# no_house_spending <- lcm_data |> 
#   filter(House.Related == 0)
# 
# sums_no_housing <- no_house_spending |>
#   select(married_no_kids, married_kids, single_no_kids, single_kids,
#          income_less_50k, income_50k_to_75k, income_75k_to_100k, income_100k_to_200k, income_more_200k) |>
#   colSums()
# 
# data.frame(Category = c(rep("Family Structure",4),rep("Income",5)),
#            Label = names(sums_no_housing),
#            Count = sums_no_housing) |>
#   group_by(Category) |>
#   mutate(Percent = round(100 * Count / sum(Count), 1)) |>
#   ungroup() |>
#   gt()



#### CODE KATIE SENT ####
# Covariates to summarize
covariates <- c("income_less_50k",
                "age_ref_35_to_44", "age_ref_45_to_54","age_ref_18_to_34",
                "region_northeast",
                "edu_masters_doctorate","edu_bachelors",
                "single_kids",
                "married_no_kids", "married_kids",
                "pop_more_1_mil",
                "mandatory2", "mandatory3" ,"mandatory4",
                "house2" ,"house3" , "house4",
                "food2" , "food3",
                "leisure2" , "leisure3")


# Define a function for weighted mean and variance
weighted_summary <- function(data, weights) {
  weighted_mean <- sum(data * weights) / sum(weights)
  weighted_var <- sum(weights * (data - weighted_mean)^2) / sum(weights)
  c(mean = weighted_mean, variance = weighted_var)
}

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

# Print summaries for each class
for (class in 1:3) {
  cat("\nSummary Statistics for Class", class, ":\n")
  print(class_summaries_df[[class]])
}


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

create_nice_covariate_tbl(final_model, lcm_data, covariates)
















