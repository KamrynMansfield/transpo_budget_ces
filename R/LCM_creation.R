library(poLCA)
library(tidyverse)
library(writexl)
library(gt)

lcm_data <- read.csv("data/diary24_combined/created_data/exp_fmly_data.csv",header=TRUE)

#### Initial things ####
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
labsT <- c("Zero", "TwentyB", "TwentyFifty", "HighFifB")

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


# sjPlot::sjt.xtab(var.row=lcm_data$pop_more_5_mil,
#                  var.col=lcm_data$city_type_rural,
#                  show.col.prc=TRUE,
#                  show.summary = F)
# 
# sjPlot::sjt.xtab(var.row=lcm_data$in_rural,
#                  var.col=lcm_data$city_type_rural,
#                  show.col.prc=TRUE,
#                  show.summary = F)


#### Model Creation ####
library(car)
#vif(lm(MEDICALDEV ~ EMPLOYHH + MONEYPY + NHSLDMEM + NUMADULT2 + KOWNRENT +SOLAR +INTERNET + TELLWORK +ACEQUIPM_PUB,data=df_clean1))

# Specify the LCA model formula
# Include only outcome variables in the main formula
# No covariates yet, just latent classes (indicator variables)


lcm_formula <- cbind( carFuelP_cat1, carMainP_cat1,
                        taxiP_cat1, busP_cat1, trainP_cat1,
                        airP_cat1, microP_cat1
) ~ 1

#adding covariates
# final_formula <- update(lcm_formula, . ~ .   #+  city_type_rural
#                         + income_less_50k + income_50k_to_75k + income_75k_to_100k
#                         + age_ref_18_to_34 + age_ref_35_to_44 + age_ref_45_to_54
#                        # + region_midwest	+ region_south	+ region_wes
#                         + edu_masters_doctorate + edu_bachelors
#                         + fam_single_mom	+ fam_single_dad + fam_single_person
#                         + fam_married_all_kids_less_6	+ fam_married_oldest_kid_between_6_18	+ fam_married_oldest_kid_more_17
#                         + as.factor(Man_cat1) + as.factor(House_cat1) + as.factor(Food_cat1) + as.factor(Lei_cat1)
#                         + SchoolY
#
# )
##USE THIS ONEEEEE
final_formula <- update(lcm_formula, . ~ .
                        + income_less_50k
                        + age_ref_35_to_44 + age_ref_45_to_54  + age_ref_18_to_34
                        + region_northeast
                        + edu_masters_doctorate + edu_bachelors
                        + single_kids
                        + married_no_kids + married_kids
                        + pop_more_1_mil
                        + mandatory2 + mandatory3 + mandatory4
                        + house2 + house3 + house4
                        + food2 + food3
                        + leisure2 + leisure3
                        # + as.factor(Man_cat1) + as.factor(House_cat1) + as.factor(Food_cat1) + as.factor(Lei_cat1)


)



#sjmisc::frq(x=lcm_data[c("city_type_rural")], out="viewer")


set.seed(123)  # Ensure reproducibility
model_results <- list()
for (n in 2:3) {
  model_results[[n]] <- poLCA(final_formula, lcm_data, nclass = n, na.rm = TRUE, maxiter = 5000)
}



#### reviewing results ####

for (n in 2:3) {
  print(model_results[[n]]$Chisq)
  print(model_results[[n]]$Gsq)
  print(model_results[[n]]$aic)
  print(model_results[[n]]$bic)
}


print(model_results[[3]])

# Extract posterior probabilities for the 4-class model
posterior_probs <- model_results[[3]]$posterior


# Add posterior probabilities and latent class assignments to the data
lcm_data_with_probs <- cbind(lcm_data, posterior_probs)
lcm_data_with_probs$assigned_class <- apply(posterior_probs, 1, which.max)

# Define a function for weighted mean and variance
weighted_summary <- function(data, weights) {
  weighted_mean <- sum(data * weights) / sum(weights)
  weighted_var <- sum(weights * (data - weighted_mean)^2) / sum(weights)
  c(mean = weighted_mean, variance = weighted_var)
}


# Covariates to summarize
covariates <- c("city_type_rural",
                "income_less_50k",
                "income_50k_to_75k",
                "income_75k_to_100k",
                "age_ref_18_to_34",
                "age_ref_35_to_44",
                "age_ref_45_to_54",
                "region_midwest",
                "region_south",
                "region_wes",
                "edu_masters_doctorate", "edu_bachelors",
                "fam_single_mom", "fam_single_dad", "fam_single_person",
                "fam_married_all_kids_less_6", "fam_married_oldest_kid_between_6_18", "fam_married_oldest_kid_more_17",
                "Man_cat1", "House_cat1", "Food_cat1", "Lei_cat1",
                "SchoolY"
                )


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

model_3 <- model_results[[3]]


# saveRDS(model_3, "data/final_model.rds")
