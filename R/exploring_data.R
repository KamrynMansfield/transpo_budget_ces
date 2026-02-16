library(tidyverse)
library(readxl)
library(sf)
library(tigris)
source("R/functions.R")

## diary files ##
# memd.csv = member level income and characteristics
d_member <- read_csv("data/CE_data/2023/diary23_combined/memd.csv")
# fmld.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
d_family <- read_csv("data/CE_data/2023/diary23_combined/fmld.csv")

## interview files ##
# memi.csv = member level income and characteristics
i_member <- read_csv("data/CE_data/2023/intrvw23_combined/memi.csv")
# fmli.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
i_family <- read_csv("data/CE_data/2023/intrvw23_combined/fmli.csv")

# expense / family demographics data I created
exp_data <- read_csv("data/CE_data/2023/diary23_combined/created_data/exp_fmly_data.csv")

# how many rows are in the interview files?
nrow(i_family)
nrow(i_member)

# how many unique ids are in the intervew files
length(unique(i_family$NEWID))
length(unique(i_member$NEWID))

# how many rows are in the diary files?
nrow(d_family)
nrow(d_member)

# how many unique ids are in the diary files
length(unique(d_family$NEWID))
length(unique(d_member$NEWID))

# are any of the newid values the same across surveys?
length(d_family$NEWID[d_family$NEWID %in% i_family$NEWID])
length(i_family$NEWID[i_family$NEWID %in% d_family$NEWID])



transpo_medians <- c(car_fuel = median(exp_data$`Car Fuel`[exp_data$`Car Fuel` != 0]),
                           car_purch = median(exp_data$`Car Purchase`[exp_data$`Car Purchase` != 0]),
                           car_maint = median(exp_data$`Car Maintenance`[exp_data$`Car Maintenance` != 0]),
                           car_oth = median(exp_data$`Car Other`[exp_data$`Car Other` != 0]),
                           taxi = median(exp_data$`Taxi or Limo Travel`[exp_data$`Taxi or Limo Travel` != 0]),
                           bus = median(exp_data$`Bus Travel`[exp_data$`Bus Travel` != 0]),
                           train = median(exp_data$`Train Travel`[exp_data$`Train Travel` != 0]),
                           air = median(exp_data$`Air Travel`[exp_data$`Air Travel` != 0]),
                           bike = median(exp_data$`Bike or Scooter or other Single Rider Travel`[exp_data$`Bike or Scooter or other Single Rider Travel` != 0]))


transpo_sums <- c(car_fuel = sum(exp_data$`Car Fuel`[exp_data$`Car Fuel` != 0]),
                     car_purch = sum(exp_data$`Car Purchase`[exp_data$`Car Purchase` != 0]),
                     car_maint = sum(exp_data$`Car Maintenance`[exp_data$`Car Maintenance` != 0]),
                     car_oth = sum(exp_data$`Car Other`[exp_data$`Car Other` != 0]),
                     taxi = sum(exp_data$`Taxi or Limo Travel`[exp_data$`Taxi or Limo Travel` != 0]),
                     bus = sum(exp_data$`Bus Travel`[exp_data$`Bus Travel` != 0]),
                     train = sum(exp_data$`Train Travel`[exp_data$`Train Travel` != 0]),
                     air = sum(exp_data$`Air Travel`[exp_data$`Air Travel` != 0]),
                     bike = sum(exp_data$`Bike or Scooter or other Single Rider Travel`[exp_data$`Bike or Scooter or other Single Rider Travel` != 0]))


transpo_data <- data.frame(category = names(transpo_medians),
                           median_expenses = transpo_medians,
                           summed_expenses = transpo_sums)

transpo_cats <- c("Car Fuel",
                  "Car Purchase",
                  "Car Maintenance",
                  "Car Other",
                  "Taxi or Limo Travel",
                  "Bus Travel",
                  "Train Travel",
                  "Air Travel",
                  "Bike or Scooter or other Single Rider Travel")

df_list <- list()
for (i in 1:length(transpo_cats)){
  col_name <- transpo_cats[[i]]
  median_col <- median(exp_data[col_name][exp_data[col_name] != 0])
  sum_col <- sum(exp_data[col_name][exp_data[col_name] != 0])
  n <- exp_data[exp_data[col_name] != 0,] |> nrow()

  df_list[[paste0("row_",i)]] <- data.frame(category = col_name,
                                            median_exp = median_col,
                                          summed_exp = sum_col,
                                          count = n)
}

transpo_data <- bind_rows(df_list)
transpo_data[9,1] <- "Bike"

transpo_data |>
  filter(category != "car_purch") |>
  ggplot() +
  geom_col(aes(x = category, y = summed_exp), fill = "#ff8200") +
  geom_text(aes(x = category, y = summed_exp, label = count), vjust = -.45) +
  theme_minimal() +
  labs(x = "Expense Category",
       y = "Total Spent In Sample")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

transpo_data |>
  filter(category != "Car Purchase") |>
  ggplot() +
  geom_col(aes(x = category, y = median_exp), fill = "#ff8200") +
  geom_text(aes(x = category, y = median_exp, label = count), vjust = -.45) +
  theme_minimal() +
  labs(x = "Expense Category",
       y = "Median Expense Sample")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


total_income <- exp_data |>
  select(income_less_50k, income_50k_to_75k, income_75k_to_100k, income_100k_to_200k, income_more_200k) |>
  pivot_longer(cols = starts_with("income")) |>
  filter(value > 0) |>
  count(name, name = "count_total")

# anyone who put bus or train
transit_income <- exp_data |>
  mutate(transit_expenses = `Bus Travel` + `Train Travel`) |>
  filter(transit_expenses > 0) |>
  select(transit_expenses, income_less_50k, income_50k_to_75k, income_75k_to_100k, income_100k_to_200k, income_more_200k) |>
  pivot_longer(cols = starts_with("income")) |>
  filter(value > 0) |>
  count(name, name = "count_transit")



# who only put bus or train and no car expenses
only_transit_income <- exp_data |>
  mutate(car_expenses = `Car Fuel` + `Car Other` + `Car Purchase` + `Car Maintenance`) |>
  mutate(transit_expenses = `Bus Travel` + `Train Travel`) |>
  filter(car_expenses == 0) |>
  filter(transit_expenses > 0) |>
  select(transit_expenses, income_less_50k, income_50k_to_75k, income_75k_to_100k, income_100k_to_200k, income_more_200k) |>
  pivot_longer(cols = starts_with("income")) |>
  filter(value > 0) |>
  count(name, name = "count_only_transit")




combined_income <- left_join(total_income, transit_income) |>
  left_join(only_transit_income)

correct_order <- c("income_less_50k",
                   "income_50k_to_75k",
                   "income_75k_to_100k",
                   "income_100k_to_200k",
                   "income_more_200k")

combined_income$name <- factor(combined_income$name, levels = correct_order)

arrange(combined_income,name)

combined_income |>
  pivot_longer(cols = c("count_total","count_transit","count_only_transit"), names_to = "count_type") |>
  ggplot() +
  geom_col(aes(x = name, y = value, fill = count_type))


combined_income |>
  mutate(pct_total = count_total / sum(combined_income$count_total),
         pct_transit = count_transit / sum(combined_income$count_transit),
         pct_only_transit = count_only_transit / sum(combined_income$count_only_transit)) |>
  pivot_longer(cols = c("pct_total","pct_transit", "pct_only_transit"), names_to = "pct_type") |>
  ggplot() +
  geom_col(aes(x = name, y = value, fill = pct_type))

# write_csv(combined_income,"data/CE_data/2023/diary23_combined/created_data/income_and_transit.csv")


income_pct <- as.data.frame(t(combined_income[,names(combined_income) != "name"]))

names(income_pct) <- combined_income$name

income_pct$sums <- rowSums(income_pct)

income_pct$type <- income_pct |> row.names()

income_pct |>
  mutate(pct_100k_to_200k = 100 * income_100k_to_200k / sums,
         pct_50k_to_75k = 100 * income_50k_to_75k / sums,
         pct_75k_to_100k =  100 * income_75k_to_100k / sums,
         pct_less_50k = 100 * income_less_50k / sums,
         pct_more_200k = 100 * income_more_200k / sums) |>
  select(type, pct_100k_to_200k,pct_50k_to_75k,
         pct_75k_to_100k,
         pct_less_50k,
         pct_more_200k) |>
  pivot_longer(cols = c("pct_100k_to_200k",
                        "pct_50k_to_75k",
                        "pct_75k_to_100k",
                        "pct_less_50k",
                        "pct_more_200k"), names_to = "income_pct") |>
  ggplot() +
  geom_col(aes(x = type, y = value, fill = income_pct)) +
  theme_minimal() +
  labs(x = "Expense Type",
       y = "Percent",
       fill = "Income Bracket")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))




exp_with_income <- exp_data |>
  mutate(income_group = case_when(
    income_less_50k > 0 ~ "Less than 50K",
    income_50k_to_75k > 0 ~ "50K - 75K",
    income_75k_to_100k > 0 ~ "75K - 100K",
    income_100k_to_200k > 0 ~ "100K - 200K",
    income_more_200k > 0 ~ "200K or more",
    TRUE ~ "Did not Specify"
  )) |>
  mutate(transit = case_when(
    income_less_50k > 0 ~ "Less than 50K",
    income_50k_to_75k > 0 ~ "50K - 75K",
    income_75k_to_100k > 0 ~ "75K - 100K",
    income_100k_to_200k > 0 ~ "100K - 200K",
    income_more_200k > 0 ~ "200K or more",
    TRUE ~ "Did not Specify"
  ))

exp_with_income |>
  ggplot() +
  geom_histogram(aes(x = income_group))
