library(tidyverse)
library(readxl)
library(sf)
library(tigris)
source("R/functions.R")

## diary files ##
# memd.csv = member level income and characteristics
d_member <- read_csv("data/CE_data/2023/diary23_combined/memd.csv")
# dtbd.csv = detailed income
d_income <- read_csv("data/CE_data/2023/diary23_combined/dtbd.csv")
# dtid.csv = Income imputation iterations
d_income_itter <- read_csv("data/CE_data/2023/diary23_combined/dtid.csv")
# expd.csv = Detailed expenditure and non-expenditure data
d_exp <- read_csv("data/CE_data/2023/diary23_combined/expd.csv")
# fmld.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
d_family <- read_csv("data/CE_data/2023/diary23_combined/fmld.csv")

## interview files ##
# fmli.csv = Consumer Unit (CU) level:
# summary expenditures - income, assets, liabilities - CU characteristics and weights
i_family <- read_csv("data/CE_data/2023/intrvw23_combined/fmli.csv")
# itbi.csv = Detailed income
i_income <- read_csv("data/CE_data/2023/intrvw23_combined/itbi.csv")
# itii.csv = imputed income itterations
i_income_itter <- read_csv("data/CE_data/2023/intrvw23_combined/itii.csv")
# memi.csv = member level income and characteristics
i_member <- read_csv("data/CE_data/2023/intrvw23_combined/memi.csv")
# mtbi.csv = monthly expenditures
i_month_exp <- read_csv("data/CE_data/2023/intrvw23_combined/mtbi.csv")
# ntaxi.csv = estimated federal and state income taxes
i_income_tax <- read_csv("data/CE_data/2023/intrvw23_combined/ntaxi.csv")

## definitions ##
def_variables <- read_excel("data/CE_data/ce-pumd-interview-diary-dictionary.xlsx", sheet = 2)
def_codes <- read_excel("data/CE_data/ce-pumd-interview-diary-dictionary.xlsx", sheet = 3)
def_variables_filtered <- filter_definition_data(def_variables, year = 2023)
def_codes_filtered <- filter_definition_data(def_codes, year = 2023)
#write_csv(def_variables_filtered, "data/CE_data/ce-pumd_VARIABLES_filtered.csv")
#write_csv(def_codes_filtered, "data/CE_data/ce-pumd_CODES_filtered.csv")

## create col name description files ##
# create_col_name_csv("data/CE_data/ce-pumd-interview-diary-dictionary.xlsx",
#                     "data/CE_data/2023/intrvw23_combined")
# create_col_name_csv("data/CE_data/ce-pumd-interview-diary-dictionary.xlsx",
#                     "data/CE_data/2023/diary23_combined")


#########################################################################################################
#  Combining both weeks together

# WEEKI = Week Number

d_exp_weeks <- d_exp |>
  mutate(CUID = substr(NEWID,1,7),
         WEEKI = substr(NEWID,8,8))

d_exp_grouped <- d_exp_weeks |>
  group_by(CUID, UCC) |>
  summarize(COST = sum(COST))

#########################################################################################################


#########################################################################################################
#  Checking to see which UCCs are not used

new_ucc_defs <- read_csv("data/ce_source_integrate.csv")

current_ucc_defs <- read_excel("data/CE_data/2023/ucc_categories.xlsx")

# these UCCs are from the original files Katie and I have been using
current_uccs <- current_ucc_defs$ucc_code
# these UCCs are from a file I found on the website with more UCCs
new_uccs <- new_ucc_defs$UCC

in_current_not_new <- current_uccs[!current_uccs %in% new_uccs][order(current_uccs[!current_uccs %in% new_uccs])]
in_new_not_current <- new_uccs[!new_uccs %in% current_uccs][order(new_uccs[!new_uccs %in% current_uccs])]

in_current_not_new <- in_current_not_new[grepl("[A-Za-z]", in_current_not_new) == FALSE]
in_new_not_current <- in_new_not_current[grepl("[A-Za-z]", in_new_not_current) == FALSE]

# looking at the UCC codes that are in the expense file but not in the definition files we have
uccs_not_in_file_new <- d_exp_grouped[!d_exp_grouped$UCC %in% new_uccs,]$UCC |> unique()
uccs_not_in_file_current <- d_exp_grouped[!d_exp_grouped$UCC %in% current_uccs,]$UCC |> unique()
uccs_not_in_file_current[!uccs_not_in_file_current %in% new_uccs]e
uccs_not_in_file_new[!uccs_not_in_file_new %in% current_uccs]

additional_uccs <- new_ucc_defs[new_ucc_defs$UCC %in% uccs_not_in_file_current,c("Description","UCC")]
additional_uccs <- additional_uccs |>
  select(ucc_code = UCC, `Code description` = Description)

uccs_new_and_old <- bind_rows(current_ucc_defs, additional_uccs)
uccs_new_and_old <- bind_rows(uccs_new_and_old, data.frame(ucc_code = "250210")) # 250210 is not in either but my guess is it is "Gas, btld/tank"


uccs_new_and_old <- uccs_new_and_old |>
  mutate(in_exp_file = ifelse(ucc_code %in% unique(d_exp$UCC),1,0),
         original_ucc = ifelse(!is.na(category), 1,0),
         new_ucc = ifelse(is.na(category) & !is.na(`Code description`),1,0))

# write_csv(uccs_new_and_old, "data/CE_data/2024/ucc_categories.csv")

#########################################################################################################


data_file_path <- "data/CE_data/2023/diary23_combined/memd.csv"
col_desc_file_path <- "data/CE_data/2023/diary23_combined/column_descriptions/memd_col_descr.csv"

d_family_decoded <- decode_cells(d_family, def_codes_filtered)
d_family_adjusted <- get_wanted_cols(d_family_decoded, "data/CE_data/2023/diary23_combined/column_descriptions/fmld_col_descr.csv")

d_member_decoded <- decode_cells(d_member, def_codes_filtered)
d_member_adjusted <- get_wanted_cols(d_member_decoded, "data/CE_data/2023/diary23_combined/column_descriptions/memd_col_descr.csv")

unique_newids <- d_member_adjusted$NEWID |> unique()


for (newid in unique_newids){
  filtered_df <- d_member_adjusted |>
    filter(NEWID == newid)
}

#### making a csv with ucc categories ####
ucc_table <- read_excel("data/CES_UCC_classified_KEA.xlsx", sheet = 2) |>
  mutate(repeated = duplicated(`UCC CODE`)) |>
  filter(repeated == FALSE) |>
  select(!c("Notes", "repeated"))

categories <- names(ucc_table)[!names(ucc_table) %in% c("UCC CODE", "Code description","sum")]

list_of_dfs <- list()
for (name in categories){
  filtered_df <- ucc_table[ucc_table[name] == 1,]
  new_df <- filtered_df |>
    mutate(category = name) |>
    select("UCC CODE", "Code description", "category")

  list_of_dfs[[name]] <- new_df

}

filtered_others <- ucc_table[ucc_table["sum"] == 0,]
others_df <- filtered_others |>
  mutate(category = "no category specified") |>
  select("UCC CODE", "Code description", "category")
list_of_dfs[["others"]] <- others_df

categories_combined <- bind_rows(list_of_dfs)

# write_csv(categories_combined, "data/CE_data/2023/ucc_category_filter_new.csv")




#### Grouping by the categories we made ####

filter_df <- read_excel("data/CE_data/2023/ucc_categories_tranpso_detail.xlsx")
filter_df <- filter_df |>
  filter(!duplicated(ucc_code))
d_exp_filtered <- d_exp |>
  filter(UCC %in% filter_df$ucc_code)

d_exp_categories <- left_join(d_exp_filtered, filter_df, by = join_by(UCC == ucc_code))

d_exp_summarized <- d_exp_categories |>
  group_by(NEWID, category) |>
  summarize(tot_cost = sum(COST))

# pivotting it for my viewing pleasure
d_exp_wider <- d_exp_summarized |>
  pivot_wider(names_from = category, values_from = tot_cost)

# combine transportation and transit spending
d_exp_wider <- d_exp_wider |>
  mutate(transportation = sum(`Transportation Spending`,`Transit Spending`, na.rm = T))


ggplot(d_exp_summarized) +
  geom_boxplot(aes(y = tot_cost, colour = category), outliers = F) +
  facet_wrap(~ category)

d_exp_category_summaries <- d_exp_summarized |>
  group_by(category) |>
  summarize(mean_cost = mean(tot_cost))

d_exp_category_summaries |>
  ggplot() +
  geom_col(aes(x = category, y = mean_cost))


# potential things to look at (especially after combining expense data with family data)
# - vehicle ownership and transportation costs
# - family size and transportation costs

d_exp_fam_combined <- left_join(d_family_adjusted, d_exp_wider, by = "NEWID")
write_csv(d_exp_fam_combined, "data/CE_data/2023/diary23_combined/comb_exp_fmly_data.csv")

d_exp_fam_combined[,"Transportation Spending"] <- as.numeric(d_exp_fam_combined$`Transportation Spending`)
d_exp_fam_combined[,"owned_vehicles"] <- as.numeric(d_exp_fam_combined$owned_vehicles)
d_exp_fam_combined[,"cu_size"] <- as.numeric(d_exp_fam_combined$cu_size)
d_exp_fam_combined[,"food_exp_tot"] <- as.numeric(d_exp_fam_combined$food_exp_tot)

# write_csv(d_exp_fam_combined, "data/CE_data/2023/expenses_family_combined.csv")

d_exp_fam_combined <- d_exp_fam_combined |>
  mutate(owned_veh_zeros = ifelse(is.na(owned_vehicles),0,owned_vehicles)) |>
  mutate(owned_veh_binned = cut(owned_veh_zeros,
                                     breaks = c(-1,0,1,2,3,100),
                                     labels = c(0,1,2,3,"4+")))
ggplot(d_exp_fam_combined) +
  geom_point(aes(x = owned_veh_binned, y = `Transportation Spending`)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = ppl_under_18_yrs, y = `Transportation Spending`)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = cu_size, y = `Transportation Spending`)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = cu_size, y = owned_vehicles)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = cu_size, y = `Family Spending`)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = cu_size, y = food_exp_tot)) +
  theme_bw()

ggplot(d_exp_fam_combined) +
  geom_point(aes(x = cu_size, y = `Food Related`)) +
  theme_bw()

lm_cu_size_food_related <- lm(`Food Related` ~ cu_size, data = d_exp_fam_combined)
lm_cu_size_food_tot <- lm(food_exp_tot ~ cu_size, data = d_exp_fam_combined)


d_exp_fam_combined$children_age
d_exp_fam_combined$cu_qty
d_exp_fam_combined$cu_size
d_exp_fam_combined$pop_size_of_psu
d_exp_fam_combined$ppl_under_18_yrs
d_exp_fam_combined$ppl_over_64_yrs
d_exp_fam_combined$wlefare
d_exp_fam_combined$wage_inc_sum_all_cu_mems
d_exp_fam_combined$inc_bfr_taxes_last_12_months


# summaries grouped by vehicle ownership
veh_own_groups <- d_exp_fam_combined |>
  filter(owned_veh_flag %in% c("A","D","E","T")) |>
  group_by(owned_veh_binned) |>
  summarise(non_transit_med_exp = median(`Transportation Spending`, na.rm = T),
            transit_med_exp = median(`Transit Spending`, na.rm = T),
            family_med_exp = median(`Family Spending`, na.rm = T),
            food_med_exp = median(`Food Related`, na.rm = T),
            house_med_exp = median(`House Related`, na.rm = T),
            clothing_med_exp = median(`Clothing Related`, na.rm = T),
            recreation_med_exp = median(`Recreational Related`, na.rm = T),
            at_home_med_exp = median(`At -Home Entertainment`, na.rm = T),
            alcohol_med_exp = median(`Alcohol`, na.rm = T),
            medical_med_exp = median(`Medical`, na.rm = T),
            school_med_exp = median(`School Supplies`, na.rm = T),
            edu_med_exp = median(`Education`, na.rm = T))

# write_csv(veh_own_groups, "data/CE_data/2023/veh_own_groups.csv")

class(d_exp_fam_combined$wage_inc_sum_all_cu_mems) <- "numeric"
class(d_exp_fam_combined$inc_bfr_taxes_last_12_months) <- "numeric"
income_by_veh <- d_exp_fam_combined |>
  filter(owned_veh_flag %in% c("A","D","E","T")) |>
  group_by(owned_veh_binned) |>
  summarise(mean_incom = mean(wage_inc_sum_all_cu_mems, na.rm = T),
            med_income = median(wage_inc_sum_all_cu_mems, na.rm = T))

# write_csv(income_by_veh, "data/CE_data/2023/income_by_veh.csv")

veh_by_incom <- d_exp_fam_combined |>
  filter(!is.na(wage_inc_sum_all_cu_mems)) |>
  mutate(income_bins = cut(wage_inc_sum_all_cu_mems,
                                breaks = c(-1,20000,40000,60000,80000,100000,200000,10000000000),
                                labels = c("0 - 20K","20K - 40K","40K - 60K","60K - 80K","80K - 100K","100K - 200K", "200K +"))) |>
  filter(owned_veh_flag %in% c("A","D","E","T")) |>
  group_by(income_bins) |>
  summarise(mean_vehicles = mean(owned_vehicles, na.rm = T),
            med_vehicles = median(owned_vehicles, na.rm = T))

# write_csv(veh_by_incom, "data/CE_data/2023/veh_by_incom.csv")


group_and_summ <- function(data ,group_by_var, summarize_var){
  data |>
    group_by(.data[[group_by_var]]) |>
    summarise(mean = mean(.data[[summarize_var]]),
              median = median(.data[[summarize_var]]),
              sum = sum(.data[[summarize_var]]),
              max = max(.data[[summarize_var]]),
              min = min(.data[[summarize_var]]))
}

children_transpo <- d_exp_fam_combined |>
  group_by(children_age, ppl_under_18_yrs) |>
  summarise(median = median(transportation)) |>
  ggplot() +
  geom_col(aes(x = ppl_under_18_yrs, y = median), fill = "#FF8200") +
  theme_minimal() +
  labs(x = "People Under 18 Years Old",y = "Median Transportation Expenses ($)") +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # Transparent panel background
    plot.background = element_rect(fill = "transparent", colour = NA), # Transparent plot background
    panel.grid.minor = element_line(color = NA),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.major.x = element_line(color = NA)
  )

# ggsave("Poster/Images/children_transpo.png", dpi = 1200)


d_exp_fam_combined |>
  filter(owned_veh_flag %in% c("A","D","E","T"))
  group_by(owned_veh_binned) |>
  summarize(`Non-transit` = median(`Transportation Spending`, na.rm = T),
            Transit = median(`Transit Spending`, na.rm = T)) |>
  pivot_longer(cols = c("Non-transit","Transit"), names_to = "CostType_med",values_to = "median") |>
  filter(!is.na(owned_veh_binned)) |>
  ggplot() +
  scale_fill_manual(values = c("#ff8200", "#4B4B4B")) +
  geom_col(aes(x = owned_veh_binned, y = median, fill = CostType_med)) +
  theme_minimal() +
  labs(x = "Number of Vehicles",
       y = "Median Transportation Expenses ($)",
       fill = "Type") +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # Transparent panel background
    plot.background = element_rect(fill = "transparent", colour = NA), # Transparent plot background
    panel.grid.minor = element_line(color = NA),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.major.x = element_line(color = NA)
  )

# ggsave("Poster/Images/cars_transit.png", dpi = 1200)


## Graphing the states and vehicle ownership
us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

state_veh_avg <- d_exp_fam_combined |>
  group_by(STATE) |>
  summarise(veh_avg = mean(owned_vehicles, na.rm = T))

state_veh_avg |>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = veh_avg)) +
  geom_sf_text(aes(geometry = geometry, label = round(veh_avg,1))) +
  scale_fill_viridis_b(option = "cividis", direction = -1) +
  theme_void()


d_exp_fam_combined |>
  group_by(STATE) |>
  summarise(income_avg = mean(inc_bfr_taxes_last_12_months, na.rm = T))|>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = income_avg)) +
  geom_sf_text(aes(geometry = geometry, label = round(income_avg / 1000,0))) +
  scale_fill_viridis_b(option = "cividis", direction = -1) +
  theme_void()

# how many in each state
d_exp_fam_combined |>
  group_by(STATE) |>
  summarise(cu_count = n())|>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = cu_count)) +
  geom_sf_text(aes(geometry = geometry, label = cu_count)) +
  scale_fill_gradient(low = "grey",high = "#FF8200") +
  theme_void() +
  labs(fill = "HH Count")

d_exp_fam_combined |>
  group_by(STATE) |>
  summarise(cu_count = n())|>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = cu_count)) +
  scale_fill_gradient(low = "grey",high = "#FF8200") +
  theme_void() +
  labs(fill = "HH Count")

i_family_decoded <- decode_cells(i_family, def_codes_filtered)

i_family_decoded |>
  group_by(STATE) |>
  summarise(cu_count = n())|>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = cu_count)) +
  geom_sf_text(aes(geometry = geometry, label = cu_count)) +
  scale_fill_gradient(low = "grey",high = "#FF8200") +
  theme_void() +
  labs(fill = "HH Count")

i_family_decoded |>
  group_by(STATE) |>
  summarise(cu_count = n())|>
  left_join(us_states, by = join_by(STATE == NAME)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry,fill = cu_count)) +
  scale_fill_gradient(low = "grey",high = "#FF8200") +
  theme_void() +
  labs(fill = "HH Count")

