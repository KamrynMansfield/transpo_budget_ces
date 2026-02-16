library(tidyverse)
library(readxl)

#### Grouping by the UCC prefix
categories_data <- read_excel("data/CES_UCC_classified.xlsx")

categories_data <- categories_data |>
  mutate(prefix = substr(`UCC CODE`,1,2))

grouped_categories <- categories_data |>
  group_by(prefix) |>
  summarise(family = sum(`Family Spending`),
            transpo = sum(`Transportation Spending`),
            food = sum(`Food Related`),
            house = sum(`House Related`),
            clothing = sum(`Clothing Related`),
            rec = sum(`Recreational Related`),
            other = sum(Other))

# write_csv(grouped_categories, "data/grouped_categories.csv")

#### Summarizing some of the data ####
totals_vec <- c(family = sum(categories_data$`Family Spending` > 0),
transport = sum(categories_data$`Transportation Spending` > 0),
food = sum(categories_data$`Food Related` > 0),
house = sum(categories_data$`House Related` > 0),
clothes = sum(categories_data$`Clothing Related` > 0),
rec = sum(categories_data$`Recreational Related` > 0),
other = sum(categories_data$Other > 0))

double_categories <- categories_data |>
  filter(Sum > 1) |>
  select(`UCC CODE`,
         `Code description`,
         `Family Spending`,
         `Transportation Spending`,
         `Food Related`, `House Related`,
         `Clothing Related`,
         `Recreational Related`,
         Other)

names(double_categories) <- c("UCC","descr","family","trans","food","house","clothes","rec","other")

#### Creating a table out of the txt file ####

ucc_txt_lines <- readLines("data/CE_data/UCC_stubs/CE-HG-Inter-2023.txt")

split <- strsplit(ucc_txt_lines, "  ")

organized_rows <- lapply(split,function(x) trimws(x) |> str_subset(pattern = ".+"))

bad_rows <- c()
good_new_rows <- c()
num_new_row <- 1
new_rows <- list(organized_rows[[1]])
for (i in 2:length(organized_rows)){
  row <- organized_rows[[i]]
  if (length(row) == 2){
    old_row <- new_rows[[num_new_row]]
    new_string <- paste(old_row[[3]], row[[2]])
    new_rows[[num_new_row]][[3]] <- new_string

    bad_rows <- c(bad_rows, i)
    good_new_rows <- c(good_new_rows, num_new_row)
  } else{
    num_new_row <- num_new_row + 1
    new_rows[[num_new_row]] <- row
  }
}

CCE_stubs_df <- as.data.frame(do.call(rbind, new_rows))
names(CCE_stubs_df) <- paste0("col_",1:7)

# write_csv(CCE_stubs_df, "data/CCE_stubs_2023_df.csv")


#### adding columns for the heading number ####
# get original lines but just the ones that start with 1
# the other ones will
ucc_txt_lines_1 <- ucc_txt_lines[which(substr(ucc_txt_lines,1,1) == 1)]

heading <- substr(ucc_txt_lines_1,5,1000)
print(heading)

num_leading_spaces <- nchar(heading) - nchar(trimws(heading, which = "left"))
num_leading_spaces <- num_leading_spaces / 2

CCE_stubs_df$spaces <- num_leading_spaces

head(CCE_stubs_df)
unique(CCE_stubs_df$spaces)

heading_title1 <- NA
heading_title2 <- NA
heading_title3 <- NA
heading_title4 <- NA
heading_title5 <- NA
heading_title6 <- NA
heading_title7 <- NA
heading_title8 <- NA
heading_title9 <- NA
heading1 <- c()
heading2 <- c()
heading3 <- c()
heading4 <- c()
heading5 <- c()
heading6 <- c()
heading7 <- c()
heading8 <- c()
heading9 <- c()
for (i in 1:nrow(CCE_stubs_df)){
  row <- CCE_stubs_df[i,]
  spaces <- row$spaces
  name <- row$col_3

  if (spaces == 1){
    heading_title1 <- name
    heading_title2 <- NA
    heading_title3 <- NA
    heading_title4 <- NA
    heading_title5 <- NA
    heading_title6 <- NA
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 2){
    heading_title2 <- name
    heading_title3 <- NA
    heading_title4 <- NA
    heading_title5 <- NA
    heading_title6 <- NA
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 3){
    heading_title3 <- name
    heading_title4 <- NA
    heading_title5 <- NA
    heading_title6 <- NA
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 4){
    heading_title4 <- name
    heading_title5 <- NA
    heading_title6 <- NA
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 5){
    heading_title5 <- name
    heading_title6 <- NA
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 6){
    heading_title6 <- name
    heading_title7 <- NA
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 7){
    heading_title7 <- name
    heading_title8 <- NA
    heading_title9 <- NA
  } else if (spaces == 8){
    heading_title8 <- name
    heading_title9 <- NA
  } else if (spaces == 9){
    heading_title9 <- name
  } else{
    warning(paste("row",i,"had something weird"))
  }

  heading1 <- c(heading1, heading_title1)
  heading2 <- c(heading2, heading_title2)
  heading3 <- c(heading3, heading_title3)
  heading4 <- c(heading4, heading_title4)
  heading5 <- c(heading5, heading_title5)
  heading6 <- c(heading6, heading_title6)
  heading7 <- c(heading7, heading_title7)
  heading8 <- c(heading8, heading_title8)
  heading9 <- c(heading9, heading_title9)

}

CCE_stubs_df$head_1 <- heading1
CCE_stubs_df$head_2 <- heading2
CCE_stubs_df$head_3 <- heading3
CCE_stubs_df$head_4 <- heading4
CCE_stubs_df$head_5 <- heading5
CCE_stubs_df$head_6 <- heading6
CCE_stubs_df$head_7 <- heading7
CCE_stubs_df$head_8 <- heading8
CCE_stubs_df$head_9 <- heading9




#### Taking a look at CCE_stubs_df ####

# making a nested list of categories
main_categories <- CCE_stubs_df$head_1 |> unique()

cat_list <- list()
for (i in 1:length(main_categories)){
  cat_name <- main_categories[[i]]
  filtered_df <- CCE_stubs_df |>
    filter(head_1 == cat_name)
  sub_categories <- unique(filtered_df$head_2)

  for (j in 1:length(sub_categories)){
    sub_cat_name <- sub_categories[[j]]
    filtered_again <- filtered_df |>
      filter(head_2 == sub_cat_name)
    sub_sub_cats <- unique(filtered_again$head_3)

    cat_list[[cat_name]][[sub_cat_name]] <- sub_sub_cats
  }
}

names(cat_list[[11]])


#### Comparing my UCC data and Katie's excel ####

ucc_katie <- read_excel("data/CES_UCC_classified.xlsx")
ucc_kamryn <- read_excel("data/UCC_stubs_2023_df.xlsx")

ucc_katie_filtered <- ucc_katie |>
  filter(!`UCC CODE` %in% ucc_kamryn$col_4)

nrow(ucc_katie)
nrow(ucc_katie_filtered)

ucc_kamryn_filtered <- ucc_kamryn |>
  filter(!col_4 %in% ucc_katie$`UCC CODE`)
nrow(ucc_kamryn_filtered)

ordered_kam <- ucc_kamryn_filtered$col_4[order(as.numeric(ucc_kamryn_filtered$col_4))]
ordered_katie <- ucc_katie_filtered$`UCC CODE`[order(as.numeric(ucc_katie_filtered$`UCC CODE`))]

ucc_kamryn_filtered$col_3
ucc_katie_filtered$`Code description`


