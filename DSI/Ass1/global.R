library(shiny)
library(tidyverse)
# load plot packages
library(DT) # DT::datatable
library(vcd) # vcd::mosaic
library(corrgram) # corrgram::corrgram
library(visdat) # visdat::vis_miss
library(GGally) # GGally::ggpairs
library(scales)
library(tabplot) # tabplot::tableplot
library(RColorBrewer) # brewer.pal

# if (!require("RColorBrewer")) {
#   install.packages("RColorBrewer")
#   library(RColorBrewer)
# }

# dataset load as a tibble
my_tb <- read_csv("Ass1Data.csv")

# column lists
all_cols <- colnames(my_tb)
cate_cols <- all_cols[3:14][-2]
default_cate_cols <- cate_cols[1:4]
num_cols <- all_cols[-(2:14)]
default_num_cols <- num_cols[2:5]
default_tab_cols <- all_cols[c(2:4,17,18,27,31,36,38,41)]
sort_cols <- all_cols[1:14][-4]
choice_tab_col <- c("rownum","ID","Date","Author")

# subset of dataset, also tibble
my_numeric_data <- my_tb %>% select(1,15:44)
my_cate_data <- my_tb %>% select(3,5:14)
my_cate_data <- my_tb %>% select(5:9)
my_cate_data <- my_cate_data %>%
  mutate(Priority = case_when(
    Priority == "Low" & !is.na(Priority) ~ 1,
    Priority == "Medium" & !is.na(Priority) ~ 2,
    Priority == "High" & !is.na(Priority) ~ 3
  ))

my_cate_data <- my_cate_data %>%
  mutate(Price = case_when(
    Price == "Cheap" & !is.na(Price) ~ 1,
    Price == "Costly" & !is.na(Price) ~ 2,
    Price == "Extravagant" & !is.na(Price) ~ 3
  ))

my_cate_data <- my_cate_data %>%
  mutate(Speed = case_when(
    Speed == "Slow" & !is.na(Speed) ~ 1,
    Speed == "Medium" & !is.na(Speed) ~ 2,
    Speed == "Fast" & !is.na(Speed) ~ 3
  ))

my_cate_data <- my_cate_data %>%
  mutate(Duration = case_when(
    Duration == "Short" & !is.na(Duration) ~ 1,
    Duration == "Long" & !is.na(Duration) ~ 2,
    Duration == "Very Long" & !is.na(Duration) ~ 3
  ))

my_cate_data <- my_cate_data %>%
  mutate(Temp = case_when(
    Temp == "Cold" & !is.na(Temp) ~ 1,
    Temp == "Warm" & !is.na(Temp) ~ 2,
    Temp == "Hot" & !is.na(Temp) ~ 3
  ))

sensor_group_col1 <- colnames(my_numeric_data[c(1,2,3,6:11)])
sensor_group_col2 <- colnames(my_numeric_data[c(4,5,14,18,23,25,28)])
sensor_group_col3 <- colnames(my_numeric_data[c(12,13,15:17,19:21)])
sensor_group_col4 <- colnames(my_numeric_data[c(22,24,26,27,29:31)])

spear_group_col1 <- colnames(my_numeric_data[c(1,2:11)])
spear_group_col2 <- colnames(my_numeric_data[c(12:21)])
spear_group_col3 <- colnames(my_numeric_data[c(22:31)])