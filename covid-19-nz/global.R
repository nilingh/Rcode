# if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")

library(shiny)
library(tidyverse)
library(DT) # DT::datatable
library(ggplot2)
# library(hrbrthemes)

# dataset load as a tibble
my_raw_data <- readxl::read_xlsx("cases20200324.xlsx")
my_raw_data <- my_raw_data[,-1]

# tibble
my_grouped_data <- my_raw_data %>% group_by(AGE,GENDER) %>% count() %>% rename(NUMBER_OF_CASES=n)