# load shiny
if(!require(shiny)) install.packages("shiny")
if(!require(shinythemes)) install.packages("shinythemes")

# load base packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dplyr)) install.packages("dplyr")

# load visualise packages
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(visdat)) install.packages("visdat")
if(!require(GGally)) install.packages("GGally")

# load statistic packages
if(!require(MASS)) install.packages("MASS")
if(!require(pls)) install.packages("pls")
#if(!require(corrgram)) install.packages("corrgram")
if(!require(caret)) install.packages("caret")
if(!require(recipes)) install.packages("recipes")
#if(!require(factoextra)) devtools::install_github("kassambara/factoextra")
#if(!require(leaps)) install.packages("leaps") # for regsubsets, but failed
if(!require(glmnet)) install.packages("glmnet")


# load data set
data(mayonnaise)

# NIR matrix
nir <- mayonnaise$NIR
nir_msc <- msc(mayonnaise$NIR)

# Pre-process NIR by using msc()
mayo <- data.frame(msc(mayonnaise$NIR), oil.type = mayonnaise$oil.type, train=mayonnaise$train)
mayo_train <- mayo %>% filter(train) %>% dplyr::select(-train)
mayo_test <- mayo %>% filter(!train) %>% dplyr::select(-train)

# cols
cols_mayo <- names(mayo)

# mods
y_train <- mayo_train$oil.type
x_train <- model.matrix(oil.type~., mayo_train)[,-1]
y_test <- mayo_test$oil.type
x_test <- model.matrix(oil.type~., mayo_test)[,-1]
grid <-  10^seq(10, -2, length=100)




