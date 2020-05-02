library(recipes)
library(modeldata)

setwd("~/Workspace/Repositories/Rcode/DSI/Ass3")

my_data <-  read.csv(file = "Ass3Data.csv", row.names = "ID")

sc_estimates <- recipe(Y ~ ., data = my_data) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>% 
  prep(data = my_data)  # "prep" trains the recipe (that holds the YJ transform step)

sc_yj_estimates <- recipe(Y ~ ., data = my_data) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>% 
  prep(data = my_data)  # "prep" trains the recipe (that holds the YJ transform step)

sc_all_estimates <- recipe(Y ~ ., data = my_data) %>%
  # step_bagimpute(all_numeric()) %>%
  step_YeoJohnson(all_predictors(),-all_nominal()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>% 
  step_dummy(recipe, all_predictors(), -all_numeric(), one_hot = FALSE) %>%
  prep(data = my_data)  # "prep" trains the recipe (that holds the YJ transform step)

sc_ummy <- recipe(Y ~ ., data = my_data) %>%
  step_dummy(recipe, all_predictors(), -all_numeric(), one_hot = FALSE) %>%
  prep(data = my_data)  # "prep" trains the recipe (that holds the YJ transform step)


par(mfrow=c(2,2))
plot(density(na.omit(my_data$ReagentN)), main = "Sulfur before YJ transform")

sc_te <- bake(sc_estimates, my_data)  # bake runs the trained-transform on some data
plot(density(na.omit(sc_te$ReagentH)), main = "Sulfur after YJ transform")

sc_yj_te <- bake(sc_yj_estimates, my_data)  # bake runs the trained-transform on some data
plot(density(na.omit(sc_te$ReagentH)), main = "Sulfur after SC transform")

sc_all_te <- bake(sc_all_estimates, my_data)  # bake runs the trained-transform on some data
plot(density(sc_all_te$ReagentH), main = "Sulfur after SC transform")

