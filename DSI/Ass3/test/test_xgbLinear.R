# The recipes package is central to preprocessing
library(recipes)
library(caret)
if(!require(doParallel)) install.packages('doParallel')


setwd("~/Workspace/Repositories/Rcode/DSI/Ass3")

steps <- function(recipe, preprocess) {
  for (s in preprocess) {
    if (s == "knnimpute") {
      recipe <- step_knnimpute(recipe, all_predictors(), k = 5) # 5 is a reasonable guess
    } else if (s == "bagimpute") {
      recipe <- step_bagimpute(recipe, all_predictors())
    } else if (s == "medianimpute") {
      recipe <- step_medianimpute(recipe, all_predictors(), -all_nominal())
    } else if (s == "YeoJohnson") {
      recipe <- step_YeoJohnson(recipe, all_predictors(), -all_nominal())
    } else if (s == "naomit") {
      recipe <- step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- step_pca(recipe, all_predictors(), -all_nominal(), threshold = 0.95)
    } else if (s == "pls") {
      recipe <- step_pls(recipe, all_predictors(), -all_nominal(), outcome = "Y", num_comp = 25)
    } else if (s == "ica") {
      recipe <- step_ica(recipe, all_predictors(), -all_nominal())
    } else if (s == "center") {
      recipe <- step_center(recipe, all_predictors(), -all_nominal())
    } else if (s == "scale") {
      recipe <- step_scale(recipe, all_predictors(), -all_nominal())
    } else if (s == "nzv") {
      recipe <- step_nzv(recipe, all_predictors(), -all_nominal())
    } else if (s == "other") {
      recipe <- step_nzv(recipe, all_predictors(), -all_numeric())
    } else if (s == "dummy") {
      recipe <- step_dummy(recipe, all_predictors(), -all_numeric(), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- step_poly(recipe, all_predictors(), -all_nominal(), options = list(degree = 2))
    }
  }
  recipe
}

startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

df <-  read.csv(file = "Ass3Data.csv", row.names = "ID")

myPreprocess <-  c("knnimpute","dummy")

getSplit <- function() {
  createDataPartition(y = df$Y, p = 0.8, list = FALSE)
}

getTrainData <- function(){
  df[getSplit(),]
}

getRecipe <- function() {
  recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), myPreprocess)
}

getTrControl <- function() {
  # shared bootstrap specification i.e. 25 x bootstrap
  y <- getTrainData()[,"Y"]
  n <- 25
  trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
               index = caret::createResample(y = y, times = n), savePredictions = "final")
}

stepped <- getRecipe() %>% prep(data = getTrainData()) %>% bake(df)
str(stepped)
##############################################################################
library(xgboost)
# if(!require(Ckmeans.1d.dp)) install.packages('Ckmeans.1d.dp')

method <- "xgbLinear"
tr_res <- list()
tr_res[[method]] <- NULL
clus <- startMode(TRUE)
tryCatch({
  tr_res[[method]] <- caret::train(getRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
},
finally = {
  stopMode(clus)
})

tr_res$xgbLinear$results[ which.min(tr_res$xgbLinear$results[, "RMSE"]), ]
tr_res$xgbLinear$times
tr_res$xgbLinear$recipe
plot(tr_res$xgbLinear)


