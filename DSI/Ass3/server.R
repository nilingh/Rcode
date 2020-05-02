shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################

  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################

  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  ############################################################################## 
  output$GlmnetTimeConsuming <- renderPrint({
    req(models$glmnet)
    et <- time_string(models$glmnet$times$everything[["elapsed"]])
    ft <- time_string(models$glmnet$times$final[["elapsed"]])
    pt <- time_string(models$glmnet$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })
  
  
############################################################ PLS ########################################################

  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  ############################################################################## 
  output$PLSTimeConsuming <- renderPrint({
    req(models$glmnet)
    et <- time_string(models$pls$times$everything[["elapsed"]])
    ft <- time_string(models$pls$times$final[["elapsed"]])
    pt <- time_string(models$pls$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })

  
############################################################ RPART ########################################################
 
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  
  ############################################################################## 
  output$RpartTimeConsuming <- renderPrint({
    req(models$glmnet)
    et <- time_string(models$rpart$times$everything[["elapsed"]])
    ft <- time_string(models$rpart$times$final[["elapsed"]])
    pt <- time_string(models$rpart$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })
  
  
######################################################### maintenance point ####################################################

############################################################ xgbLinear ########################################################
  getxgbLinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xgbLinearPreprocess)
  })

  observeEvent(
    input$xgbLinearGo,
    {
      library(xgboost)
      method <- "xgbLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getxgbLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "xgbLinear")
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  output$xgbLinearModelSummary0 <- renderText({
    description("xgbLinear")
  })

  output$xgbLinearMetrics <- renderTable({
    req(models$xgbLinear)
    models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
  })

  output$xgbLinearRecipe <- renderPrint({
    req(models$xgbLinear)
    models$xgbLinear$recipe
  })

  output$xgbLinearModelPlots <- renderPlot({
    req(models$xgbLinear)
    plot(models$xgbLinear)
  })
  
  output$xgbLinearModelSummary2 <- renderPrint({
    req(models$xgbLinear)
    print(models$xgbLinear)
  })

  output$xgbLinearTimeConsuming <- renderPrint({
    req(models$xgbLinear)
    et <- time_string(models$xgbLinear$times$everything[["elapsed"]])
    ft <- time_string(models$xgbLinear$times$final[["elapsed"]])
    pt <- time_string(models$xgbLinear$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })
          
############################################################ rf ########################################################
  getRfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RfPreprocess)
  })
  
  observeEvent(
    input$RfGo,
    {
      library(randomForest)
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      # rf_trainControl <- trainControl(method = "oob", allowParallel = TRUE, search = "random")
      tryCatch({
        models[[method]] <- caret::train(getRfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rf")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$RfModelSummary0 <- renderText({
    description("rf")
  })
  
  output$RfMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  output$RfRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  output$RfModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  output$RfFinalModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf$finalModel)
  })

  output$RfPredictorsPlot <- renderPlot({
    req(models$rf)
    plot(varImp(models$rf))
  })
  
  output$RfModelSummary2 <- renderPrint({
    req(models$rf)
    print(models$rf)
  })

  output$RfTimeConsuming <- renderPrint({
    req(models$rf)
    et <- time_string(models$rf$times$everything[["elapsed"]])
    ft <- time_string(models$rf$times$final[["elapsed"]])
    pt <- time_string(models$rf$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })  
  
############################################################ gbm ########################################################
  getgbmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gbmPreprocess)
  })
  
  observeEvent(
    input$gbmGo,
    {
      library(gbm)
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgbmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "gbm")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$gbmModelSummary0 <- renderText({
    description("gbm")
  })
  
  output$gbmMetrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
  })
  
  output$gbmRecipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })  
  
  output$gbmModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })
  
  output$gbmFinalModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm$finalModel)
  })
  
  output$gbmModelSummary2 <- renderPrint({
    req(models$gbm)
    print(models$gbm)
  })
  
  output$gbmTimeConsuming <- renderPrint({
    req(models$gbm)
    et <- time_string(models$gbm$times$everything[["elapsed"]])
    ft <- time_string(models$gbm$times$final[["elapsed"]])
    pt <- time_string(models$gbm$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })  
  
  



############################################################ lasso ########################################################
  getlassoRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$lassoPreprocess)
  })
  
  observeEvent(
    input$lassoGo,
    {
      library(elasticnet)
      method <- "lasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "lasso")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$lassoModelSummary0 <- renderText({
    description("lasso")
  })
  
  output$lassoMetrics <- renderTable({
    req(models$lasso)
    models$lasso$results[ which.min(models$lasso$results[, "RMSE"]), ]
  })
  
  output$lassoRecipe <- renderPrint({
    req(models$lasso)
    models$lasso$recipe
  })  
  
  output$lassoModelPlots <- renderPlot({
    req(models$lasso)
    plot(models$lasso)
  })
  
  output$lassoFinalModelPlots <- renderPlot({
    req(models$lasso)
    plot(models$lasso$finalModel)
  })
  
  output$lassoModelSummary2 <- renderPrint({
    req(models$lasso)
    print(models$lasso)
  })
  
  output$lassoTimeConsuming <- renderPrint({
    req(models$lasso)
    et <- time_string(models$lasso$times$everything[["elapsed"]])
    ft <- time_string(models$lasso$times$final[["elapsed"]])
    pt <- time_string(models$lasso$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })  
  
  
  
  
############################################################ ridge ########################################################
  getridgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ridgePreprocess)
  })
  
  observeEvent(
    input$ridgeGo,
    {
      library(elasticnet)
      method <- "ridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getridgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "ridge")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$ridgeModelSummary0 <- renderText({
    description("ridge")
  })
  
  output$ridgeMetrics <- renderTable({
    req(models$ridge)
    models$ridge$results[ which.min(models$ridge$results[, "RMSE"]), ]
  })
  
  output$ridgeRecipe <- renderPrint({
    req(models$ridge)
    models$ridge$recipe
  })  
  
  output$ridgeModelPlots <- renderPlot({
    req(models$ridge)
    plot(models$ridge)
  })
  
  output$ridgeFinalModelPlots <- renderPlot({
    req(models$ridge)
    plot(models$ridge$finalModel)
  })
  
  output$ridgeModelSummary2 <- renderPrint({
    req(models$ridge)
    print(models$ridge)
  })
  
  output$ridgeTimeConsuming <- renderPrint({
    req(models$ridge)
    et <- time_string(models$ridge$times$everything[["elapsed"]])
    ft <- time_string(models$ridge$times$final[["elapsed"]])
    pt <- time_string(models$ridge$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })  
  

############################################################ kernelpls ########################################################
  getkernelplsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$kernelplsPreprocess)
  })
  
  observeEvent(
    input$kernelplsGo,
    {
      library(pls)
      method <- "kernelpls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getkernelplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "kernelpls")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$kernelplsModelSummary0 <- renderText({
    description("kernelpls")
  })
  
  output$kernelplsMetrics <- renderTable({
    req(models$kernelpls)
    models$kernelpls$results[ which.min(models$kernelpls$results[, "RMSE"]), ]
  })
  
  output$kernelplsRecipe <- renderPrint({
    req(models$kernelpls)
    models$kernelpls$recipe
  })  
  
  output$kernelplsModelPlots <- renderPlot({
    req(models$kernelpls)
    plot(models$kernelpls)
  })
  
  output$kernelplsFinalModelPlots <- renderPlot({
    req(models$kernelpls)
    plot(models$kernelpls$finalModel)
  })
  
  output$kernelplsModelSummary2 <- renderPrint({
    req(models$kernelpls)
    print(models$kernelpls)
  })
  
  output$kernelplsTimeConsuming <- renderPrint({
    req(models$kernelpls)
    et <- time_string(models$kernelpls$times$everything[["elapsed"]])
    ft <- time_string(models$kernelpls$times$final[["elapsed"]])
    pt <- time_string(models$kernelpls$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })  
  
############################################################ glmboost ########################################################
  getglmboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmboostPreprocess)
  })
  
  observeEvent(
    input$glmboostGo,
    {
      library(pls)
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "glmboost")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$glmboostModelSummary0 <- renderText({
    description("glmboost")
  })
  
  output$glmboostMetrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[ which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  output$glmboostRecipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  output$glmboostModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })
  
  output$glmboostFinalModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost$finalModel)
  })
  
  output$glmboostModelSummary2 <- renderPrint({
    req(models$glmboost)
    print(models$glmboost)
  })
  
  output$glmboostTimeConsuming <- renderPrint({
    req(models$glmboost)
    et <- time_string(models$glmboost$times$everything[["elapsed"]])
    ft <- time_string(models$glmboost$times$final[["elapsed"]])
    pt <- time_string(models$glmboost$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
############################################################ svmLinear ########################################################
  getsvmLinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmLinearPreprocess)
  })
  
  observeEvent(
    input$svmLinearGo,
    {
      library(kernlab)
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "svmLinear")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$svmLinearModelSummary0 <- renderText({
    description("svmLinear")
  })
  
  output$svmLinearMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  output$svmLinearRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  output$svmLinearModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  output$svmLinearFinalModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear$finalModel)
  })
  
  output$svmLinearModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })
  
  output$svmLinearTimeConsuming <- renderPrint({
    req(models$svmLinear)
    et <- time_string(models$svmLinear$times$everything[["elapsed"]])
    ft <- time_string(models$svmLinear$times$final[["elapsed"]])
    pt <- time_string(models$svmLinear$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
 ######################################################### End of maintenance point ####################################################
  
  
  
  
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
