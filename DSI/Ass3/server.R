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
  
  # ############################################################################## 
  # output$PredictorPlot1 <- renderPlot({
  #   d <- getData()
  #   numeric <- sapply(d, FUN = is.numeric)
  #   req(d, length(numeric) > 0)
  #   GGally::ggpairs(data = d[,c(1:5,10,18,20)])
  # })
  # 
  # ############################################################################## 
  # output$PredictorPlot2 <- renderPlot({
  #   d <- getData()
  #   numeric <- sapply(d, FUN = is.numeric)
  #   req(d, length(numeric) > 0)
  #   GGally::ggpairs(data = d[,c(1:5,7,16,20)])
  # })
  # 
  # ############################################################################## 
  # output$PredictorPlot3 <- renderPlot({
  #   d <- getData()
  #   numeric <- sapply(d, FUN = is.numeric)
  #   req(d, length(numeric) > 0)
  #   GGally::ggpairs(data = d[,c(5,6,8,12,14,20)])
  # })
  # 
  # ############################################################################## 
  # output$PredictorPlot4 <- renderPlot({
  #   d <- getData()
  #   numeric <- sapply(d, FUN = is.numeric)
  #   req(d, length(numeric) > 0)
  #   GGally::ggpairs(data = d[,c(5,9,11,13,15,17,19,20)])
  # })
  
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
    plot(models$xgbLinear$finalModel)
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
  
  
  
  

############################################################ gaussprLinear ########################################################
  getgaussprLinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprLinearPreprocess)
  })
  
  observeEvent(
    input$gaussprLinearGo,
    {
      library(kernlab)
      method <- "gaussprLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "gaussprLinear")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$gaussprLinearModelSummary0 <- renderText({
    description("gaussprLinear")
  })
  
  output$gaussprLinearMetrics <- renderTable({
    req(models$gaussprLinear)
    models$gaussprLinear$results[ which.min(models$gaussprLinear$results[, "RMSE"]), ]
  })
  
  output$gaussprLinearRecipe <- renderPrint({
    req(models$gaussprLinear)
    models$gaussprLinear$recipe
  })  
  
  output$gaussprLinearModelPlots <- renderPlot({
    req(models$gaussprLinear)
    plot(models$gaussprLinear)
  })
  
  output$gaussprLinearFinalModelPlots <- renderPlot({
    req(models$gaussprLinear)
    plot(models$gaussprLinear$finalModel)
  })
  
  output$gaussprLinearModelSummary2 <- renderPrint({
    req(models$gaussprLinear)
    print(models$gaussprLinear)
  })
  
  output$gaussprLinearTimeConsuming <- renderPrint({
    req(models$gaussprLinear)
    et <- time_string(models$gaussprLinear$times$everything[["elapsed"]])
    ft <- time_string(models$gaussprLinear$times$final[["elapsed"]])
    pt <- time_string(models$gaussprLinear$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
############################################################ mlpML ########################################################
  getmlpMLRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$mlpMLPreprocess)
  })
  
  observeEvent(
    input$mlpMLGo,
    {
      library(RSNNS)
      method <- "mlpML"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getmlpMLRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "mlpML")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$mlpMLModelSummary0 <- renderText({
    description("mlpML")
  })
  
  output$mlpMLMetrics <- renderTable({
    req(models$mlpML)
    models$mlpML$results[ which.min(models$mlpML$results[, "RMSE"]), ]
  })
  
  output$mlpMLRecipe <- renderPrint({
    req(models$mlpML)
    models$mlpML$recipe
  })  
  
  output$mlpMLModelPlots <- renderPlot({
    req(models$mlpML)
    plot(models$mlpML)
  })
  
  output$mlpMLFinalModelPlots <- renderPlot({
    req(models$mlpML)
    plot(models$mlpML$finalModel)
  })
  
  output$mlpMLModelSummary2 <- renderPrint({
    req(models$mlpML)
    print(models$mlpML)
  })
  
  output$mlpMLTimeConsuming <- renderPrint({
    req(models$mlpML)
    et <- time_string(models$mlpML$times$everything[["elapsed"]])
    ft <- time_string(models$mlpML$times$final[["elapsed"]])
    pt <- time_string(models$mlpML$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
############################################################ xyf ########################################################
  getxyfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xyfPreprocess)
  })
  
  observeEvent(
    input$xyfGo,
    {
      library(kohonen)
      method <- "xyf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getxyfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "xyf")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$xyfModelSummary0 <- renderText({
    description("xyf")
  })
  
  output$xyfMetrics <- renderTable({
    req(models$xyf)
    models$xyf$results[ which.min(models$xyf$results[, "RMSE"]), ]
  })
  
  output$xyfRecipe <- renderPrint({
    req(models$xyf)
    models$xyf$recipe
  })  
  
  output$xyfModelPlots <- renderPlot({
    req(models$xyf)
    plot(models$xyf)
  })
  
  output$xyfFinalModelPlots <- renderPlot({
    req(models$xyf)
    plot(models$xyf$finalModel)
  })
  
  output$xyfModelSummary2 <- renderPrint({
    req(models$xyf)
    print(models$xyf)
  })
  
  output$xyfTimeConsuming <- renderPrint({
    req(models$xyf)
    et <- time_string(models$xyf$times$everything[["elapsed"]])
    ft <- time_string(models$xyf$times$final[["elapsed"]])
    pt <- time_string(models$xyf$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
############################################################ kknn ########################################################
  getkknnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$kknnPreprocess)
  })
  
  observeEvent(
    input$kknnGo,
    {
      library(kknn)
      method <- "kknn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getkknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "kknn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$kknnModelSummary0 <- renderText({
    description("kknn")
  })
  
  output$kknnMetrics <- renderTable({
    req(models$kknn)
    models$kknn$results[ which.min(models$kknn$results[, "RMSE"]), ]
  })
  
  output$kknnRecipe <- renderPrint({
    req(models$kknn)
    models$kknn$recipe
  })  
  
  output$kknnModelPlots <- renderPlot({
    req(models$kknn)
    plot(models$kknn)
  })
  
  output$kknnFinalModelPlots <- renderPlot({
    req(models$kknn)
    plot(models$kknn$finalModel)
  })
  
  output$kknnModelSummary2 <- renderPrint({
    req(models$kknn)
    print(models$kknn)
  })
  
  output$kknnTimeConsuming <- renderPrint({
    req(models$kknn)
    et <- time_string(models$kknn$times$everything[["elapsed"]])
    ft <- time_string(models$kknn$times$final[["elapsed"]])
    pt <- time_string(models$kknn$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
############################################################ brnn ########################################################
  getbrnnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$brnnPreprocess)
  })
  
  observeEvent(
    input$brnnGo,
    {
      library(brnn)
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "brnn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$brnnModelSummary0 <- renderText({
    description("brnn")
  })
  
  output$brnnMetrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  output$brnnRecipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  output$brnnModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  output$brnnFinalModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn$finalModel)
  })
  
  output$brnnModelSummary2 <- renderPrint({
    req(models$brnn)
    print(models$brnn)
  })
  
  output$brnnTimeConsuming <- renderPrint({
    req(models$brnn)
    et <- time_string(models$brnn$times$everything[["elapsed"]])
    ft <- time_string(models$brnn$times$final[["elapsed"]])
    pt <- time_string(models$brnn$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
  
############################################################ qrnn ########################################################
  getqrnnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$qrnnPreprocess)
  })
  
  observeEvent(
    input$qrnnGo,
    {
      library(qrnn)
      method <- "qrnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getqrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "qrnn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$qrnnModelSummary0 <- renderText({
    description("qrnn")
  })
  
  output$qrnnMetrics <- renderTable({
    req(models$qrnn)
    models$qrnn$results[ which.min(models$qrnn$results[, "RMSE"]), ]
  })
  
  output$qrnnRecipe <- renderPrint({
    req(models$qrnn)
    models$qrnn$recipe
  })  
  
  output$qrnnModelPlots <- renderPlot({
    req(models$qrnn)
    plot(models$qrnn)
  })
  
  output$qrnnFinalModelPlots <- renderPlot({
    req(models$qrnn)
    plot(models$qrnn$finalModel)
  })
  
  output$qrnnModelSummary2 <- renderPrint({
    req(models$qrnn)
    print(models$qrnn)
  })
  
  output$qrnnTimeConsuming <- renderPrint({
    req(models$qrnn)
    et <- time_string(models$qrnn$times$everything[["elapsed"]])
    ft <- time_string(models$qrnn$times$final[["elapsed"]])
    pt <- time_string(models$qrnn$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
############################################################ rvmRadial ########################################################
  getrvmRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rvmRadialPreprocess)
  })
  
  observeEvent(
    input$rvmRadialGo,
    {
      library(kernlab)
      method <- "rvmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getrvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rvmRadial")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$rvmRadialModelSummary0 <- renderText({
    description("rvmRadial")
  })
  
  output$rvmRadialMetrics <- renderTable({
    req(models$rvmRadial)
    models$rvmRadial$results[ which.min(models$rvmRadial$results[, "RMSE"]), ]
  })
  
  output$rvmRadialRecipe <- renderPrint({
    req(models$rvmRadial)
    models$rvmRadial$recipe
  })  
  
  output$rvmRadialModelPlots <- renderPlot({
    req(models$rvmRadial)
    plot(models$rvmRadial)
  })
  
  output$rvmRadialFinalModelPlots <- renderPlot({
    req(models$rvmRadial)
    plot(models$rvmRadial$finalModel)
  })
  
  output$rvmRadialModelSummary2 <- renderPrint({
    req(models$rvmRadial)
    print(models$rvmRadial)
  })
  
  output$rvmRadialTimeConsuming <- renderPrint({
    req(models$rvmRadial)
    et <- time_string(models$rvmRadial$times$everything[["elapsed"]])
    ft <- time_string(models$rvmRadial$times$final[["elapsed"]])
    pt <- time_string(models$rvmRadial$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
############################################################ glmStepAIC ########################################################
  getglmStepAICRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmStepAICPreprocess)
  })
  
  observeEvent(
    input$glmStepAICGo,
    {
      library(MASS)
      method <- "glmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getglmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "glmStepAIC")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$glmStepAICModelSummary0 <- renderText({
    description("glmStepAIC")
  })
  
  output$glmStepAICMetrics <- renderTable({
    req(models$glmStepAIC)
    models$glmStepAIC$results[ which.min(models$glmStepAIC$results[, "RMSE"]), ]
  })
  
  output$glmStepAICRecipe <- renderPrint({
    req(models$glmStepAIC)
    models$glmStepAIC$recipe
  })  
  
  output$glmStepAICModelPlots <- renderPlot({
    req(models$glmStepAIC)
    plot(models$glmStepAIC)
  })
  
  output$glmStepAICFinalModelPlots <- renderPlot({
    req(models$glmStepAIC)
    plot(models$glmStepAIC$finalModel)
  })
  
  output$glmStepAICModelSummary2 <- renderPrint({
    req(models$glmStepAIC)
    print(models$glmStepAIC)
  })
  
  output$glmStepAICTimeConsuming <- renderPrint({
    req(models$glmStepAIC)
    et <- time_string(models$glmStepAIC$times$everything[["elapsed"]])
    ft <- time_string(models$glmStepAIC$times$final[["elapsed"]])
    pt <- time_string(models$glmStepAIC$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
############################################################ pcaNNet ########################################################
  getpcaNNetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$pcaNNetPreprocess)
  })
  
  observeEvent(
    input$pcaNNetGo,
    {
      library(nnet)
      method <- "pcaNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getpcaNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "pcaNNet")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$pcaNNetModelSummary0 <- renderText({
    description("pcaNNet")
  })
  
  output$pcaNNetMetrics <- renderTable({
    req(models$pcaNNet)
    models$pcaNNet$results[ which.min(models$pcaNNet$results[, "RMSE"]), ]
  })
  
  output$pcaNNetRecipe <- renderPrint({
    req(models$pcaNNet)
    models$pcaNNet$recipe
  })  
  
  output$pcaNNetModelPlots <- renderPlot({
    req(models$pcaNNet)
    plot(models$pcaNNet)
  })
  
  output$pcaNNetFinalModelPlots <- renderPlot({
    req(models$pcaNNet)
    plot(models$pcaNNet$finalModel)
  })
  
  output$pcaNNetModelSummary2 <- renderPrint({
    req(models$pcaNNet)
    print(models$pcaNNet)
  })
  
  output$pcaNNetTimeConsuming <- renderPrint({
    req(models$pcaNNet)
    et <- time_string(models$pcaNNet$times$everything[["elapsed"]])
    ft <- time_string(models$pcaNNet$times$final[["elapsed"]])
    pt <- time_string(models$pcaNNet$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
############################################################ GFS.LT.RS ########################################################
  getGFS.LT.RSRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GFS.LT.RSPreprocess)
  })
  
  observeEvent(
    input$GFS.LT.RSGo,
    {
      library(frbs)
      method <- "GFS.LT.RS"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGFS.LT.RSRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "GFS.LT.RS")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$GFS.LT.RSModelSummary0 <- renderText({
    description("GFS.LT.RS")
  })
  
  output$GFS.LT.RSMetrics <- renderTable({
    req(models$GFS.LT.RS)
    models$GFS.LT.RS$results[ which.min(models$GFS.LT.RS$results[, "RMSE"]), ]
  })
  
  output$GFS.LT.RSRecipe <- renderPrint({
    req(models$GFS.LT.RS)
    models$GFS.LT.RS$recipe
  })  
  
  output$GFS.LT.RSModelPlots <- renderPlot({
    req(models$GFS.LT.RS)
    plot(models$GFS.LT.RS)
  })
  
  output$GFS.LT.RSFinalModelPlots <- renderPlot({
    req(models$GFS.LT.RS)
    plot(models$GFS.LT.RS$finalModel)
  })
  
  output$GFS.LT.RSModelSummary2 <- renderPrint({
    req(models$GFS.LT.RS)
    print(models$GFS.LT.RS)
  })
  
  output$GFS.LT.RSTimeConsuming <- renderPrint({
    req(models$GFS.LT.RS)
    et <- time_string(models$GFS.LT.RS$times$everything[["elapsed"]])
    ft <- time_string(models$GFS.LT.RS$times$final[["elapsed"]])
    pt <- time_string(models$GFS.LT.RS$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
############################################################ spikeslab ########################################################
  getspikeslabRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$spikeslabPreprocess)
  })
  
  observeEvent(
    input$spikeslabGo,
    {
      library(spikeslab)
      method <- "spikeslab"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getspikeslabRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "spikeslab")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$spikeslabModelSummary0 <- renderText({
    description("spikeslab")
  })
  
  output$spikeslabMetrics <- renderTable({
    req(models$spikeslab)
    models$spikeslab$results[ which.min(models$spikeslab$results[, "RMSE"]), ]
  })
  
  output$spikeslabRecipe <- renderPrint({
    req(models$spikeslab)
    models$spikeslab$recipe
  })  
  
  output$spikeslabModelPlots <- renderPlot({
    req(models$spikeslab)
    plot(models$spikeslab)
  })
  
  output$spikeslabFinalModelPlots <- renderPlot({
    req(models$spikeslab)
    plot(models$spikeslab$finalModel)
  })
  
  output$spikeslabModelSummary2 <- renderPrint({
    req(models$spikeslab)
    print(models$spikeslab)
  })
  
  output$spikeslabTimeConsuming <- renderPrint({
    req(models$spikeslab)
    et <- time_string(models$spikeslab$times$everything[["elapsed"]])
    ft <- time_string(models$spikeslab$times$final[["elapsed"]])
    pt <- time_string(models$spikeslab$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
  
############################################################ bayesglm ########################################################
  getbayesglmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bayesglmPreprocess)
  })
  
  observeEvent(
    input$bayesglmGo,
    {
      library(arm)
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbayesglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "bayesglm")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$bayesglmModelSummary0 <- renderText({
    description("bayesglm")
  })
  
  output$bayesglmMetrics <- renderTable({
    req(models$bayesglm)
    models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  })
  
  output$bayesglmRecipe <- renderPrint({
    req(models$bayesglm)
    models$bayesglm$recipe
  })  
  
  output$bayesglmModelPlots <- renderPlot({
    req(models$bayesglm)
    plot(models$bayesglm)
  })
  
  output$bayesglmFinalModelPlots <- renderPlot({
    req(models$bayesglm)
    plot(models$bayesglm$finalModel)
  })
  
  output$bayesglmModelSummary2 <- renderPrint({
    req(models$bayesglm)
    print(models$bayesglm)
  })
  
  output$bayesglmTimeConsuming <- renderPrint({
    req(models$bayesglm)
    et <- time_string(models$bayesglm$times$everything[["elapsed"]])
    ft <- time_string(models$bayesglm$times$final[["elapsed"]])
    pt <- time_string(models$bayesglm$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
  
############################################################ rqlasso ########################################################
  getrqlassoRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rqlassoPreprocess)
  })
  
  observeEvent(
    input$rqlassoGo,
    {
      library(rqPen)
      method <- "rqlasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getrqlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rqlasso")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$rqlassoModelSummary0 <- renderText({
    description("rqlasso")
  })
  
  output$rqlassoMetrics <- renderTable({
    req(models$rqlasso)
    models$rqlasso$results[ which.min(models$rqlasso$results[, "RMSE"]), ]
  })
  
  output$rqlassoRecipe <- renderPrint({
    req(models$rqlasso)
    models$rqlasso$recipe
  })  
  
  output$rqlassoModelPlots <- renderPlot({
    req(models$rqlasso)
    plot(models$rqlasso)
  })
  
  output$rqlassoFinalModelPlots <- renderPlot({
    req(models$rqlasso)
    plot(models$rqlasso$finalModel)
  })
  
  output$rqlassoModelSummary2 <- renderPrint({
    req(models$rqlasso)
    print(models$rqlasso)
  })
  
  output$rqlassoTimeConsuming <- renderPrint({
    req(models$rqlasso)
    et <- time_string(models$rqlasso$times$everything[["elapsed"]])
    ft <- time_string(models$rqlasso$times$final[["elapsed"]])
    pt <- time_string(models$rqlasso$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
  
############################################################ blasso ########################################################
  getblassoRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$blassoPreprocess)
  })
  
  observeEvent(
    input$blassoGo,
    {
      library(monomvn)
      method <- "blasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getblassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "blasso")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$blassoModelSummary0 <- renderText({
    description("blasso")
  })
  
  output$blassoMetrics <- renderTable({
    req(models$blasso)
    models$blasso$results[ which.min(models$blasso$results[, "RMSE"]), ]
  })
  
  output$blassoRecipe <- renderPrint({
    req(models$blasso)
    models$blasso$recipe
  })  
  
  output$blassoModelPlots <- renderPlot({
    req(models$blasso)
    plot(models$blasso)
  })
  
  output$blassoFinalModelPlots <- renderPlot({
    req(models$blasso)
    plot(models$blasso$finalModel)
  })
  
  output$blassoModelSummary2 <- renderPrint({
    req(models$blasso)
    print(models$blasso)
  })
  
  output$blassoTimeConsuming <- renderPrint({
    req(models$blasso)
    et <- time_string(models$blasso$times$everything[["elapsed"]])
    ft <- time_string(models$blasso$times$final[["elapsed"]])
    pt <- time_string(models$blasso$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   
  
  
  
  
  
  

############################################################ cubist ########################################################
  getcubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$cubistPreprocess)
  })
  
  observeEvent(
    input$cubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "cubist")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$cubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  output$cubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  output$cubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  output$cubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  output$cubistFinalModelPlots <- renderPlot({
    req(models$cubist)
    dotplot(models$cubist$finalModel, what='splits')
  })
  
  output$cubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  output$cubistTimeConsuming <- renderPrint({
    req(models$cubist)
    et <- time_string(models$cubist$times$everything[["elapsed"]])
    ft <- time_string(models$cubist$times$final[["elapsed"]])
    pt <- time_string(models$cubist$times$prediction[3])
    print(paste("Everything:", et, "Final:", ft, "Prediction:", pt))
  })   

############################################################ qrf ########################################################
  getqrfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$qrfPreprocess)
  })
  
  observeEvent(
    input$qrfGo,
    {
      library(quantregForest)
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "qrf")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  output$qrfModelSummary0 <- renderText({
    description("qrf")
  })
  
  output$qrfMetrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  output$qrfRecipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  output$qrfModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  output$qrfFinalModelPlots <- renderPlot({
    req(models$qrf)
    dotplot(models$qrf$finalModel, what='splits')
  })
  
  output$qrfModelSummary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
  output$qrfTimeConsuming <- renderPrint({
    req(models$qrf)
    et <- time_string(models$qrf$times$everything[["elapsed"]])
    ft <- time_string(models$qrf$times$final[["elapsed"]])
    pt <- time_string(models$qrf$times$prediction[3])
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
