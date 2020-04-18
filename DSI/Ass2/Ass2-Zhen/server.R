# The Server part of a Shiny app

# Course: DATA423-20S1
# Task: Assignment 2
# Student: Zhen Huang
# ID: 74093323

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ##########################
    output$Str_Output <- renderPrint({ str(mayonnaise) })
    
    output$Vis_Miss <- renderPlot({ visdat::vis_miss(mayonnaise) })
    ##########################
    output$Plot_NIR <- renderPlot({
        data <-  nir
        if (input$MSC) { data = nir_msc }
        matplot(data, type="l", xlab = "Observations", ylab = "NIR-spectra") 
    })
    
    rv_gg <- reactiveValues(data1 = mayo[,c(1:5)])
    observeEvent(input$Rand_Group, {rv_gg$data1 <- mayo[,sample(1:351, 5)]})
    output$Pairs_NIR <- renderPlot({
        GGally::ggpairs(data = rv_gg$data1, title = "Pairs of 10 Samples Group")
    })
    
    ##########################
    output$Bar_Oil <- renderPlot({
        data <- table(mayonnaise %>% dplyr::select(train, oil.type))
        barplot(data, legend=c("Test","Train"), xlab = "Oil types", ylab = "Number of observations")
    })
    ##########################
    rv_plsr <- reactiveValues(plsr_mod = pls::plsr(oil.type ~ .,ncomp = 50,validation = "CV",data = mayo_train))
    observeEvent(input$go1, {
        rv_plsr$plsr_mod <- pls::plsr(
            oil.type ~ .,
            ncomp = input$ncomp,
            validation = input$cv,
            scale=input$sc,
            center=input$ce,
            data = mayo_train)
    })
    output$Plot_RMSEP <- renderPlot({ plot(RMSEP(rv_plsr$plsr_mod), legendpos = "topright") })
    output$Plot_SNcomp <- renderPlot({ selectNcomp(rv_plsr$plsr_mod, method = input$he, plot = TRUE) })
    output$Plsr_2d <- renderPlot({ 
        my_plot_data_plsr <- cbind(as.data.frame(rv_plsr$plsr_mod$scores[,1:2]), "oil.type"=mayo_train$oil.type)
        plot(x=my_plot_data_plsr$`Comp 1`, type= "n", y = my_plot_data_plsr$`Comp 2`,xlab = "PC1", ylab = "PC2")
        text(my_plot_data_plsr, labels = my_plot_data_plsr[,3], col=my_plot_data_plsr[,3])
    })
    output$Plsr_pairs <- renderPlot({ plot(rv_plsr$plsr_mod, plottype = input$pt, comps = 1:5) })
    ##########################
    rv_pcr <- reactiveValues(pcr_mod = pls::pcr(oil.type ~ .,ncomp = 100,validation = "CV",data = mayo_train))
    observeEvent(input$go2, {
        rv_pcr$pcr_mod <- pls::pcr(
            oil.type ~ .,
            ncomp = input$ncomp2,
            validation = input$cv2,
            scale=input$sc2,
            center=input$ce2,
            data = mayo_train)
    })
    output$Plot_RMSEP_pcr <- renderPlot({ plot(RMSEP(rv_pcr$pcr_mod), legendpos = "topright") })    
    output$Plot_SNcomp_pcr <- renderPlot({ selectNcomp(rv_pcr$pcr_mod, method = input$he2, plot = TRUE) })
    ##########################
    # Partof code in fitting shrinkage model are based on ISLR 6.5.3 Lab
    shrink_mod0 <-  glmnet(x_train,y_train,alpha = 0, lambda = grid, exact = TRUE, standardize = TRUE)
    cv_shrink_mod0 <- cv.glmnet(x_train,y_train,alpha = 0, lambda = grid, nfolds = 10,exact = TRUE, standardize = TRUE)
    output$Shrink_plot0 <- renderPlot({
        # use CV to choose lambda
        set.seed(1)
        par(mfrow=c(1,2))
        plot(shrink_mod0, main = "Ridge coefficients vary with lambda")
        plot(cv_shrink_mod0, main = "Choosing ridge model using CV")

    })
    shrink_mod1 <- glmnet(x_train,y_train,alpha = 1, lambda = grid, exact = TRUE, standardize = TRUE)
    cv_shrink_mod1 <-  cv.glmnet(x_train,y_train,alpha = 1, lambda = grid, nfolds = 10,exact = TRUE, standardize = TRUE)
    
    output$Shrink_plot1 <- renderPlot({
        # use CV to choose lambda
        set.seed(1)
        par(mfrow=c(1,2))
        plot(shrink_mod1, main = "Lasso coefficients vary with lambda")
        plot(cv_shrink_mod1, main = "Choosing lasso model using CV")
    })
    
    best_lam0 <- cv_shrink_mod0$lambda.min
    best_lam1 <- cv_shrink_mod1$lambda.min
    output$Shrink_output <- renderPrint({
        print(paste("Optimal lambda of ridge regression is: ",best_lam0))
        print(paste("Optimal lambda of lasso is: ",best_lam1))
        print("----------------------------------------------------------")
        # predict in test
        shrink_pred0 <- predict(shrink_mod0, s=best_lam0, newx = x_test)
        print(paste("Test MSE of ridge regression is: ",mean((shrink_pred0-y_test)^2)))
        shrink_pred1 <- predict(shrink_mod1, s=best_lam1, newx = x_test)
        print(paste("Test MSE of lasso model is: ",mean((shrink_pred1-y_test)^2)))
        print("----------------------------------------------------------")
        # choosen variables
        shrink_coef0 <- predict(shrink_mod0,type="coefficients", s=best_lam0)[1:352,]
        print(paste("Number of variables choosen by ridge regression: ",sum(shrink_coef0!=0)))
        shrink_coef1 <- predict(shrink_mod1,type="coefficients", s=best_lam1)[1:352,]
        print(paste("Number of variables choosen by lasso: ",sum(shrink_coef1!=0)))
        print("----------------------------------------------------------")
        print("The variables with coefficients choosen by lasso: ")
        print(shrink_coef1[shrink_coef1!=0])
    })
    ##########################
    output$MDS_plot1 <- renderPlot({ 
        mayo_train_dis <- stats::dist(mayo_train[,-352], method = input$dm)
        mayo_train_cmds <- stats::cmdscale(mayo_train_dis, k = input$kn)
        plot_data_cmds <- cbind(as.data.frame(mayo_train_cmds), "oil.type"=mayo_train$oil.type)
        par(mfrow=c(1,2))
        # layout(mat = matrix(c(3,2)))
        plot(x=plot_data_cmds$V1,y=plot_data_cmds$V2, type = "n", xlab = "Principal component 1", ylab = "Principal component 2")
        text(plot_data_cmds[1:2], labels = plot_data_cmds$oil.type, col = plot_data_cmds$oil.type)
        title("Classical MDS in two PCs")

        mayo_train_mds <- MASS::isoMDS(mayo_train_dis, k = input$kn)
        plot_data_mds <- cbind(as.data.frame(mayo_train_mds$points), "oil.type"=mayo_train$oil.type)
        plot(x=plot_data_mds$V1,y=plot_data_mds$V2, type = "n", xlab = "Principal component 1", ylab = "Principal component 2")
        text(plot_data_mds[1:2], labels = plot_data_mds$oil.type, col = plot_data_mds$oil.type)
        title("Non-numerical MDS in two PCs")
    })
    
    output$MDS_plot2 <- renderPlot({
        mayo_train_dis <- stats::dist(mayo_train[,-352], method = input$dm)
        par(mfrow=c(1,2))
        mayo_train_sam <- MASS::sammon(mayo_train_dis, k = input$kn)
        plot_data_sam <- cbind(as.data.frame(mayo_train_sam$points), "oil.type"=mayo_train$oil.type)
        plot(x=plot_data_sam$V1,y=plot_data_sam$V2, type = "n", xlab = "Principal component 1", ylab = "Principal component 2")
        text(plot_data_sam[1:2], labels = plot_data_sam$oil.type, col = plot_data_sam$oil.type)
        title("Sammon Mapping in two PCs")
        
        
        # sammon followed by clustering
        kmeans_clust <- kmeans(mayo_train_sam$points, 6)  # k-means wihth 3 clusters.
        my_plot_data <- cbind(as.data.frame(mayo_train_sam$points), "oil.type"=mayo_train$oil.type,"cluster"=kmeans_clust$cluster)
        plot(x=my_plot_data$V1,y=my_plot_data$V2, type = "n", xlab = "Principal component 1", ylab = "Principal component 2")
        text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$cluster)  # set color using k-means output
        title("Sammon Mapping followed by K-means cluster (k=6)")
    })
    ##########################
    output$PCA_plot1 <- renderPlot({
    # Feature extract by pls
        rec <- recipes::recipe(oil.type ~ ., data = mayo_train) %>%
            step_pls(all_predictors(), outcome = mayo_train$oil.type, num_comp = 60)
        modpls <- caret::train(rec, data = mayo_train, method = "glmnet")
        plot(varImp(modpls))
    })
    output$PCA_plot2 <- renderPlot({
        # Feature extract by pca
        rec <- recipes::recipe(oil.type ~ ., data = mayo_train) %>%
            step_pca(all_predictors(), num_comp = 60)
        modpca <- caret::train(rec, data = mayo_train, method = "glmnet")
        plot(varImp(modpca))
    })
})
