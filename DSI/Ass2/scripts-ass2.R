# load base packages
if(!require(shiny)) install.packages("shiny")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")


# load statistic packages
if(!require(MASS)) install.packages("MASS")
if(!require(pls)) install.packages("pls")
#if(!require(corrgram)) install.packages("corrgram")
if(!require(caret)) install.packages("caret")
if(!require(recipes)) install.packages("recipes")
#if(!require(factoextra)) devtools::install_github("kassambara/factoextra")
if(!require(leaps)) install.packages("leaps")
if(!require(glmnet)) install.packages("glmnet")


# load data set
data(mayonnaise)
str(mayonnaise)
my_mayo_train <- mayonnaise[mayonnaise$train,-4] # training data set
my_mayo_test <- mayonnaise[!mayonnaise$train,-4] # test data set
my_mayo_train2 <- data.frame(oil.type = my_mayo_train$oil.type, msc(my_mayo_train$NIR))
my_mayo_test2 <- data.frame(oil.type = my_mayo_test$oil.type, msc(my_mayo_test$NIR))

str(my_mayo_train)
data(my_mayo_train)
my_mayo_train %>% group_by(oil.type) %>% count()
str(my_mayo_test)
my_mayo_test %>% group_by(oil.type) %>% count()

matplot(mayonnaise$NIR, type="l")
# Multiplicative Scatter Correction
matplot(msc(mayonnaise$NIR), type="l")

pairs(mayonnaise$NIR[,201:210])

################################
# PLSR
my_mayo_plsr <- pls::cppls(
  oil.type ~ msc(NIR),
  ncomp = 50,
  data = my_mayo_train,
  validation = "CV")
# The validation results here are Root Mean Squared Error of Prediction (RMSEP). 
# There are two cross-validation estimates: CV is the ordinary CV estimate, 
# and adjCV is a bias-corrected CV estimate [17]. 
# (For a LOO CV, there is virtually no difference)

summary(my_mayo_plsr)

plot(RMSEP(my_mayo_plsr), legendpos = "topright")
# same output with below function in lecture example
# pls::validationplot(my_mayo_plsr, legendpos = "topright")
ncomp.onesigma <- selectNcomp(my_mayo_plsr, method = "onesigma", plot = TRUE)

plot(my_mayo_plsr, plottype = "scores", comps = 1:5)
# plot(my_mayo_plsr, ncomp = 17, asp = 1, line = TRUE)

my_plot_data_plsr <- cbind(as.data.frame(my_mayo_plsr$scores[,1:2]), "oil.type"=my_mayo_train$oil.type)
plot(x=my_plot_data_plsr$`Comp 1`, type= "n", y = my_plot_data_plsr$`Comp 2`,xlab = "PC1", ylab = "PC2")
text(my_plot_data_plsr, labels = my_plot_data_plsr[,3], col=my_plot_data_plsr[,3])

# predict by plsr
explvar(my_mayo_plsr)
predict(my_mayo_plsr, ncomp = 1:16, newdata = my_mayo_test)

###############
# PCA
# prcomp is from baseR, alternative function is princomp
# prcomp() function works with data.frame here, so I transfered NIR from matrix to a data.frame
my_mayo_pca <- prcomp(~ . -oil.type, data = my_mayo_train2, center = TRUE, scale. = TRUE, rank. = 30)
names(my_mayo_pca)
summary(my_mayo_pca,loadings=TRUE)
screeplot(my_mayo_pca,type = "lines", main = "PCA Scree plot")

pcaCharts <- function(x) {
  x.var <- x$sdev[1:10] ^ 2
  x.pvar <- x.var/sum(x.var[1:10])
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(my_mayo_pca)


biplot(my_mayo_pca,scale=0, cex=.7)



my_mayo_pca_pred <- predict(my_mayo_pca, newdata = my_mayo_train)
dim(my_mayo_pca_pred)
my_mayo_pca_pred


##############
# PCR
my_mayo_pcr <- pls::pcr(
  oil.type ~ .,
  ncomp = 100,
  data = my_mayo_train2,
  validation = "CV")

plot(RMSEP(my_mayo_pcr), legendpos = "topright")
ncomp.onesigma <- selectNcomp(my_mayo_pcr, method = "onesigma", plot = TRUE)

my_plot_data <- cbind(as.data.frame(my_mayo_pcr$scores[,1:2]), "oil.type"=my_mayo_train2$oil.type)
plot(x=my_plot_data$`Comp 1`, type= "n", y = my_plot_data$`Comp 2`)
text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$oil.type)

#################
# MDS
my_mayo_train_dis <- stats::dist(my_mayo_train2, method = "manhattan")
my_mayo_train_cmds <- stats::cmdscale(my_mayo_train_dis, k = 2)
my_plot_data <- cbind(as.data.frame(my_mayo_train_cmds), "oil.type"=my_mayo_train2$oil.type)
plot(x=my_plot_data$V1,y=my_plot_data$V2, type = "n", xlab = "PC1", ylab = "PC2")
text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$oil.type)

# non-metric
my_mayo_train_dis = dist(my_mayo_train2, 'manhattan')
my_mayo_train_mds <- MASS::isoMDS(my_mayo_train_dis, k = 2)
my_plot_data <- cbind(as.data.frame(my_mayo_train_mds$points), "oil.type"=my_mayo_train2$oil.type)
plot(x=my_plot_data$V1,y=my_plot_data$V2, type = "n", xlab = "PC1", ylab = "PC2")
text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$oil.type)

# sammon
my_mayo_train_dis = dist(my_mayo_train2, 'manhattan')
my_mayo_train_sam <- MASS::sammon(my_mayo_train_dis, k = 2)
my_plot_data <- cbind(as.data.frame(my_mayo_train_sam$points), "oil.type"=my_mayo_train2$oil.type)
plot(x=my_plot_data$V1,y=my_plot_data$V2, type = "n", xlab = "PC1", ylab = "PC2")
text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$oil.type)

# sammon followed by clustering
kmeans_clust <- kmeans(my_mayo_train_sam$points, 6)  # k-means wihth 3 clusters.
my_plot_data <- cbind(as.data.frame(my_mayo_train_sam$points), "oil.type"=my_mayo_train2$oil.type,"cluster"=kmeans_clust$cluster)
plot(x=my_plot_data$V1,y=my_plot_data$V2, type = "n", xlab = "PC1", ylab = "PC2")
text(my_plot_data[1:2], labels = my_plot_data$oil.type, col = my_plot_data$cluster)  # set color using k-means output

#####################
# Feature extract by pls
rec <- recipes::recipe(oil.type ~ ., data = my_mayo_train2) %>%
  step_pls(all_predictors(), outcome = my_mayo_train2$oil.type, num_comp = 60)
modpls <- caret::train(rec, data = my_mayo_train2, method = "glmnet")
plot(varImp(modpls))
caret::predictors(modpls)

# Feature extract by pca
rec <- recipes::recipe(oil.type ~ ., data = my_mayo_train2) %>%
  step_pca(all_predictors(), num_comp = 60)
modpca <- caret::train(rec, data = my_mayo_train2, method = "glmnet")
plot(varImp(modpca))
caret::predictors(modpca)

pca <- prep(rec)
summary(pca)
pca <- bake(pca, new_data = my_mayo_train2)
plot(x=pca$PC01,y=pca$PC02, type = 'n', xlab = "PC1", ylab = "PC2")
text(pca[c("PC01","PC02")], labels = pca$oil.type, col = pca$oil.type)  # set color using k-means output


my_lda <- lda(pca$oil.type~., data=pca)

####################
# regsubsets
regfit.full = regsubsets(oil.type~., my_mayo_train2, method="backward")
summary(regfit.full)

##################
# shrinkage
y=my_mayo_train2$oil.type
x=model.matrix(oil.type~., my_mayo_train2)[,-1]
y_test=my_mayo_test2$oil.type
x_test=model.matrix(oil.type~., my_mayo_test2)[,-1]

grid = 10^seq(10, -2, length=100)
lasso.mod = glmnet(x,y,alpha = 1, lambda = grid)
plot(lasso.mod)
cv.lasso.mod = cv.glmnet(x,y,alpha = 1, lambda = grid)
plot(cv.lasso.mod)
bestlam=cv.lasso.mod$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx = x_test)
mean((lasso.pred-y_test)^2)
out=glmnet(x,y,alpha=1,lamdba=grid)
lasso.coef=predict(out,type="coefficients", s=bestlam)[1:352,]
lasso.coef[lasso.coef!=0]


ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)
plot(ridge.mod)
cv.ridge.mod = cv.glmnet(x,y,alpha = 0, lambda = grid)
plot(cv.ridge.mod)
bestlam=cv.ridge.mod$lambda.min
ridge.pred=predict(ridge.mod, s=bestlam, newx = x_test)
mean((ridge.pred-y_test)^2)
out=glmnet(x,y,alpha=0,lamdba=grid)
ridge.coef=predict(out,type="coefficients", s=bestlam)[2:352,]
ridge.coef[ridge.coef!=0]

