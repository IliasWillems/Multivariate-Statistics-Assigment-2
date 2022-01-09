
library(MASS)
library(dplyr)
library(paran)
library(HDclassif)  
library(stargazer)
library(randomForest)

load("Caravan.Rdata")

head(Caravan)

# Define a function that will compute the desired performance measures
metrics <- function(observed,predicted)
{tab<-table(observed,predicted)
hit_rate<-sum(diag(tab))/sum(tab)
sensitivity <- tab[4]/sum(tab[4], tab[2])
specificity <- tab[1]/sum(tab[1], tab[3])
return(c(hit_rate, sensitivity, specificity))
}

# Prior probability
round(table(Caravan[,84])/dim(Caravan)[1],3)

# Center x matrix
Caravan[,1:83] <- scale(Caravan[,1:83], center = T, scale = F)


# (1) LDA

lda_model_train <- lda(Caravan[,1:83], Caravan[,84], prior=c(.940,.060)) 
pred_train_lda <- predict(lda_model_train,Caravan[,1:83])
pred_train_lda_ecm <- ifelse(1*pred_train_lda$posterior[,1]>=20*pred_train_lda$posterior[,2],"No","Yes")


# Metrics - Training data
metrics_train_lda <- metrics(Caravan[,84], pred_train_lda$class)
metrics_train_lda_ecm <- metrics(Caravan[,84], pred_train_lda_ecm)

 
# Metrics- Test data
lda_model_test <- lda(Caravan[,1:83], Caravan[,84], prior=c(.940,.060), CV=T)
metrics_test_lda <- metrics(Caravan[,84], lda_model_test$class)
 
pred_test_lda_ecm <- ifelse(1*lda_model_test$posterior[,1]>=20*lda_model_test$posterior[,2],"No","Yes")
metrics_test_lda_ecm <- metrics(Caravan[,84], pred_test_lda_ecm)
 
metrics_table <- rbind(metrics_train_lda,metrics_train_lda_ecm, metrics_test_lda, metrics_test_lda_ecm)
colnames(metrics_table) <- c("Hit Rate", "Sensitivity", "Specificity")
round(metrics_table, 4)
 



# (2) LDA+PCA


p_comp <- prcomp(Caravan[,1:83])
 
 
set.seed(10)
horn <- paran(Caravan[,1:83],iterations=5000,graph=TRUE,cfa=FALSE,centile=0)
 
#31 components capture 72% of the variance
n_comp_var <- 31

#compute unstandardized principal components
train_comp<-as.matrix(Caravan[,1:83])%*%p_comp$rotation[,1:n_comp_var]

#compute lda on components
lda_pca <- lda(train_comp, Caravan[,84], prior=c(.940,.060))

#hit rate training set
pred_train_lda_pca <- predict(lda_pca, train_comp)
pred_train_lda_pca_ecm <- ifelse(1*pred_train_lda_pca$posterior[,1]>=20*pred_train_lda_pca$posterior[,2],"No","Yes")

# Metrics - Training data
metrics_train_lda_pca <- metrics(Caravan[,84], pred_train_lda_pca$class)
metrics_train_lda_pca_ecm <- metrics(Caravan[,84], pred_train_lda_pca_ecm)

 
#compute unstandardized components for test set, using rotation vectors of training data
test_comp <- as.matrix(Caravan[,1:83])%*%p_comp$rotation[,1:n_comp_var]

# Metrics- Test data
lda_pca_model_test <- lda(test_comp, Caravan[,84], prior=c(.940,.060), CV=T)
metrics_test_lda_pca <- metrics(Caravan[,84], lda_pca_model_test$class)

pred_test_lda_pca_ecm <- ifelse(1*lda_pca_model_test$posterior[,1]>=20*lda_pca_model_test$posterior[,2],"No","Yes")
metrics_test_lda_pca_ecm <- metrics(Caravan[,84], pred_test_lda_pca_ecm)



 
# (3) QDA 

qda_model_train <- qda(Caravan[,1:83], Caravan[,84], prior=c(.940,.060)) 
metrics_qda <- c(0,0,0)
# Rank deficiency: Covariance matrix is singular for class 1


# (4) QDA+PCA


n_data <-dim(Caravan)[1]
 
train_comp_1 <-matrix(rep(0,n_data*n_comp_var), nrow=n_data)
 
Caravan[,85] <- ifelse(Caravan$Purchase == "Yes", 1, 0)

#caravan_x <- Caravan[,1:83]
 
#a <- prcomp(Caravan[,1:83][Caravan[,85]==1,])
#b <- prcomp(caravan_x[Caravan[,85]==1,])
#summary(a)
#summary(b)

for (i in 0:1){
        p_component_qda <- prcomp(Caravan[,1:83][Caravan[,85]==i,])
        train_comp_1[Caravan[,85]==i,]<-as.matrix(Caravan[,1:83][Caravan[,85]==i,])%*%p_component_qda$rotation[,1:n_comp_var]
}
 
 


#hit rate training data
qda_pca <- qda(train_comp_1,Caravan[,84], prior=c(.940,.060))
pred_train_qda_pca <-predict(qda_pca,train_comp_1)
pred_train_qda_pca_ecm <- ifelse(1*pred_train_qda_pca$posterior[,1]>=20*pred_train_qda_pca$posterior[,2],"No","Yes")
 
 
# Metrics - Training data
metrics_train_qda_pca <- metrics(Caravan[,84], pred_train_qda_pca$class)
metrics_train_qda_pca_ecm <- metrics(Caravan[,84], pred_train_qda_pca_ecm)
 


# Metrics- Test data
qda_pca_model_test <-qda(train_comp_1,Caravan[,84], prior=c(.940,.060), CV=T)
metrics_test_qda_pca <-metrics(Caravan[,84], qda_pca_model_test$class)
 
pred_test_qda_pca_ecm <- ifelse(1*qda_pca_model_test$posterior[,1]>=20*qda_pca_model_test$posterior[,2],"No","Yes")
metrics_test_qda_pca_ecm <- metrics(Caravan[,84], pred_test_qda_pca_ecm)
 



 


 

# HDDA
hdda_train <- hdda(Caravan[,1:83], Caravan[,84], model="all", scaling=TRUE, d_select = "BIC")
hdda_train




#Prediction training data
pred_train_hdda <- predict(hdda_train, Caravan[,1:83], Caravan[,84])
pred_train_hdda_ecm <- ifelse(1*pred_train_hdda$posterior[,1]>=20*pred_train_hdda$posterior[,2],"No","Yes")


# Metrics - Training data
metrics_train_hdda <- metrics(Caravan[,84], pred_train_hdda$class)
metrics_train_hdda_ecm <- metrics(Caravan[,84], pred_train_hdda_ecm)

 

# Metrics - Testing data
hdda_test <- hdda(Caravan[,1:83], Caravan[,84], model="all", scaling=TRUE, 
                  d_select = "BIC",LOO=TRUE)

metrics_test_hdda <-metrics(Caravan[,84], hdda_test$class)

pred_test_hdda_ecm <- ifelse(1*hdda_test$posterior[,1]>=20*hdda_test$posterior[,2],"No","Yes")
metrics_test_hdda_ecm <- metrics(Caravan[,84], pred_test_hdda_ecm)
 
 
metrics_table <-
        rbind(
                "LDA train set" = metrics_train_lda,
                "LDA test set" = metrics_test_lda,
                "LDA train set. ECM" = metrics_train_lda_ecm,
                "LDA test set. ECM" = metrics_test_lda_ecm,
                "LDA + PCA train set (31 componentes)" = metrics_train_lda_pca,
                "LDA + PCA test set (31 componentes)" = metrics_test_lda_pca,
                "LDA + PCA train set (31 componentes). ECM" = metrics_train_lda_pca_ecm,
                "LDA + PCA test set (31 componentes). ECM" = metrics_test_lda_pca_ecm,
                "QDA" = metrics_qda,
                "QDA + PCA train set (31 componentes)" = metrics_train_qda_pca,
                "QDA + PCA test set (31 componentes)" = metrics_test_qda_pca,
                "QDA + PCA train set (31 componentes). ECM" = metrics_train_qda_pca_ecm,
                "QDA + PCA test set (31 componentes). ECM" = metrics_test_qda_pca_ecm,
                "HDDA train set (model [AjBQd], with d = 49)" = metrics_train_hdda,
                "HDDA test set (model [AjBQd], with d = 49)" = metrics_test_hdda,
                "HDDA train set (model [AjBQd], with d = 49). ECM" = metrics_train_hdda_ecm,
                "HDDA test set (model [AjBQd], with d = 49). ECM" = metrics_test_hdda_ecm
        )
colnames(metrics_table) <- c("Hit Rate", "Sensitivity", "Specificity")
round(metrics_table, 4)
 
stargazer(metrics_table,  # type = "text", 
          summary = FALSE  )

################################################################################
#                               Tree-based models                              #
################################################################################

# Code Jonas

load("Caravan.Rdata")

set.seed(999)


# The dataset is randomly split into two equal parts. One for training, one for testing
# A special dataframe is made for the dependent variable, which is 'Purchase'
train = sample(nrow(Caravan), (nrow(Caravan) / 2))
data.train <- Caravan[train, 1:83]
data.test <- Caravan[-train, 1:83]
dependent.train <- Caravan[train, 84]
dependent.test <- Caravan[-train, 84] 

cdata.train <- scale(data.train, center = TRUE, scale = FALSE)
cdata.test <- scale(data.test, center = TRUE, scale = FALSE)
ndata.train <- data.frame(data.train, Purchase = dependent.train)
ndata.test <- data.frame(data.test, Purchase = dependent.test) 


# The performance of each classifier is measured with the sensitivity, specificity, and hit rate
# The error is the OOB error for training data and test error for test data
performance <- function(tab){
        sensitivity <- tab[2, 2] / (tab[2, 1] + tab[2, 2])
        specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
        hit_rate    <- (tab[1, 1] + tab[2, 2]) / (tab[1, 1] + tab[1, 2] + tab[2, 1] + tab[2, 2])
        Error <- (tab[1, 2] + tab[2, 1]) / (tab[1, 1] + tab[1, 2] + tab[2, 1 ] + tab[2, 2])
        performance <- c(sensitivity = sensitivity, specificity = specificity, hit_rate = hit_rate, Error = Error)
} 


# Bagging
# mtry = 83, as there are 83 independent variables in the dataset
# So he can use all the independent variables
# 5000 trees are fitted
ctrain <- cbind(cdata.train, dependent.train)
bag.mod = randomForest(as.factor(dependent.train)~., data = ctrain, mtry = 83, ntree = 5000, importance = TRUE)
bag.mod 


# Predictions training data
pred.train <- predict(bag.mod, newdata = ctrain, type = "prob")

# Equal distribution
class.train <- ifelse(pred.train > 0.5, 1, 0)
tab <- table(dependent.train, class.train[, 2])
bag.train.equal <- performance(tab) 

# Unequal distribution 
class.train <- ifelse(1 * pred.train[, 1] >= 20 * pred.train[, 2], "No", "Yes")
tab <- table(dependent.train, class.train)
bag.train.unequal <- performance(tab) 


# Predictions test data
pred.test <- predict(bag.mod, newdata = cdata.test, type="prob")

# Equal distribution
class.test <- ifelse(pred.test > 0.5, 1 ,0)
tab <- table(dependent.test, class.test[, 2])
bag.test.equal <- performance(tab)

# Unequal distribution
class.test <- ifelse( 1* pred.test[, 1] >= 20 * pred.test[, 2], "No", "Yes")
tab <- table(dependent.test, class.test)
bag.test.unequal <- performance(tab) 


# Random forest
# 8 predictors are randomly selected out of 83
# 5000 trees are fitted
ctrain <- cbind(cdata.train, dependent.train)
rf.mod = randomForest(as.factor(dependent.train)~., data=ctrain, mtry = 8, ntree = 5000, importance = TRUE)
rf.mod


# Predictions training data
pred.train <- predict(rf.mod, newdata = ctrain, type = "prob")

# Equal distribution
class.train <- ifelse(pred.train > 0.5, 1, 0)
tab <- table(dependent.train, class.train[, 2])
rf.train.equal <- performance(tab)

# Unequal distribution
class.train <- ifelse(1 * pred.train[, 1] >= 20 * pred.train[, 2], "No", "Yes")
tab <- table(dependent.train, class.train)
rf.train.unequal <- performance(tab)


# Predictions test data
pred.test <- predict(rf.mod, newdata = cdata.test, type = "prob")

# Equal distribution
class.test <- ifelse(pred.test > 0.5, 1, 0)
tab <- table(dependent.test, class.test[, 2])
rf.test.equal <- performance(tab)

# Unequal distribution
class.test <- ifelse( 1 * pred.test[, 1] >= 20 * pred.test[, 2], "No", "Yes")
tab <- table(dependent.test, class.test)
rf.test.unequal <- performance(tab) 


# Making tables for the two different scenarios
tabel_equal_distribution <- round(rbind(bag.train.equal, bag.test.equal, rf.train.equal, rf.test.equal), 3) 
tabel_unequal_distribution <- round(rbind(bag.train.unequal, bag.test.unequal, rf.train.unequal, rf.test.unequal), 3) 


# Code Ilias

#
# Bagging
# 

performance <- function(tab){
        sensitivity <- tab[2, 2] / (tab[2, 1] + tab[2, 2])
        specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
        hit_rate    <- (tab[1, 1] + tab[2, 2]) / (tab[1, 1] + tab[1, 2] + tab[2, 1] + tab[2, 2])
        Error <- (tab[1, 2] + tab[2, 1]) / (tab[1, 1] + tab[1, 2] + tab[2, 1 ] + tab[2, 2])
        performance <- c(sensitivity = sensitivity, specificity = specificity, hit_rate = hit_rate, Error = Error)
} 

set.seed(42069)

# compute model
bag.mod <- randomForest(Purchase~., data=Caravan, mtry=83, ntree=5000,
                        importance = TRUE)

# Compute training and test error with equal misclass costs
bag.pred.probs <- predict(bag.mod, newdata=Caravan, type="prob")
bag.pred.eq <- ifelse(bag.pred.probs[,1] >= 0.5, "No", "Yes")
bag.pred.uneq <- ifelse(bag.pred.probs[,1] >= bag.pred.probs[,2]*20, "No", "Yes")

bag.pred.eq.measures <- performance(table(Caravan$Purchase, bag.pred.eq))
bag.pred.uneq.measures <- performance(table(Caravan$Purchase, bag.pred.uneq))

# estimate OOB performances
bag.oob.eq <- ifelse(bag.mod$votes[,1] >= 0.5, "No", "Yes")
bag.oob.eq <- performance(table(Caravan$Purchase, bag.oob.eq))
bag.oob.uneq <- ifelse(bag.mod$votes[,1] >= bag.mod$votes[,2]*20, "No", "Yes")
bag.oob.uneq <- performance(table(Caravan$Purchase, bag.oob.uneq))

bag.perf <- rbind(bag.pred.eq.measures, bag.pred.uneq.measures, bag.oob.eq, bag.oob.uneq)

#
# Random Forests
#
rf.mod <- randomForest(as.factor(Purchase)~.,data=Caravan,mrty=5,ntree=5000,
                       importance=TRUE)
rf.mod

rf.pred.probs <- predict(rf.mod, newdata=Caravan, type="prob")
rf.pred.eq <- ifelse(rf.pred.probs[,1] >= 0.5, "No", "Yes")
rf.pred.uneq <- ifelse(rf.pred.probs[,1] >= rf.pred.probs[,2]*20, "No", "Yes")

rf.pred.eq.measures <- performance(table(Caravan$Purchase, rf.pred.eq))
rf.pred.uneq.measures <- performance(table(Caravan$Purchase, rf.pred.uneq))

# estimate OOB performances
rf.oob.eq <- ifelse(rf.mod$votes[,1] >= 0.5, "No", "Yes")
rf.oob.eq <- performance(table(Caravan$Purchase, rf.oob.eq))
rf.oob.uneq <- ifelse(rf.mod$votes[,1] >= rf.mod$votes[,2]*20, "No", "Yes")
rf.oob.uneq <- performance(table(Caravan$Purchase, rf.oob.uneq))

rf.perf <- rbind(rf.pred.eq.measures, rf.pred.uneq.measures, rf.oob.eq, rf.oob.uneq) 

tree.perf <- rbind(bag.perf, rf.perf)
rownames(tree.perf) <- c("Bagging train", "Bagging train. ECM",
                         "Bagging test", "Bagging test. ECM",
                         "Rand. For. train", "Rand. For. train. ECM",
                         "Rand. For. test", "Rand. For. test. ECM")

stargazer(tree.perf,  # type = "text", 
          summary = FALSE  )

# Expected profits
profit <- function(tab){
       profit <- tab[1,2]*20 - tab[2,2]*1
} 

lda.profit <- profit(table(Caravan$Purchase, pred_test_lda_ecm))
lda.pca.profit <- profit(table(Caravan$Purchase, pred_test_lda_pca_ecm))
qda.pca.profit <- profit(table(Caravan$Purchase, pred_test_qda_pca_ecm))
hdda.profit <- profit(table(Caravan$Purchase, pred_test_hdda_ecm))
bag.profit <- profit(table(Caravan$Purchase, 
                           ifelse(bag.mod$votes[,1] >= bag.mod$votes[,2]*20, "No", "Yes")))
rf.profit <- profit(table(Caravan$Purchase, 
                          ifelse(rf.mod$votes[,1] >= rf.mod$votes[,2]*20, "No", "Yes")))
profits <- data.frame(lda.profit, lda.pca.profit, qda.pca.profit, hdda.profit,
                      bag.profit, rf.profit)
profits

stargazer(profits,  # type = "text", 
          summary = FALSE  )
