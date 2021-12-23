
library(MASS)
library(dplyr)
library(paran)
library(HDclassif)  
library(stargazer)

load("Caravan.Rdata")

head(Caravan)
 
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

 