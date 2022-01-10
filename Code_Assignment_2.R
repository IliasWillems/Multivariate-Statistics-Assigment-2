################################################################################
#                                   TASK 1                                     # 
################################################################################

data <- dtrust

library(candisc)

vars <- scale(data[,2:11],scale=TRUE,center=TRUE)

model <- lm(as.matrix(vars)~as.factor(country), data=data)

candisc <- candisc(model)

print(candisc)
## Canonical correlations 0.38 and 0.15 for first and second 
## discriminant functions. 

## Likelihood ratio test 1 is significant, which shows that the first and
## following canonical correlations are not equal to zero. The second likelihood
## ratio test shows that the second and all following canonical correlations
## are not equal to zero, but since the second is the last, this means 
## the second is significant, so both the first and second canonical 
## correlations are significant. The fact that at least one of the 
## canonical correlations is significant means that at least one of the 
## means of the countries is different from the others. Both discriminant
## functions kept. 

plot(candisc,col=c("blue","red","green"),pch=c(1,1,1),cex=(0.8),
     main="Respondent scores on discriminant functions")
legend("topright",                  
       legend = c("Netherlands","Poland","United Kingdom"),
       col = c("blue","red","green"),
       pch = c(1,1,1))

candisc$structure
## (canonical loadings -- correlations between discriminant functions and variables)



################################################################################
#                                   TASK 2                                     # 
################################################################################


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



################################################################################
#                                   TASK 3                                     # 
################################################################################

# Clear workspace
rm(list = ls())

# Load in the data
load("~/,School/Master/Multivariate Statistics/Exam Assignments/Assignment2/features.Rdata")
load("~/,School/Master/Multivariate Statistics/Exam Assignments/Assignment2/dissim.Rdata")

# Load in the necessary packages
library(smacof)
library(stargazer)

# Question a
# 
# Use smacofSym() to conduct multidimensional scaling assuming different
# measurement levels. Include 2 dimensions.
set.seed(123)

mratio <- smacofSym(delta = dissim, ndim=2, type="ratio", init="torgerson",
                    ties = "primary")
minterval <- smacofSym(delta = dissim, ndim=2, type="interval",
                       init="torgerson", ties = "primary")

# Using spline.degree = 3 and spline,intKnots = 3 gives the best results.
mmspline <- smacofSym(delta = dissim, ndim=2, type="mspline", init="torgerson",
                      spline.degree = 3, spline.intKnots = 3, ties = "primary")
mordinal <- smacofSym(delta = dissim, ndim=2, type="ordinal", init="torgerson",
                      ties = "primary")

# Get the stress-1 values of the models
stresses <- round(c(mratio$stress, minterval$stress, mmspline$stress,
                    mordinal$stress), 3)
stresses

par(mfrow=c(2,2))
plot(mordinal,plot.type="resplot",main="residual plot ordinal MDS")
plot(mordinal,plot.type="Shepard",main="Shepard diagram ordinal MDS")
plot(mmspline,plot.type="resplot",main="residual plot spline MDS")
plot(mmspline,plot.type="Shepard",main="Shepard diagram spline MDS")
# There are some quite large residuals. This is also evident from the stress-1
# values, which are quite large.

par(mfrow=c(1,1))
plot(mordinal, plot.type="conf")
# In the configuration plot we can clearly see that the first dimension 
# separates male relatives from female relatives, and the second dimension
# separates 'direct' family from 'indirect' family.

# Question b
# 
# Evaluate goodness-of-fit using stress-1, randomstress and a permutaion test.
set.seed(123)

# Stress norms: randomstress
rstress.ratio <- randomstress(n=15, ndim=2, type="ratio")
rstress.interval <- randomstress(n=15, ndim=2, type="interval")
rstress.ordinal <- randomstress(n=15, ndim=2, type="ordinal")
rstress.mspline <- randomstress(n=15, ndim=2, type="mspline")

co.ratio <- mean(rstress.ratio) - 2*sd(rstress.ratio)
co.interval <- mean(rstress.interval) - 2*sd(rstress.interval)
co.ordinal <- mean(rstress.ordinal) - 2*sd(rstress.ordinal)
co.mspline <- mean(rstress.mspline) - 2*sd(rstress.mspline)

rand.co <- round(c(co.ratio, co.interval, co.mspline, co.ordinal), 3)
# All stresses are in the lower tail of the random stress distributions

# Stress norms: permutation
perm.ratio <- permtest(mratio, nrep=500)
perm.interval <- permtest(minterval, nrep=500)
perm.ordinal <- permtest(mordinal, nrep=500)
perm.mspline <- permtest(mmspline, nrep=500)

co.perm.ratio <- mean(perm.ratio$stressvec) - 2*sd(perm.ratio$stressvec)
co.perm.interval <- mean(perm.interval$stressvec) - 2*sd(perm.interval$stressvec)
co.perm.ordinal <- mean(perm.ordinal$stressvec) - 2*sd(perm.ordinal$stressvec)
co.perm.mspline <- mean(perm.mspline$stressvec) - 2*sd(perm.mspline$stressvec)

perm.co <- round(c(co.perm.ratio, co.perm.interval, co.perm.mspline, co.perm.ordinal), 3)

df <- data.frame(rbind(stresses, rand.co, perm.co))
colnames(df) <- c("ratio", "interval", "spline", "ordinal")
df

stargazer(df, summary = FALSE, flip = TRUE)

# From these tests we see that:
# All stress-1 values are in the lower tail of the random stress distributions,
# but only marginally. The ordinal step function results in the lowest stress.

# In conclusion, I would select the ordinal transformation function to compute
# the disparities, as it gives the lowest stress-1 and has an interpretable 
# result (see above). We check stability

set.seed(123)

jack.kinship <- jackmds(mordinal)

jack.kinship$stab
# Very low stability measure.

plot(jack.kinship)
# The solution is very unstable. However, looking more closely at the plot, it 
# can be seen that the lines indicating the different scores for a kinship on
# the dimensions mainly go from the upper-left area to the bottom right area.
# These areas represent males and females respectively. The instability of the
# solution is possibly due to the interpretation of the dimensions reversing.
# One iteration, a positive score on the first dimension might indicate a male,
# whilst in a next iteration it indicates a female (and the same thing goes for
# the second dimension).

for (i in 1:dim())
  
  # Question c
  # 
  # Use an MDSbiplot to project the variables "generation" and "degree" in the
  # configuration plot, and interpret the analysis.
  
  biKinship <- biplotmds(mordinal, features)
plot(biKinship, vecscale = 0.8, main="External Variables Biplot",
     xlim = c(-1.5, 1.5), vec.conf = list(col = "red"), pch = 20, cex = 0.5)
# The external variables confirm our interpretations of the dimensions
# partially. The degree has high correlation with the second principal component
# as was mentioned previously. The generation vector is not so easy to inter-
# pret. We look at the correlations:

cor(mordinal$conf, features)

# From these correlations we see that the correlation of degree with the first
# dimension is only small. The correlations are even smaller with the second
# dimension. It could be that including a third dimension, the third dimension
# will correlate with this feature.

coef(biKinship)

biKinship$R2vec




