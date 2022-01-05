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