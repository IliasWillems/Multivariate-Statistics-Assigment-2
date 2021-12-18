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
