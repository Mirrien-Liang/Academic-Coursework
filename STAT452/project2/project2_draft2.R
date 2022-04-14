#########################################################################
# Random Forest on Prostate Data 
# 

project2 <-  read.table("project2/Data2021_final.csv", 
                        header=TRUE, sep=",", na.strings=" ")
project2
library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
## 
## Can tune "nodesize=", which is the smallest size of node that will 
##    still be split.  All terminal nodes will be smaller than this.
####################################################################

project.rf <- randomForest(data=project2, Y~., 
                       importance=TRUE, ntree=1000, mtry=1, 
                       keep.forest=TRUE)
project.rf             # Barely useful here
summary(project.rf)    # Not useful here.

# Default plot method shows OOB error vs. number of trees.

x11(h=7,w=6,pointsize=12)
plot(project.rf)

x11(h=7,w=6,pointsize=12)
plot(project.rf, xlim=c(50,1000), ylim=c(1.75,2))

# Histogram of tree sizes
x11(h=7,w=6,pointsize=12)
hist(treesize(project.rf))

# Note: If you specify no newdata= in predict.randomforest, 
#   it gives the OOB predictions for the sample, same as the
#   $predicted element of the randomforest object.
(project.oob <- mean((predict(project.rf) - project2$Y)^2))
(project.oob.p <- mean((project.rf$predicted - project2$Y)^2))
# Specifying the original data set uses the usual prediction, which results in training error.
(project.mse <- mean((predict(project.rf, newdata=project2) - project2$Y)^2))

# Variable Importance
importance(project.rf) # Print out importance measures
x11()
varImpPlot(project.rf) # Plot of importance measures; more interesting with more variables

library(rgl)  

test_project2 <- read.csv("project2/Data2021test_final_noY.csv")

pred <- predict(project.rf,newdata=test_project2)
pred


###############################################################
## Now let's look at entire data set, default values

project.rf8 <- randomForest(data=project2, Y~., 
                        importance=TRUE, ntree=1000, 
                        keep.forest=TRUE)

# Default plot method shows OOB error vs. number of trees.

x11(h=7,w=6,pointsize=12)
plot(project.rf8, main="RF OOB Error vs. Number of trees")

# Note: If you specify no newdata= in predict.randomforest, 
#   it gives the OOB predictions for the sample, same as the
#   $predicted element of the randomforest object.
(project.oob8 <- mean((predict(project.rf8) - project2$Y)^2))

# Variable Importance
importance(project.rf8) # Print out importance measures
x11()
varImpPlot(project.rf8, main="RF Variable Importance Plots") # Plot of importance measures; more interesting with more variables

###### tuning mtry using OOB error
# Usually, don't need to try every m, but can do it cheap here
# Also could put this whole ting inside another loop and replicate

reps=20 # Doing lots of reps here because it's cheap
varz = 1:8
nodez = c(3,5,7,10,15,20)

NS = length(nodez)
M = length(varz)
rf.oob = matrix(NA, nrow=M*NS, ncol=reps)

for(r in 1:reps){
  counter=1
  for(m in varz){
    for(ns in nodez){
      project.rfm <- randomForest(data=project2, Y~., ntree=500, 
                              mtry=m, nodesize=ns)
      rf.oob[counter,r] = mean((predict(project.rfm) - project2$Y)^2)
      counter=counter+1
    }
  }
}

parms = expand.grid(nodez,varz)
row.names(rf.oob) = paste(parms[,2], parms[,1], sep="|")

mean.oob = apply(rf.oob, 1, mean)
min.oob = apply(rf.oob, 2, min)

x11(h=7,w=10,pointsize=8)
boxplot(rf.oob, use.cols=FALSE, las=2)

x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

