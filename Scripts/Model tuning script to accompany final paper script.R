#Model tuning script

#SCORE MODEL

#Start with a best guess of what ranges might make sense based on a review of literature and internet resources. Aiming for less trees (to avoid overfitting) while still minimizing loss function. Learning rate is typically between 0.001 and 0.3, I assume it will be on the lower end here due to small sample size. Similarly with tree depth and minimum observations which are usually 2-8 and 5-15 respectively so I go down there. 
model<-SCORE_minusengagement_summed_minusproj ~.
metric<-"RMSE"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.001, 0.01, 0.1), n.minobsinnode=c(3:6))  #

set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.65)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #started at 2:20, done by 2:35, RMSE 13.67884. The final values used for the model were n.trees = 4900, interaction.depth = 2, shrinkage = 0.001 and n.minobsinnode = 6.

#Try a new, lower span of learning rates since using lots of trees
model<-SCORE_minusengagement_summed_minusproj ~.
metric<-"RMSE"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.001, 0.0025, 0.05, 0.01), n.minobsinnode=c(3:6))  

set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.65)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #Started 2:47 finished 3:05 RMSE 13.68477. The final values used for the model were n.trees = 4900, interaction.depth = 2, shrinkage =0.001 and n.minobsinnode = 6. So doesn't change anythign and still lots of trees. 

#Try experiment with bagging fraction, typically 0.5-0.8 so explore in that range

set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.5)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #started 3:06 ended 3:20, RMSE 13.7793. Just slightly higher RMSE but less trees and quick. 

set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.6)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #Started 3:23 ended by 3:38, more trees

set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.5)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #started 3:39, not sure when ended 4200 trees and RMSE 13.837

#One more time with .55
set.seed(1990)
enforce.tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.55)
enforce.tuneScore
max(enforce.tuneScore$results["RMSE"]) #started 4:39 done 4:50, RMSE 13.6451 but trees higher to 5000 so stick with 0.5

####MT model

model<-QualityMT~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.001, 0.0025, 0.005, 0.01), n.minobsinnode=c(3,4,5,6)) 

set.seed(1990)
enforce.tuneMT<-train(model, data=DataFactor_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.70)
enforce.tuneMT
max(enforce.tuneMT$results["Accuracy"]) ## Started 1:39, ended sometime around 2:30 accuracy is 69.71% #he final values used for the model were n.trees = 2000, interaction.depth= 2, shrinkage = 0.001 and n.minobsinnode = 4.

#now try with lower bagging fraction
set.seed(1990)
enforce.tuneMT<-train(model, data=DataFactor_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.60)
enforce.tuneMT
max(enforce.tuneMT$results["Accuracy"]) ##Started 2:34, ended around 2:30 or before. Worse, accuracy 68.92857

#Try again with higher bagging fraction
set.seed(1990)
enforce.tuneMT<-train(model, data=DataFactor_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.75)
enforce.tuneMT
max(enforce.tuneMT$results["Accuracy"]) #started 3:39, FInished 4:26. Accuracy is 0.706619. Accuracy was used to select the optimal model using the largest value. The final values used for the model were n.trees = 2000, interaction.depth = 2, shrinkage = 0.001 and n.minobsinnode = 4.

###PROJECTS MODEL
#run model
model<-Projects_DW~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.001, 0.0025, 0.005, 0.01), n.minobsinnode=c(3,4,5,6)) 

set.seed(1990)
enforce.tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.70)
enforce.tunePROJ
max(enforce.tunePROJ$results["Accuracy"]) #started sometime around 9:20 or 9:40, didn't note when it ended sadly accuracy is 73.43%, choosing highest shrinkage, try one more higher shrinkage maybe 0.025

model<-Projects_DW~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.0025, 0.005, 0.01, 0.025), n.minobsinnode=c(3,4,5,6)) 

set.seed(1990)
enforce.tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.70)
enforce.tunePROJ
max(enforce.tunePROJ$results["Accuracy"]) #started 10:38, ended sometime 10:53, even more accurage at 74%. Choose the highest shrinkage. 

model<-Projects_DW~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.005, 0.01, 0.025, 0.05), n.minobsinnode=c(3,4,5,6)) 

set.seed(1990)
enforce.tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.70)
enforce.tunePROJ
max(enforce.tunePROJ$results["Accuracy"]) #started 11:00 am with higher learning rate grip again, finished around 11:15 accuracy is 75% but choose lower shrinkage and just ran more trees. That is probably more what we want

#now try with lower bagging fraction
set.seed(1990)
enforce.tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.6)
enforce.tunePROJ
max(enforce.tunePROJ$results["Accuracy"]) #Started 11:17, done by 11:32, accuracy up again 78.63%

#now try with lower bagging fraction again
set.seed(1990)
enforce.tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.55)
enforce.tunePROJ
max(enforce.tunePROJ$results["Accuracy"]) #Started 11:39, finished 11:53 accuracy 76.63% so down so go with 0.6
