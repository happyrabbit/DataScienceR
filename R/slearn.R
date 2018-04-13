## code for super learner
library(caret)
library(MASS)
library(pROC)
library(pls)
library(sparseLDA)
library(subselect)
library(e1071)
library(kernlab)
library(svmpath)
library(nnet)
library(randomForest)
library(grplasso)
library(dplyr)
###########################################################
ctrl=trainControl(method="cv",number=10)
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#ntree=2000
tunegrid <- expand.grid(mtry=c(25:35))
#str(tunegrid)

rf_model<-train(trainx,trainy,method="rf",
                trControl=ctrl,
                tuneGrid=tunegrid,
                prox=TRUE,
                allowParallel=TRUE)
#####################################################
ntree=1000
rfit=randomForest(trainx,trainy,ntree=ntree,mtry=57)
plot(rfit)
# sum(names(testx)==names(trainx))
# summary(testx)

hat_rf = predict(rfit,testx)

#y18 = sum(hat_rf)
#y17 = sum(trainy)
#y16 = sum(trainx$CPA_DISC_AMT2016)
#y15 = sum(trainx$CPA_DISC_AMT2015)
#y14 = sum(trainx$CPA_DISC_AMT2014)
#x = c(14, 15, 16, 17, 18)
#plot(x, y = -c(y14, y15, y16, y17, y18))
#y15-y14
#y16-y15
#y17-y16
#y18-y17
# sum(hat_rf)
# summary(hat_rf)
# sum(testy)
############################### random forest
imp=data.frame(importance(rfit))
imp$variable=row.names(imp)
imp=imp[order(imp$IncNodePurity,decreasing = T),]
head(imp,50)
############################### mars
library(earth)  
correlation<-cor(trainx)
highcor<-caret::findCorrelation(correlation, cutoff=0.75)
names(trainx)[highcor]
ltrainx=trainx[,-highcor]

marsGrid<-expand.grid(.degree=1:5,.nprune=2:30)
set.seed(100)

marsTune<-train(ltrainx,trainy,method="earth",tuneGrid = marsGrid,
                trControl = ctrl)

marsFit<-earth(ltrainx,trainy,degree=1, nprune=23)
# summary(marsFit)
hat_mars=predict(marsFit,subset(testx,select=names(ltrainx)))
# summary(hat_mars)
attr(hat_mars, "dimnames")[[2]] <- "hat_mars"

# sum(hat_mars)
############################### linear regression
#lmfit=train(subset(Xtrain,select=selnam),solTrainY,method="lm",trControl = ctrl)
lmfit=train(ltrainx,trainy,method="lm",trControl = ctrl)
hat_lm=predict(lmfit,subset(testx,select=names(ltrainx)))
# sephat_lm[which(sephat_lm>0)]<-0
# sum(hat_lm)
hat_lm[which(hat_lm>0)] <- 0
#r2_lm <- cor(hat_lm, testy)^2
#rmse_lm = sqrt(mean((hat_lm - testy)^2))

############################### Linear regression + AIC 
lmfit=lm(trainy~.,data=ltrainx)
# summary(lmfit)
library(MASS)
saicfit=stepAIC(lmfit)
summary(saicfit)
hat_saic=predict(saicfit,subset(testx,select=names(ltrainx)))
#sephat_saic[which(sephat_saic>0)]<-0
sum(hat_saic)
hat_saic[which(hat_saic>0)]<-0
# r2_saic <- cor(hat_saic, testy)^2
# rmse_saic = sqrt(mean((hat_saic - testy)^2))
############################### robust linear regression
#rlmfit=train(xtrain,ytrain,method="rlm",
#             preProcess="pca",
#             trControl = ctrl)
#sephat_rlm=predict(rlmfit,test17)
#sum(sephat_rlm)
############################### Partial Least Square
plsfit=train(trainx,trainy,method="pls",
             prePro=c("center","scale"),
             tuneLength = 20,
             trControl = ctrl)
hat_pls=predict(plsfit, testx)
#sephat_pls[which(sephat_pls>0)]<-0
hat_pls[which(hat_pls>0)]<-0
# r2_pls <- cor(hat_pls, testy)^2
# rmse_pls = sqrt(mean((hat_pls - testy)^2))
sum(hat_pls)
############################### Ridge Regression
ridgeGrid = data.frame(.lambda=seq(0,0.2,length=50))
set.seed(100)
# lambda = 0.1001001
ridgefit = train(trainx,trainy,method="ridge",
               prePro=c("center","scale"),
               tuneGrid = ridgeGrid,
               trControl = ctrl)
hat_ridge = predict(ridgefit, testx)
#sephat_ridge[which(sephat_ridge>0)]<-0
hat_ridge[which(hat_ridge>0)]<-0
# r2_ridge <- cor(hat_ridge, testy)^2
# rmse_ridge = sqrt(mean((hat_ridge - testy)^2))
sum(hat_ridge)
############################### Elastic Network
enetGrid=expand.grid(.lambda=seq(0,.2,length=15),
                     .fraction=seq(0.05,1,length=20))
set.seed(100)

enetfit=train(trainx, trainy, method="enet",
              prePro=c("center","scale"),
              tuneGrid = enetGrid,
              trControl = ctrl)

enetfit = enet(as.matrix(trainx), trainy, lambda= 0, normalize = T)

hat_enet=predict(enetfit, as.matrix(testx), s= 0.25, mode="fraction", type="fit")$fit

hat_enet[which(hat_enet>0)]<-0
# r2_enet <- cor(hat_enet, testy)^2
# rmse_enet = sqrt(mean((hat_enet - testy)^2))
sum(hat_enet)
##########################################################
############################### Elastic Network+ Linear Regression
#enelmfit=lm(ytrain~.,data=subset(xtrain,select=enenam))
#sephat_enelm=predict(enelmfit,data=subset(test17,select=enenam))
#summary(data)
#summary(sephat_enelm)
##########################################################
############################### Support Vector Machines
svmfit=train(ltrainx, trainy,method="svmRadial",
             prePro=c("center","scale"),
             tuneLength = 20,
             trControl = ctrl)
#svmfit$finalModel
hat_svm=predict(svmfit,subset(testx,select=names(ltrainx)))
hat_svm[which(hat_svm>0)]<-0
sum(hat_svm)
# summary(hat_svm)
############################################GBM
gbmGrid <-  expand.grid(interaction.depth = 3,
                        n.trees = 1000,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

set.seed(100)
gbmTune <- train(trainx,trainy,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 ## The gbm() function produces copious amounts
                 ## of output, so pass in the verbose option
                 ## to avoid printing a lot to the screen.
                 verbose = FALSE)

# The final values used for the model were n.trees = 1000, interaction.depth = 3, shrinkage = 0.1
# and n.minobsinnode = 20.
# sepfit<-update(gbmTune,.interaction.depth=8,
#               .n.trees = 1000,
#               .shrinkage = 0.021)
hat_gbm<-predict(gbmTune,testx)
hat_gbm[which(hat_gbm>0)]<-0
sum(hat_gbm)
###############################################KNN
bootControl <- trainControl(number = 1)
knnGrid <- expand.grid(k=c(1:25))
set.seed(2)
knnFit1 <- train(trainx,trainy,
                 method = "knn", trControl = bootControl, verbose = FALSE,
                 tuneGrid = knnGrid )
hat_knn=predict(knnFit1,subset(testx,select=names(trainx)))
# r2_knn <- cor(hat_knn, testy)^2
# rmse_knn = sqrt(mean((hat_knn - testy)^2))
sum(hat_knn)
###############################################nnet 
# too many parameters
nnetGrid=expand.grid(.decay=c(0,0.01,.1),
                     .size=c(1:10),
                     .bag=F)
set.seed(100)
nnetTune=train(ltrainx,trainy,
               method="avNNet",
               trControl=ctrl,
               preProc = c("center","scale"),
               linout=T,
               trace=F,
               MaxNWts=10*(ncol(ltrainx)+1)+10+1,
               maxit=500)
hat_avNNet=predict(nnetTune,subset)
