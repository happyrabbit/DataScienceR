library(plyr)
library(caret)
library(dplyr)
library(nnet)
library(randomForest)
library(earth)
modeldat<-read.csv("C:/Users/linhu/Desktop/2017Str/CleanData/FinalSaleModel.csv")
#load("C:/Users/linhu/Desktop/2017Str/Results/reliefValue1415.RData")

strdat<-read.csv("C:/Users/linhu/Desktop/2017Str/RawData/CUSTCORN2016.csv")%>%
  dplyr::select(BU_CD,SLS_AREA_CD,SLS_TERR_CD)%>%
  distinct()
# names(strdat)
modeldat<-merge(modeldat,strdat,all.x=T)
#######Training: data from 2010 to 2014, SEP, OCT
inctrain<-c(#2014
  "CumSumUnits201309","CumSumUnits201310","CumSumUnits201311",
  "CumSumUnits201312","CumSumUnits201401","CumSumUnits201402","CumSumUnits201403",
  "CumSumUnits201404","CumSumUnits201405","CumSumUnits201406","CumSumUnits201407",
  "CumSumUnits201408",
  #2013
  "CumSumUnits201209","CumSumUnits201210","CumSumUnits201211",
  "CumSumUnits201212","CumSumUnits201301","CumSumUnits201302","CumSumUnits201303",
  "CumSumUnits201304","CumSumUnits201305","CumSumUnits201306","CumSumUnits201307",
  "CumSumUnits201308",
  #2012
  "CumSumUnits201109","CumSumUnits201110","CumSumUnits201111",
  "CumSumUnits201112","CumSumUnits201201","CumSumUnits201202","CumSumUnits201203",
  "CumSumUnits201204","CumSumUnits201205","CumSumUnits201206","CumSumUnits201207",
  "CumSumUnits201208",
  # 2011
  "CumSumUnits201009","CumSumUnits201010","CumSumUnits201011",
  "CumSumUnits201012","CumSumUnits201101","CumSumUnits201102","CumSumUnits201103",
  "CumSumUnits201104","CumSumUnits201105","CumSumUnits201106","CumSumUnits201107",
  "CumSumUnits201108",
  # 2010
  "CumSumUnits200909","CumSumUnits200910","CumSumUnits200911",
  "CumSumUnits200912","CumSumUnits201001","CumSumUnits201002","CumSumUnits201003",
  "CumSumUnits201004","CumSumUnits201005","CumSumUnits201006","CumSumUnits201007",
  "CumSumUnits201008",
  # pros
  "ProsUnits2012","ProsCPA2012",  "ProsEQ2012",   "ProsDP2012",   "ProsRet2012",
  "ProsCS2012",   "ProsUnits2013","ProsCPA2013",  "ProsEQ2013",   "ProsDP2013",   "ProsRet2013",
  "ProsCS2013",   "ProsUnits2014","ProsCPA2014",  "ProsEQ2014",   "ProsDP2014",   "ProsRet2014",
  "ProsCS2014",
  # soph
  "SophUnits2012","SophCPA2012",  "SophEQ2012",   "SophDP2012",   "SophRet2012",
  "SophCS2012",   "SophUnits2013","SophCPA2013",  "SophEQ2013",   "SophDP2013",   "SophRet2013",
  "SophCS2013",   "SophUnits2014","SophCPA2014",  "SophEQ2014",   "SophDP2014",   "SophRet2014",
  "SophCS2014",
  # cust
  "CustUnits2012","CustCPA2012",  "CustEQ2012",   "CustDP2012",   "CustRet2012",
  "CustCS2012",   "CustUnits2013","CustCPA2013",  "CustEQ2013",   "CustDP2013",   "CustRet2013",
  "CustCS2013",   "CustUnits2014","CustCPA2014",  "CustEQ2014",   "CustDP2014",   "CustRet2014",
  "CustCS2014"
)

inctest<-gsub("2014","2015",inctrain)
inctest<-gsub("2013","2014",inctest)
inctest<-gsub("2012","2013",inctest)
inctest<-gsub("2011","2012",inctest)
inctest<-gsub("2010","2011",inctest)
inctest<-gsub("2009","2010",inctest)
##########################################################################
#names(modeldat)
CUIND=data.frame(class.ind(modeldat$BU_CD))%>%
  select(-X1B)
names(CUIND)=substr(names(CUIND),1,3)

##################### Oct model
solTrainXtrans<-subset(modeldat,select=c(inctrain,
                                         "CumSumUnits201409",
                                         "CumSumUnits201410"))
solTrainXtrans$sepdiff<-modeldat$CumSumUnits201409-modeldat$CumSumUnits201309
solTrainXtrans$octdiff<-modeldat$CumSumUnits201410-modeldat$CumSumUnits201310

solTrainY<-modeldat$CumSumUnits201508
solTrainXtrans=cbind(solTrainXtrans,CUIND)


testdat<-subset(modeldat,select=c(inctest,
                                  "CumSumUnits201509",
                                  "CumSumUnits201510"))
testdat$sepdiff<-testdat$CumSumUnits201509-testdat$CumSumUnits201409
testdat$octdiff<-testdat$CumSumUnits201510-testdat$CumSumUnits201410
testdat=cbind(testdat,CUIND)

names(testdat)<-names(solTrainXtrans)
true16<-modeldat$CumSumUnits201608
##########################################################################
##########################################################################
ctrl=trainControl(method="cv",number=10)
ntree=5000
#i=100
#Xtrain=subset(solTrainXtrans,select=varname[1:i])
#Xtest=subset(testdat,select=varname[1:i])
Xtrain=solTrainXtrans
Xtest=testdat
############################### random forest
rfit=randomForest(Xtrain,solTrainY,ntree=ntree)
octhat_rf=predict(rfit,Xtest)
sum(sephat_rf)
imp=data.frame(importance(rfit))
imp$variable=row.names(imp)
imp=imp[order(imp$IncNodePurity,decreasing = T),]
selnam=imp$variable[1:50]
rfit2=randomForest(subset(Xtrain,select=selnam),solTrainY,ntree=ntree)
sephat_rf2=predict(rfit2,Xtest)
sum(sephat_rf2)

#sum(sephat_rf)
############################### mars
library(earth)
#marsGrid<-expand.grid(.degree=1:2,.nprune=2:10)
set.seed(100)
#marsTune<-train(Xtrain,solTrainY,method="earth",tuneGrid = marsGrid,
#                trControl = trainControl(method="cv"))
#marsFit<-earth(Xtrain,solTrainY,degree=1, nprune=3)
marsFit<-earth(Xtrain,solTrainY,degree=1, nprune=3)
summary(marsFit)
octhat_mars=predict(marsFit,Xtest)
#sum(octhat_mars)
############################### linear regression
#lmfit=train(subset(Xtrain,select=selnam),solTrainY,method="lm",trControl = ctrl)
lmfit=train(Xtrain,solTrainY,method="lm",trControl = ctrl)
octhat_lm=predict(lmfit,Xtest)
sum(octhat_lm)
############################### Linear regression + AIC
lmfit=lm(solTrainY~.,data=Xtrain)
summary(lmfit)
library(MASS)
saicfit=stepAIC(lmfit)
octhat_saic=predict(saicfit,Xtest)
sum(octhat_saic)
############################### robust linear regression
rlmfit=train(Xtrain,solTrainY,method="rlm",
             preProcess="pca",
             trControl = ctrl)
octhat_rlm=predict(rlmfit,Xtest)
sum(octhat_rlm)
############################### Partial Least Square
plsfit=train(Xtrain,solTrainY,method="pls",
             prePro=c("center","scale"),
             tuneLength = 20,
             trControl = ctrl)
octhat_pls=predict(plsfit,Xtest)
sum(octhat_pls)
############################### Ridge Regression
ridgeGrid=data.frame(.lambda=seq(0,0.2,length=50))
set.seed(100)
# lambda = 0.1001001
ridgefit=train(Xtrain,solTrainY,method="ridge",
               prePro=c("center","scale"),
               tuneGrid = ridgeGrid,
               trControl = ctrl)
octhat_ridge=predict(ridgefit,Xtest)
sum(octhat_ridge)
############################### Elastic Network
enetGrid=expand.grid(.lambda=seq(0,.2,length=15),
                     .fraction=seq(0.05,1,length=20))
set.seed(100)
enetfit=train(Xtrain,solTrainY,method="enet",
              prePro=c("center","scale"),
              tuneGrid = enetGrid,
              trControl = ctrl)
enetfit=enet(as.matrix(Xtrain),solTrainY,lambda=0.01428571,normalize = T)

octhat_enet=predict(enetfit,as.matrix(Xtest),s=0.35,mode="fraction",type="fit")$fit

octcoef_enet=predict(enetfit,as.matrix(Xtest),s=0.4,mode="fraction",type="coefficients")$coefficients
enenam<-names(sort(octcoef_enet[octcoef_enet!=0]))
sum(octhat_enet)
##########################################################
############################### Elastic Network+ Linear Regression
enelmfit=lm(solTrainY~.,data=subset(Xtrain,select=enenam))
octhat_enelm=predict(enelmfit,data=subset(Xtest,select=enenam))
sum(octhat_enelm)
##########################################################
############################### Support Vector Machines
svmfit=train(Xtrain,solTrainY,method="svmRadial",
             prePro=c("center","scale"),
             tuneLength = 20,
             trControl = ctrl)
#svmfit$finalModel
octhat_svm=predict(svmfit,Xtest)
############################################GBM
gbmGrid <-  expand.grid(interaction.depth = c(9:15),
                        n.trees = 1000,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

set.seed(100)
gbmTune <- train(Xtrain,solTrainY,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 ## The gbm() function produces copious amounts
                 ## of output, so pass in the verbose option
                 ## to avoid printing a lot to the screen.
                 verbose = FALSE)
# The final values used for the model were n.trees = 1000, interaction.depth = 14, shrinkage = 0.1
# and n.minobsinnode = 20.
# sepfit<-update(gbmTune,.interaction.depth=8,
#               .n.trees = 1000,
#               .shrinkage = 0.021)
octhat_gbm<-predict(gbmTune,Xtest)
sum(octhat_gbm)
###############################################KNN
bootControl <- trainControl(number = 1)
knnGrid <- expand.grid(k=c(2:15))
set.seed(2)
knnFit1 <- train(Xtrain,solTrainY,
                 method = "knn", trControl = bootControl, verbose = FALSE,
                 tuneGrid = knnGrid )
octhat_knn=predict(knnFit1,Xtest)
###############################################nnet
toohigh=findCorrelation(cor(Xtrain),cutoff=0.75)
trainXnnet=Xtrain[,-toohigh]
testXnnet=Xtest[,-toohigh]
nnetGrid=expand.grid(.decay=c(0,0.01,.1),
                     .size=c(1:10),
                     .bag=F)
set.seed(100)
library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

nnetTune=train(trainXnnet,solTrainY,
               method="avNNet",
               trControl=ctrl,
               preProc = c("center","scale"),
               linout=T,
               trace=F,
               MaxNWts=10*(ncol(trainXnnet)+1)+10+1,
               maxit=500)
octhat_avNNet=predict(nnetTune,testXnnet)

tot=c(sum(octhat_avNNet),
      sum(octhat_knn),
      sum(octhat_enelm),
      sum(octhat_enet),
      sum(octhat_ridge),
      sum(octhat_pls),
      sum(octhat_rlm),
      sum(octhat_saic),
      sum(octhat_lm),
      sum(octhat_mars)
      #sum(octhat_gbm),
      #sum(octhat_rf)
      #sum(octhat_svm)
)

mean(tot)
sum(true16)

res<-data.frame(cbind(octhat_avNNet=octhat_avNNet,
                      octhat_knn=octhat_knn,
                      octhat_enelm=octhat_enelm,
                      octhat_enet=octhat_enet,
                      octhat_ridge=octhat_ridge,
                      octhat_pls=octhat_pls,
                      octhat_rlm=octhat_rlm,
                      octhat_saic=octhat_saic,
                      octhat_lm=octhat_lm,
                      octhat_mars=octhat_mars,
                      octhat_gbm=octhat_gbm,
                      octhat_rf=octhat_rf,
                      octhat_svm=octhat_svm,
                      true16))

res$SLS_TERR_CD<-modeldat$SLS_TERR_CD

res<-merge(res,strdat,all.x=T)
res%>%
  dplyr::select(-SLS_TERR_CD)%>%
  dplyr::select(-SLS_AREA_CD)%>%
  group_by(BU_CD)%>%
  summarise_each(funs(sum))
