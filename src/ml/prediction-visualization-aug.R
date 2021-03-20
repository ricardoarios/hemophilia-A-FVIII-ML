# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# This code is released as part of the manuscript
# "Prediction of hemophilia A severity using a small-input machine learning framework", by Lopes et al., submitted in 2021.

###testing all classifiers
rm(list=ls())

#####... load dataset ...#####
# V3 - based on regression
source("src/preprocessing/load-data.R")


#####... load validation measures ...#####
source("src/validation/validation-GreyZone.R")

#####... load classifiers ...#####
source("src/ml/rf.R")
source("src/ml/svm.R")
source("src/ml/dt.R")
source("src/ml/svm.R")
source("src/ml/naiveBayes.R")
source("src/ml/xgboost.R")


#####... Save prediction result ...#####
#classification.prediction<-c()
file.models<-c()


#####... select your measure ...#####
.MEASURE = list(kappa, mlr::auc)
#listMeasures("classif")

#####... using data augmentation ...#####
###
## This was the best smote function to balance the dataset
#r<-ADAS(X=dataHemophilia[, -ncol(dataHemophilia)], target=dataHemophilia[, ncol(dataHemophilia)], K=2)	#	acc: 0.8711039
#dataHemophilia<-as.data.frame(r$data)
#dataHemophilia$group<-as.factor(dataHemophilia$class)
#dataHemophilia$class<-NULL
#cv.10<-cv.bin.strat.class(dataset=dataHemophilia, seed=123456, cv=10)
#save(dataHemophilia, file="data/dataHemophilia-v3-aug.rData")
#save(cv.10, file="data/CROSSVAL-10-TEST-AUG.rData")
###

###################################################################
#####... WITH DATA AUG ...#####
load(file="dataset/CROSSVAL-10-TEST-AUG.rData")
###################################################################

#####
#pdf(file="results/corr-att.pdf")
#correlations <- cor(dataHemophilia[, -ncol(dataHemophilia)])
#corrplot(correlations, method="circle")
#dev.off()
#####

train<-dataHemophilia 
train.task <- mlr::makeClassifTask(data = train, target = "group", positive = "Others")

###playing w/ classifiers
file.models[1] = "results/aug/random.forest"
output<-randomForest.class(train.task, test.task = NA, .MEASURE, save.model = file.models[1], threshold = 0.7)
#
file.models[2] = "results/aug/decision.tree"
output<-decisionTree.class(train.task, test.task = NA, .MEASURE, save.model = file.models[2])
#
file.models[3] = "results/aug/svm.rad"
output<-svm.class(train.task, test.task = NA, pol = FALSE, .MEASURE, save.model = file.models[3])
#
file.models[4] ="results/aug/svm.pol"
output<-svm.class(train.task, test.task = NA, pol = TRUE, .MEASURE, save.model = file.models[4])
#
file.models[5] = "results/aug/naive.bayes"
output<-naiveBayes.class(train.task, test.task = NA, .MEASURE, save.model = file.models[5])
#
file.models[6] = "results/aug/xg.boost"
output<-xgboost.class(train.task, test.task = NA, .MEASURE, save.model = file.models[6])

############################################
#####... loading final test dataset ...#####
predict.dataset<-read.table(file="dataset/src/dataset_for_prediction_all_positions_v2.csv", 
	sep=";", header = T)
na.rows<-c()
for(i in 1:ncol(predict.dataset)){
  na.rows<-c(na.rows, which(is.na(predict.dataset[,i])))
}
if(length(na.rows) > 0){
  predict.dataset<-predict.dataset[-unique(na.rows), ]
}

### we reload the training dataset again just to make sure
# no training instance is again used during the test phase
dataHemophilia<-load.full.dataset(dataset = "dataset/severity-FVIII-Activity-Regression.Rdata")
dataHemophilia<-dataHemophilia[-c(dataHemophilia[,-c(1:9,ncol(dataHemophilia))] %>% duplicated() %>% which()), ]


test<-anti_join(predict.dataset[,-c(1:9)], dataHemophilia[,-c(1:9, ncol(dataHemophilia))])

test<-predict.dataset[, -c(1:9)] #%>% select(AA_dist, areaSAS, areaSES, consurfDB, degree, burts, pr)
test$group<-rep("Others", nrow(test))

test.task <- mlr::makeClassifTask(data = test, target = "group", positive = "Others")
############################################


#####... using pretrained models to classify the test dataset ...#####
for(i in 1:length(file.models)){
  
  load(file=file.models[i])
  output<-predict(model, test.task)
  
  predict.result.by.classifier<-cbind(predict.dataset, 
                         output %>% as.data.frame() %>% select(prob.Severe, prob.Others))
  
  write.table(predict.result.by.classifier, 
              file=paste(sep="", file.models[i], ".csv"), row.names = F, sep=",")
  
}


#####################################################
#####... The following commands are executed ########
##### just to plot the ROC curve...          ########
##################################################### 

####file.models[1]

class.learner <- makeLearner("classif.randomForest", predict.type = "prob", 
                             predict.threshold = 0.7)
class.learner$par.vals <- list(importance = TRUE)

load(paste(sep="", file.models[1],  ".tune"))

rf.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
rf.best.model$id<-"Random Forest"

####file.models[2]

class.learner <- makeLearner("classif.rpart", predict.type = "prob", predict.threshold = 0.7)

load(paste(sep="", file.models[2],  ".tune"))
dt.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
dt.best.model$id<-"Decision Tree"

####file.models[3]

class.learner <- makeLearner("classif.svm", predict.type = "prob", kernel="radial", cost = 1000)
class.learner$par.vals <- list(importance = TRUE)

load(paste(sep="", file.models[3],  ".tune"))
svm.rad.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
svm.rad.best.model$id<-"SVM (Radial)"


####file.models[4]

class.learner <- makeLearner("classif.svm", predict.type = "prob", kernel="polynomial", 
                             cost = 1000, predict.threshold = 0.7)
class.learner$par.vals <- list(importance = TRUE)

load(paste(sep="", file.models[4],  ".tune"))
svm.pol.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
svm.pol.best.model$id<-"SVM (Polynomial)"

####file.models[5]

class.learner <- makeLearner("classif.naiveBayes", predict.type = "prob", predict.threshold = 0.7)

load(paste(sep="", file.models[5],  ".tune"))
naive.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
naive.best.model$id<-"Naive Bayes"

####file.models[6]

class.learner <- makeLearner("classif.xgboost", predict.type = "prob", 
                             par.vals = list(
                               objective = "binary:logistic",
                               eval_metric = "error")
)
class.learner$par.vals <- list(importance = TRUE)

load(paste(sep="", file.models[6],  ".tune"))

xg.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
xg.best.model$id<-"XGBoost"

##### run all

bmr = benchmark(list(rf.best.model, dt.best.model, svm.rad.best.model, svm.pol.best.model, naive.best.model, xg.best.model), 
                tasks = train.task, resampling = mlr::makeResampleDesc("CV", iters = 10L, stratify = TRUE), 
                   measures = .MEASURE, show.info = FALSE)

df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr))

pdf(file="results/roc-curve-aug.pdf")
plotROCCurves(df)
dev.off()





