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

#####... only uncomment the following command if you're not using the full training dataset to adjuste the models ...#####
#load(file="data/CROSSVAL-10-TEST.rData")

##### check the correlation between variables
# pdf(file="results/corr-att.pdf")
# correlations <- cor(dataHemophilia[, -ncol(dataHemophilia)])
# corrplot(correlations, method="circle")
# dev.off()
#####

# dataHemophilia contains all instances used to train our models
train<-dataHemophilia 
train.task <- mlr::makeClassifTask(data = train, target = "group", positive = "Others")

### running classifiers or just load pretrained ones

file.models[1] = "results/no-aug/random.forest"
# uncomment the following command if you need retrain it
output<-randomForest.class(train.task, test.task = NA, .MEASURE, save.model = file.models[1], threshold = 0.7)
#

file.models[2] = "results/no-aug/decision.tree"
# uncomment the following command if you need retrain it
#output<-decisionTree.class(train.task, test.task = NA, .MEASURE, save.model = file.models[2])
#

file.models[3] = "results/no-aug/svm.rad"
# uncomment the following command if you need retrain it
#output<-svm.class(train.task, test.task = NA, pol = FALSE, .MEASURE, save.model = file.models[3])
#

file.models[4] ="results/no-aug/svm.pol"
# uncomment the following command if you need retrain it
#output<-svm.class(train.task, test.task = NA, pol = TRUE, .MEASURE, save.model = file.models[4])
#

file.models[5] = "results/no-aug/naive.bayes"
# uncomment the following command if you need retrain it
#output<-naiveBayes.class(train.task, test.task = NA, .MEASURE, save.model = file.models[5])
#

file.models[6] = "results/no-aug/xg.boost"
# uncomment the following command if you need retrain it
#output<-xgboost.class(train.task, test.task = NA, .MEASURE, save.model = file.models[6])

############################################
#####... loading final test dataset ...#####
predict.dataset<-read.table(file="data/src/dataset_for_prediction_all_positions_v2.csv", 
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
dataHemophilia<-load.full.dataset(dataset = "data/severity-FVIII-Activity-Regression.Rdata")
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

pdf(file="results/roc-curve-no-aug.pdf")
plotROCCurves(df)
dev.off()

#mlr::performance(bmr, mlr::auc)





