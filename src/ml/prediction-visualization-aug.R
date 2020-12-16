###testing all classifiers
rm(list=ls())

#####... load dataset ...#####
# V3 - based on regression
source("R/AM/classification/reg-class/load-data.R")


#####... load validation measures ...#####
source("R/AM/validation-GreyZone.R")

#####... load classifiers ...#####
source("R/AM/classification/reg-class/rf.R")
source("R/AM/classification/reg-class/dt.R")
source("R/AM/classification/reg-class/svm.R")
source("R/AM/classification/reg-class/naiveBayes.R")
source("R/AM/classification/reg-class/xgboost.R")


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
#load(file="data/dataHemophilia-v3-aug.rData")
#load(file="data/CROSSVAL-10-TEST-AUG.rData")
#####... WITH NO DATA AUG ...#####
load(file="data/CROSSVAL-10-TEST.rData")
#####... Individual running only used along with xgboost...#####
#test.fold = 10 
###################################################################

#####
pdf(file="results/corr-att.pdf")
correlations <- cor(dataHemophilia[, -ncol(dataHemophilia)])
corrplot(correlations, method="circle")
dev.off()
#####

train<-dataHemophilia 
train.task <- mlr::makeClassifTask(data = train, target = "group", positive = "Others")

###playing w/ classifiers
#file.models[1] = "results/v3/aug/random.forest"
file.models[1] = "results/v3/no-aug/random.forest"
#output<-randomForest.class(train.task, test.task = NA, .MEASURE, save.model = file.models[1], threshold = 0.7)
#
#file.models[2] = "results/v3/aug/decision.tree"
file.models[2] = "results/v3/no-aug/decision.tree"
#output<-decisionTree.class(train.task, test.task = NA, .MEASURE, save.model = file.models[2])
#
#file.models[3] = "results/v3/aug/svm.rad"
file.models[3] = "results/v3/no-aug/svm.rad"
#output<-svm.class(train.task, test.task = NA, pol = FALSE, .MEASURE, save.model = file.models[3])
#
#file.models[4] = "results/v3/aug/svm.pol"
file.models[4] ="results/v3/no-aug/svm.pol"
#output<-svm.class(train.task, test.task = NA, pol = TRUE, .MEASURE, save.model = file.models[4])
#
#file.models[5] = "results/v3/aug/naive.bayes"
file.models[5] = "results/v3/no-aug/naive.bayes"
#output<-naiveBayes.class(train.task, test.task = NA, .MEASURE, save.model = file.models[5])
#
#file.models[6] = "results/v3/aug/xg.boost"
file.models[6] = "results/v3/no-aug/xg.boost"
#output<-xgboost.class(train.task, test.task = NA, .MEASURE, save.model = file.models[6])

#load("/tmp/test.model")
#model$learner.model$confusion

#output=predict(model, train.task)
#my.accuracy(pred = output$data$response, truth = output$data$truth)$table

#classification.prediction<-rbind(classification.prediction, 
#                                 check_results(data.frame(severity=output$data$truth, 
#                                                          prob.Severe = output$data$prob.Severe)))



#####... loading final test ...#####
#predict.dataset<-read.table(file="data/src/dataset_for_prediction_v1.csv", sep="\t", header = T)
predict.dataset<-read.table(file="data/src/dataset_for_prediction_all_positions_v2.csv", 
                            sep=";", header = T)
na.rows<-c()
for(i in 1:ncol(predict.dataset)){
  na.rows<-c(na.rows, which(is.na(predict.dataset[,i])))
}
if(length(na.rows) > 0){
  predict.dataset<-predict.dataset[-unique(na.rows), ]
}

#V3
dataHemophilia<-load.full.dataset(dataset = "data/severity-FVIII-Activity-Regression.Rdata")
dataHemophilia<-dataHemophilia[-c(dataHemophilia[,-c(1:9,ncol(dataHemophilia))] %>% duplicated() %>% which()), ]


test<-anti_join(predict.dataset[,-c(1:9)], dataHemophilia[,-c(1:9, ncol(dataHemophilia))])

test<-predict.dataset[, -c(1:9)] #%>% select(AA_dist, areaSAS, areaSES, consurfDB, degree, burts, pr)
test$group<-rep("Others", nrow(test))

test.task <- mlr::makeClassifTask(data = test, target = "group", positive = "Others")

for(i in 1:length(file.models)){
  
  load(file=file.models[i])
  output<-predict(model, test.task)
  
  predict.result.by.classifier<-cbind(predict.dataset, 
                         output %>% as.data.frame() %>% select(prob.Severe, prob.Others))
  
  write.table(predict.result.by.classifier, 
              file=paste(sep="", file.models[i], ".csv"), row.names = F, sep=",")
  
}


#################################################### PLOT ROC Curve

####file.models[1]

class.learner <- makeLearner("classif.randomForest", predict.type = "prob", 
                             predict.threshold = 0.7)
class.learner$par.vals <- list(importance = TRUE)

class.learner_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 4, upper = 100),#makeIntegerParam("ntree",lower = 10, upper = 500),
  makeIntegerParam("mtry", lower = 2, upper = 7),  
  makeIntegerParam("nodesize", lower = 1, upper = 5)#makeIntegerParam("nodesize", lower = 1, upper = 20)
)

load(paste(sep="", file.models[1],  ".tune"))

rf.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
rf.best.model$id<-"Random Forest"

####file.models[2]

class.learner <- makeLearner("classif.rpart", predict.type = "prob", predict.threshold = 0.7)

class.learner_param = makeParamSet(
  makeIntegerParam("minsplit",lower = 2, upper = 50),#minimum number of observations in a node
  makeIntegerParam("minbucket", lower = 1, upper = 20),#Minimum number of observations in a terminal node - same values used by RF (nodesize)makeIntegerParam("nodesize", lower = 1, upper = 20)
  makeNumericParam("cp", lower = 0.0001, upper = 1)#complexity parameter. The lower it is, the larger the tree will grow.
)

load(paste(sep="", file.models[2],  ".tune"))
dt.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
dt.best.model$id<-"Decision Tree"

####file.models[3]

class.learner <- makeLearner("classif.svm", predict.type = "prob", kernel="radial", cost = 1000)
class.learner$par.vals <- list(importance = TRUE)

class.learner_param = makeParamSet(
  #  makeDiscreteParam("cost", values = seq(0.5, 20, 0.5)),
  makeDiscreteParam("coef0", values = seq(0, 2, 0.1)), #seq(0, 2, 0.1)
  makeDiscreteParam("gamma", values = seq(0.01, 1.5, 0.01))
)

load(paste(sep="", file.models[3],  ".tune"))
svm.rad.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
svm.rad.best.model$id<-"SVM (Radial)"


####file.models[4]

class.learner <- makeLearner("classif.svm", predict.type = "prob", kernel="polynomial", 
                             cost = 1000, predict.threshold = 0.7)
class.learner$par.vals <- list(importance = TRUE)

class.learner_param = makeParamSet(
  makeDiscreteParam("gamma", values = seq(0.1, 2, 0.05)), #seq(0.1, 2, 0.05)
  makeDiscreteParam("coef0", values = seq(0, 2, 0.1)), #seq(0, 2, 0.1)
  makeDiscreteParam("degree", values = seq(2, 5, 1)) #seq(1, 4, 1)
)

load(paste(sep="", file.models[4],  ".tune"))
svm.pol.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
svm.pol.best.model$id<-"SVM (Polynomial)"

####file.models[5]

class.learner <- makeLearner("classif.naiveBayes", predict.type = "prob", predict.threshold = 0.7)

class.learner_param = makeParamSet(
  makeIntegerParam("laplace", lower = 0, upper = 1)
)

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

class.learner_param <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 1, upper = 20), # pior é sempre igual ao número de samples
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 25), # 25
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = 0, upper = 1),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x) # -10 ???
)

load(paste(sep="", file.models[6],  ".tune"))

xg.best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
xg.best.model$id<-"XGBoost"

##### run all

bmr = benchmark(list(rf.best.model, dt.best.model, svm.rad.best.model, svm.pol.best.model, naive.best.model, xg.best.model), 
                tasks = train.task, resampling = mlr::makeResampleDesc("CV", iters = 10L, stratify = TRUE), 
                   measures = .MEASURE, show.info = FALSE)

df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr))

#pdf(file="results/roc-curve-aug.pdf")
pdf(file="results/roc-curve-no-aug.pdf")
plotROCCurves(df)
dev.off()

mlr::performance(bmr, mlr::auc)

#load(paste(sep="", file.models, ".tune"))
#model_tune

#load(paste(sep="", file.models, ".feat"))
#sfeats
##############################





