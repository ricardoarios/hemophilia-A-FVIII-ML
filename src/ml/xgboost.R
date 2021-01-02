#######################################################
# Code implemented to model the dataset using xgboost 
#	
#
#######################################################
# This code is part of the Hema-Class framework
# Date: December, 2020
#
# Developers: Tiago Lopes, 
#		Ricardo Rios, 
#		Tatiane Nogueira, 
#		Rodrigo Mello
#
#
# GNU General Public License v3.0
#
# Permissions of this strong copyleft license are 
#	conditioned on making available complete 
#	source code of licensed works and 
#	modifications, which include larger works 
#	using a licensed work, under the same license. 
#	Copyright and license notices must be 
#	preserved. Contributors provide an express 
#	grant of patent rights.
#######################################################

#' This method runs a grid search to look for the best
#' xgboost models
#' 
#' @param train.task - train dataset
#' @param test.task  - test dataset
#' @param measure    - list of measures used to seek the best parametrization
#' @param save.model - file name to save all random forest model and configuration
#' @param threshold  - cutoff point to decide based on probability values
#' @return predicted values

xgboost.class<-function(train.task, test.task, measure, save.model=NULL, threshold = 0.7){
  
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
  
  #train.task <- xgb.DMatrix(as.matrix(train.task$env$data[,1:(ncol(train.task$env$data)-1)]), 
  #                          label=train.task$env$data[,ncol(train.task$env$data)])
  #train.task <- mlr::makeClassifTask(data = dataHem, target = "group", positive=positive)
  
  rancontrol <- mlr::makeTuneControlRandom(maxit = 50)
  #rancontrol <- mlr::makeTuneControlGrid()
  
  set_cv <- mlr::makeResampleDesc("CV",iters = 10L, stratify = TRUE)
  
  parallelStart(mode="multicore", cpu=detectCores(), level="mlr.tuneParams")
  #parallelStart(mode="multicore", cpu=28, level="mlr.tuneParams")
  start<-Sys.time()
  model_tune <- mlr::tuneParams(learner = class.learner, resampling = set_cv, 
                                task = train.task, par.set = class.learner_param, 
                                #control = rancontrol, measures = list(acc, setAggregation(acc,test.sd)),
                                control = rancontrol, measures = measure,
                                show.info = F)
  
  print(Sys.time()-start)
  parallelStop()
  
  best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
  model <- mlr::train(best.model, train.task)
  
  if(!is.null(save.model)){
    save(model, file=save.model)  
    save(model_tune, file=paste(sep="", save.model, ".tune"))
    bmr = benchmark(list(best.model), tasks = train.task, resampling = set_cv, 
                    measures = measure, show.info = FALSE)
    save(bmr, file=paste(sep="", save.model, ".bmr"))
  }
  
  if(!is.na(test.task)){
    output<-predict(model, test.task, threshold = threshold)
    invisible(output)
  }
  
}

