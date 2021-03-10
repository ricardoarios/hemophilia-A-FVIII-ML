#######################################################
# Code implemented to model the dataset using Random
#	Forest
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
#' Random Forest models
#' 
#' @param train.task - train dataset
#' @param test.task  - test dataset
#' @return predicted values

randomForest.reg<-function(train.task, test.task){
  
  ####
  learn.method <- makeLearner("regr.randomForest", predict.type = "response")
  learn.method$par.vals <- list(importance = TRUE)
  
  class.learner_param = makeParamSet(
    makeIntegerParam("ntree",lower = 1, upper = 10),#makeIntegerParam("ntree",lower = 10, upper = 500),
    makeIntegerParam("mtry", lower = 1, upper = 7),  
    makeIntegerParam("nodesize", lower = 1, upper = 15)#makeIntegerParam("nodesize", lower = 1, upper = 20)
  )
  ####
  
  rancontrol <- mlr::makeTuneControlGrid()
  
  set_cv <- mlr::makeResampleDesc("CV",iters = 10L)
  
  parallelStart(mode="multicore", cpu=detectCores(), level="mlr.tuneParams")
  start<-Sys.time()
  model_tune <- mlr::tuneParams(learner = learn.method, resampling = set_cv, 
                                task = train.task, par.set = class.learner_param, 
                                control = rancontrol, measures = measure.severe,
                                show.info = T)
  
  print(Sys.time()-start)
  parallelStop()
  
  best.model <- mlr::setHyperPars(learn.method, par.vals = model_tune$x)
  model <- mlr::train(best.model, train.task)
  
  output<-predict(model, test.task)
  invisible(output)
  
}



