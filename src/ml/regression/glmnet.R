#######################################################
# Code implemented to model the dataset using 
#	generalized linear regression (glm)
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
#' glm models
#' 
#' @param train.task - train dataset
#' @param test.task  - test dataset
#' @return predicted values
glm.reg<-function(train.task, test.task){
  
  ####
  learn.method <- makeLearner("regr.glmnet", predict.type = "response", 
                              family ="gaussian", fix.factors.prediction = TRUE)
                              #family ="poisson", fix.factors.prediction = TRUE)
  #learn.method$par.vals <- list(importance = TRUE)
  
  class.learner_param = makeParamSet(
    makeDiscreteParam("s", values = seq(0.01, 1, by=.05)), 
    makeDiscreteParam("alpha", values = seq(0., 1, by=.1))
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
  
  print(model)
  invisible(output)
  
}


