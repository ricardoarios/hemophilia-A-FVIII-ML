#######################################################
# Code implemented to model the dataset using SVM
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
#' SVM models
#' 
#' @param train.task - train dataset
#' @param test.task  - test dataset
#' @param measure    - list of measures used to seek the best parametrization
#' @param save.model - file name to save all random forest model and configuration
#' @param threshold  - cutoff point to decide based on probability values
#' @return predicted values

svm.reg<-function(train.task, test.task, pol){
  ####
  ####testing polynomial
  if(pol){
    learn.method <- makeLearner("regr.svm", predict.type = "response", kernel="polynomial", cost = 1000)
    learn.method$par.vals <- list(importance = TRUE)

    class.learner_param = makeParamSet(
      makeDiscreteParam("gamma", values = seq(0.01, 2, 1)), #seq(0.1, 2, 0.05)
      makeDiscreteParam("coef0", values = seq(0, 2, 0.1)), #seq(0, 2, 0.1)
      makeDiscreteParam("degree", values = seq(2, 5, 1)) #seq(1, 4, 1)
    )
  }else{
    ####testing radial
    learn.method <- makeLearner("regr.svm", predict.type = "response", kernel="radial", cost = 1000)
    learn.method$par.vals <- list(importance = TRUE)

    class.learner_param = makeParamSet(
      #  makeDiscreteParam("cost", values = seq(0.5, 20, 0.5)),
      makeDiscreteParam("gamma", values = seq(0.01, 1.5, 0.01))
    )
    ####
  }
  rancontrol <- mlr::makeTuneControlGrid()

  set_cv <- mlr::makeResampleDesc("CV",iters = 10L)

  parallelStart(mode="multicore", cpu=detectCores(), level="mlr.tuneParams")
  start<-Sys.time()
  model_tune <- mlr::tuneParams(learner = learn.method, resampling = set_cv,
                                task = train.task, par.set = class.learner_param,
                                #control = rancontrol, measures = mlr::rmse,
                                control = rancontrol, measures = measure.severe,
                                show.info = T)

  print(Sys.time()-start)
  parallelStop()

  best.model <- mlr::setHyperPars(learn.method, par.vals = model_tune$x)
  model <- mlr::train(best.model, train.task)

  output<-predict(model, test.task)
  invisible(output)
}
