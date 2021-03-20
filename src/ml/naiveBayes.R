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
#######################################################
# Code implemented to model the dataset using Naive 
#	Bayes
#
# This code is part of the Hema-Class framework
# Date: December, 2020
#
# Developers: Tiago Lopes, 
#		Ricardo Rios, 
#		Tatiane Nogueira, 
#		Rodrigo Mello
#
#######################################################

#' This method runs a grid search to look for the best
#' Naive Bayes models
#' 
#' @param train.task - train dataset
#' @param test.task  - test dataset
#' @param measure    - list of measures used to seek the best parametrization
#' @param save.model - file name to save all random forest model and configuration
#' @param threshold  - cutoff point to decide based on probability values
#' @return predicted values

naiveBayes.class<-function(train.task, test.task = NA, measure = list(tpr), save.model=NULL, threshold = 0.7){
  
  ####
  ###using rpart
  class.learner <- makeLearner("classif.naiveBayes", predict.type = "prob", predict.threshold = threshold)
  
  class.learner_param = makeParamSet(
    makeIntegerParam("laplace", lower = 0, upper = 1)
  )
  
  rancontrol <- mlr::makeTuneControlGrid()
  
  set_cv <- mlr::makeResampleDesc("CV", iters = 10L, stratify = TRUE)
  
  parallelStart(mode="multicore", cpu=detectCores(), level="mlr.tuneParams")
  start<-Sys.time()
  model_tune <- mlr::tuneParams(learner = class.learner, resampling = set_cv, 
                                task = train.task, par.set = class.learner_param, 
                                #control = rancontrol, measures = list(acc, setAggregation(acc,test.sd)),
                                #control = rancontrol, measures = measure.severe,
                                #control = rancontrol, measures = list(tpr),
                                control = rancontrol, measures = measure,
                                show.info = F)
  
  print(Sys.time()-start)
  parallelStop()
  
  best.model <- mlr::setHyperPars(class.learner, par.vals = model_tune$x)
  model <- mlr::train(best.model, train.task)
  
  #model <- mlr::train(class.learner, train.task)
  
  if(!is.null(save.model)){
    save(model, file=save.model) 
    save(model_tune, file=paste(sep="", save.model, ".tune"))
  }
  
  if(!is.na(test.task)){
    output<-predict(model, test.task, threshold = threshold)
    invisible(output)
  }
  
}

