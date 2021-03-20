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
#
# Code implemented to model the dataset using Random
#	Forest
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



