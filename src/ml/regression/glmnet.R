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
# Code implemented to model the dataset using 
#	generalized linear regression (glm)
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


