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
# Code implemented to create the stratified folds
#	that will be used by the cross-validation approach
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

#' This method creates a set of k stratified folds 
#'	to be used by the cross-validation strategy
#' 
#' @param dataset - a data matrix
#' @param seed    - a seed for random variables
#' @param cv - the number of folds
#' @param next.int - important to estratify values even using numeric labels
#' @param label.index - column index with the labels
#' @return a list with instace indexes in every fold
#' @examples
#' cv.10<-cv.bin.strat.class(dataset=dataHemophilia, seed=123456, cv=10)
cv.bin.strat.reg<-function(dataset, seed=123456, next.int=1, label.index=ncol(dataset), cv=10){
  set.seed(seed)
  
  cv.folds<-list()
  
  
  dataset$disc <- cut(dataset[,label.index], 
                     breaks=c(-Inf, 0.9999999, Inf), 
                     labels=c("a","b"))
  
  balance<-table(dataset$disc)  
  
  n.inst<-ceiling((nrow(dataset))*(1/cv)) #number of instances in every fold
  n.inst.c0<-round(n.inst*round(balance[1]/sum(balance), 2), 0) %>% as.numeric()
  
  actual.inst<-(1:nrow(dataset))
  
  for(cv.i in 1:(cv-1)){
    tmp<-which(dataset$disc[actual.inst]==(names(balance)[1]))
    c1<-actual.inst[sample(tmp, n.inst.c0)]
    tmp<-which(dataset$disc[actual.inst]==(names(balance)[2]))
    c2<-actual.inst[sample(tmp, n.inst-length(c1))]
    cv.folds[[cv.i]]<-sample(c(c1,c2)) #shuffle our cv.values
    actual.inst<-subset(actual.inst, !actual.inst%in%c(c1,c2))
  }
  
  cv.folds[[cv]]<-actual.inst
  
  invisible(cv.folds)
}




