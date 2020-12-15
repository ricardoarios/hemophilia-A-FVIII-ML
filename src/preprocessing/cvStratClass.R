#######################################################
# Code implemented to create the stratified folds
#	that will be used by the cross-validation approach
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

#' This method creates a set of k stratified folds 
#'	to be used by the cross-validation strategy
#' 
#' @param dataset - a data matrix
#' @param seed    - a seed for random variables
#' @param cv - the number of folds
#' @return a list with instace indexes in every fold
#' @examples
#' cv.10<-cv.bin.strat.class(dataset=dataHemophilia, seed=123456, cv=10)
cv.bin.strat.class<-function(dataset, seed=123456, cv=10){
  set.seed(seed)
  
  cv.folds<-list()

  balance<-table(dataset$group)  
  
  #number of instances in every fold
  n.inst<-ceiling((nrow(dataset))*(1/cv))
  n.inst.c0<-round(n.inst*round(balance[1]/sum(balance), 2), 0) %>% as.numeric()
  
  actual.inst<-(1:nrow(dataset))
  
  for(cv.i in 1:(cv-1)){
    tmp<-which(dataset$group[actual.inst]==(names(balance)[1]))
    c1<-actual.inst[sample(tmp, n.inst.c0)]
    tmp<-which(dataset$group[actual.inst]==(names(balance)[2]))
    c2<-actual.inst[sample(tmp, n.inst-length(c1))]
    cv.folds[[cv.i]]<-sample(c(c1,c2)) #shuffle our cv.values
    actual.inst<-subset(actual.inst, !actual.inst%in%c(c1,c2))
  }
  
  cv.folds[[cv]]<-actual.inst
  
  invisible(cv.folds)
}


