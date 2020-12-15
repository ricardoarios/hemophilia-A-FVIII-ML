#######################################################
# Code implemented to load the dataset and
#	and run basic preprocessing
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

#' This method creates to convert the labels into two  
#'	classes
#' 
#' @param dataset - a data matrix
#' @param positive - the positive class
#' @param negative - name of the other (negative) classes
#' @return a dataset with converted labels
conv2class<-function(dataset, positive="Mild", negative="OT"){
  dataset$group<-as.character(dataset$group)
  others<-which(dataset$group != positive)
  dataset[others, ncol(dataset)]<-negative
  return(dataset)
}

#' This method runs basic preprocessing.  
#'	Example: select attributes, remove instances with invalid values
#' 
#' @param dataset - a data matrix
#' @param useful.att - a predefined set of relevant attributes
#' @return the final dataset
load.full.dataset<-function(dataset, useful.att=NA){
  # loading the dataset
  load(dataset)
  
  full.label<-colnames(dataHemophilia)
  colnames(dataHemophilia)<-c(full.label[-length(full.label)], "group")
  
  if(!(is.na(useful.att) %>% any())){
    dataHemophilia<-dataHemophilia[, useful.att]
  }
  
  # removing lines with some invalid values
  na.rows<-c()
  for(i in 1:ncol(dataHemophilia)){
    na.rows<-c(na.rows, which(is.na(dataHemophilia[,i])))
  }
  
  if(length(na.rows) > 0){
    dataHemophilia<-dataHemophilia[-na.rows, ]
  }
  
  invisible(dataHemophilia)
}


