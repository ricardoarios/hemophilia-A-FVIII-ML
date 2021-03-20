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
# Code implemented to load the dataset and
#	and run basic preprocessing
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


