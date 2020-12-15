#######################################################
# Code implemented to validate the classification by
#	using gray zone approach
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

#' This method validate the classification results
#' 
#' @param dataset - a data matrix including the classification probabilities
#' @return labels based on classification probabilities
check_results = function(resultsDataset){
  
  acc_severe = length(which(resultsDataset$severity == "Severe" & resultsDataset$prob.Severe > 0.5)) / length(which(resultsDataset$prob.Severe>0.5))*100
  
  acc_others = length(which(resultsDataset$severity != "Severe" & resultsDataset$prob.Severe < 0.5)) / length(which(resultsDataset$prob.Severe<0.5))*100
  
  tmpResult = data.frame(acc.Severe = acc_severe, acc.Others = acc_others, acc.Total = mean(c(acc_severe, acc_others)))
  
  return(tmpResult)
}
