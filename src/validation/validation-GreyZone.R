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
# Code implemented to validate the classification by
#	using gray zone approach
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

#' This method validates the classification results
#' 
#' @param dataset - a data matrix including the classification probabilities
#' @return labels based on classification probabilities
check_results = function(resultsDataset){
  
  acc_severe = length(which(resultsDataset$severity == "Severe" & resultsDataset$prob.Severe > 0.5)) / length(which(resultsDataset$prob.Severe>0.5))*100
  
  acc_others = length(which(resultsDataset$severity != "Severe" & resultsDataset$prob.Severe < 0.5)) / length(which(resultsDataset$prob.Severe<0.5))*100
  
  tmpResult = data.frame(acc.Severe = acc_severe, acc.Others = acc_others, acc.Total = mean(c(acc_severe, acc_others)))
  
  return(tmpResult)
}
