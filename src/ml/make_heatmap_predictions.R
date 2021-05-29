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
# Authors: Tiago J. S. Lopes, Ricardo Rios, Tatiane Nogueira, Rodrigo F. Mello
#
# This code will generate the plots from Figure 2 of the paper.
# To make the boxplots from Supplementary Figure 3, just load the predictions of the other classifiers.
# They are at the folder dataset/predictions_classifiers/

library(gplots)

# Load and clean the prediction files
nb = read.table("../dataset/predictions_classifiers/naive.bayes.csv", header=T, sep=",", quote="\"", stringsAsFactors = F)
dt = read.table("../dataset/predictions_classifiers/decision.tree.csv", header=T, sep=",", quote="\"", stringsAsFactors = F)

# Remove instances from the training set (443 instances)

training = read.table("../dataset/training_set_v3.csv", header=T, sep="\t", quote="", stringsAsFactors = F)

toRemove = vector()

for(i in 1:nrow(training)){
    pos = which(nb$AA_Legacy == training$AA_Legacy[i] & nb$Protein_Change == training$Protein_Change[i])
    #pos = which(dt$AA_Legacy == training$AA_Legacy[i] & dt$Protein_Change == training$Protein_Change[i])
    
    if(length(pos)>0){
        toRemove = c(toRemove, pos)
    }
}

dt = dt[-toRemove,]
nb = nb[-toRemove,]


# Merge probabilities to create an ensemble
comb_prob = (dt$prob.Severe + nb$prob.Severe)/2

ensemble = data.frame(AA_Legacy = dt$AA_Legacy, prob.Severe = comb_prob, aa1 = dt$aa1, aa2 = dt$aa2)



result = matrix(nrow=length(unique(ensemble$AA_Legacy)), ncol = 20)

rownames(result) = unique(ensemble$AA_Legacy)
colnames(result) = unique(ensemble$aa1)

for(i in 1:nrow(ensemble)){
  rowPos = match(ensemble$AA_Legacy[i], rownames(result))
  colPos = match(ensemble$aa2[i], colnames(result))
  
  result[rowPos, colPos] = ensemble$prob.Severe[i]
}

# Here it is possible to adjust the "size" of the grey-zone
# hence, the colors and the shape of the heatmap will change.
result[which(result >= .39 & result < .63, arr.ind = T)] = .5

# Function derived from the gplots library
heatmap.2(as.matrix(result), trace="none", col=bluered(100), Rowv = F)






