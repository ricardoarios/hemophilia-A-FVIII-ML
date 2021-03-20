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
# They are at the folder datasets/predictions_classifiers/

upperLimit = 50
lowerLimit = 50

# Load the mutagenesis data
a2 = read.table("../datasets/A2_mutagenesis.csv", header=T, sep="\t", quote="", stringsAsFactors = F)
c2 = read.table("../datasets/C2_mutagenesis.csv", header=T, sep="\t", quote="", stringsAsFactors = F)


# Load and clean the prediction files
nb = read.table("../datasets/predictions_classifiers/naive.bayes.csv", header=T, sep=",", quote="\"", stringsAsFactors = F)
dt = read.table("../datasets/predictions_classifiers/decision.tree.csv", header=T, sep=",", quote="\"", stringsAsFactors = F)

# Remove instances from the training set (443 instances)

training = read.table("../datasets/training_set_v3.csv", header=T, sep="\t", quote="", stringsAsFactors = F)

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

# Leave only the ->Alanine mutations

nb = nb[-which(nb$aa2 != "Ala"),]
dt = dt[-which(dt$aa2 != "Ala"),]

# Match the predictions and the mutagenesis data

############################################################
# A2

# Random Forest
pos_a2 = match(intersect(nb$AA_Legacy, a2$Mature_res_num), a2$Mature_res_num)
pos_net = match(intersect(nb$AA_Legacy, a2$Mature_res_num), nb$AA_Legacy)

# The chromogenic activity
nb$a2_activity = NA
nb$a2_level = NA

nb$a2_activity[pos_net] = a2$Activity[pos_a2]
nb$a2_level[which(nb$a2_activity >= upperLimit)] = "High"
nb$a2_level[which(nb$a2_activity < lowerLimit)] = "Low"

# The antigen activity
nb$a2_antigen = NA
nb$a2_ag_level = NA

nb$a2_antigen[pos_net] = a2$Antigen[pos_a2]
nb$a2_ag_level[which(nb$a2_antigen >= upperLimit)] = "High"
nb$a2_ag_level[which(nb$a2_antigen < lowerLimit)] = "Low"

############################################################
# A2

# dt
pos_a2 = match(intersect(dt$AA_Legacy, a2$Mature_res_num), a2$Mature_res_num)
pos_net = match(intersect(dt$AA_Legacy, a2$Mature_res_num), dt$AA_Legacy)

# The chromogenic activity
dt$a2_activity = NA
dt$a2_level = NA

dt$a2_activity[pos_net] = a2$Activity[pos_a2]
dt$a2_level[which(dt$a2_activity >= upperLimit)] = "High"
dt$a2_level[which(dt$a2_activity < lowerLimit)] = "Low"

# The antigen activity
dt$a2_antigen = NA
dt$a2_ag_level = NA

dt$a2_antigen[pos_net] = a2$Antigen[pos_a2]
dt$a2_ag_level[which(dt$a2_antigen >= upperLimit)] = "High"
dt$a2_ag_level[which(dt$a2_antigen < lowerLimit)] = "Low"



############################################################
# c2

# Random Forest
pos_c2 = match(intersect(nb$AA_Legacy, c2$Mature_res_num), c2$Mature_res_num)
pos_net = match(intersect(nb$AA_Legacy, c2$Mature_res_num), nb$AA_Legacy)

# The chromogenic activity
nb$c2_activity = NA
nb$c2_level = NA

nb$c2_activity[pos_net] = c2$Activity[pos_c2]
nb$c2_level[which(nb$c2_activity >= upperLimit)] = "High"
nb$c2_level[which(nb$c2_activity < lowerLimit)] = "Low"

# The antigen activity
nb$c2_antigen = NA
nb$c2_ag_level = NA

nb$c2_antigen[pos_net] = c2$Antigen[pos_c2]
nb$c2_ag_level[which(nb$c2_antigen >= upperLimit)] = "High"
nb$c2_ag_level[which(nb$c2_antigen < lowerLimit)] = "Low"

############################################################
# c2

# dt
pos_c2 = match(intersect(dt$AA_Legacy, c2$Mature_res_num), c2$Mature_res_num)
pos_net = match(intersect(dt$AA_Legacy, c2$Mature_res_num), dt$AA_Legacy)

# The chromogenic activity
dt$c2_activity = NA
dt$c2_level = NA

dt$c2_activity[pos_net] = c2$Activity[pos_c2]
dt$c2_level[which(dt$c2_activity >= upperLimit)] = "High"
dt$c2_level[which(dt$c2_activity < lowerLimit)] = "Low"

# The antigen activity
dt$c2_antigen = NA
dt$c2_ag_level = NA

dt$c2_antigen[pos_net] = c2$Antigen[pos_c2]
dt$c2_ag_level[which(dt$c2_antigen >= upperLimit)] = "High"
dt$c2_ag_level[which(dt$c2_antigen < lowerLimit)] = "Low"


## Clean up the datasets

nb = nb[-which(is.na(nb$c2_level) &is.na(nb$c2_ag_level) & is.na(nb$a2_level) & is.na(nb$a2_ag_level)),]

dt = dt[-which(is.na(dt$c2_level) &is.na(dt$c2_ag_level) & is.na(dt$a2_level) & is.na(dt$a2_ag_level)),]

# Merge probabilities to create an ensemble
comb_prob = (dt$prob.Severe + nb$prob.Severe)/2

ensemble = data.frame(AA_Legacy = dt$AA_Legacy, prob.Severe = comb_prob, a2_level = dt$a2_level, a2_ag_level = dt$a2_ag_level,
                      c2_level = dt$c2_level, c2_ag_level = dt$c2_ag_level)


## This function will plot the probabilities outputted by the classifiers
## as displayed in Figure 2D

plot_probabilities = function(){
  
  svg("plot_probabilities.svg", height=6, width = 6)
  a2_instances = which(dt$AA_Legacy<700)
  c2_instances = which(dt$AA_Legacy>700)
  
  plot(dt$prob.Severe[a2_instances], nb$prob.Severe[a2_instances], 
       las=1, cex.axis=1.3, cex.lab=1.3, xlab="Decision Tree (Prob. Severe)", ylab="Naive Bayes (Prob. Severe)", 
       ylim=c(0,1), xlim=c(0,1), pch=16, col="blue")
  
  points(dt$prob.Severe[c2_instances], nb$prob.Severe[c2_instances], pch=16, col="red")
  
  grid(lwd=1.2)
  dev.off()

}


## The figure below created the boxplots from Figure 2C of the paper.


make_boxplots = function(){
  
  svg("boxplots_A2_C2_classifiers.svg", width=15, height=5)
  par(mfrow=c(2,6))
  
  # A2 Chromogenic activity
  boxplot(dt$prob.Severe~dt$a2_level, ylab="Probability Severe", 
          xlab="", main="Decision Tree", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(dt$prob.Severe~dt$a2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
  boxplot(nb$prob.Severe~nb$a2_level, ylab="Probability Severe", 
          xlab="", main="Naive Bayes", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(nb$prob.Severe~nb$a2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
  boxplot(ensemble$prob.Severe~ensemble$a2_level, ylab="Probability Severe", 
          xlab="", main="Ensemble", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(ensemble$prob.Severe~ensemble$a2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
   # A2 Antigen activity
  boxplot(dt$prob.Severe~dt$a2_ag_level, ylab="Probability Severe", 
          xlab="", main="Decision Tree", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(dt$prob.Severe~dt$a2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
  boxplot(nb$prob.Severe~nb$a2_ag_level, ylab="Probability Severe", 
          xlab="", main="Naive Bayes", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(nb$prob.Severe~nb$a2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
  boxplot(ensemble$prob.Severe~ensemble$a2_ag_level, ylab="Probability Severe", 
          xlab="", main="Ensemble", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(ensemble$prob.Severe~ensemble$a2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
 
  
  # C2 Chromogenic activity
  boxplot(dt$prob.Severe~dt$c2_level, ylab="Probability Severe", 
          xlab="", main="Decision Tree", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(dt$prob.Severe~dt$c2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
  boxplot(nb$prob.Severe~nb$c2_level, ylab="Probability Severe", 
          xlab="", main="Naive Bayes", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(nb$prob.Severe~nb$c2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
  boxplot(ensemble$prob.Severe~ensemble$c2_level, ylab="Probability Severe", 
          xlab="", main="Ensemble", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(ensemble$prob.Severe~ensemble$c2_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("orange", "darkgreen"))
  
  # C2 Antigen activity
  boxplot(dt$prob.Severe~dt$c2_ag_level, ylab="Probability Severe", 
          xlab="", main="Decision Tree", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(dt$prob.Severe~dt$c2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
  boxplot(nb$prob.Severe~nb$c2_ag_level, ylab="Probability Severe", 
          xlab="", main="Naive Bayes", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(nb$prob.Severe~nb$c2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
  boxplot(ensemble$prob.Severe~ensemble$c2_ag_level, ylab="Probability Severe", 
          xlab="", main="Ensemble", las=1, cex.axis=1.2, cex.lab=1.2, ylim=c(0,1), col="white")
  stripchart(ensemble$prob.Severe~ensemble$c2_ag_level, vertical=T, add=T, method="jitter", pch=20,
             col=c("blue", "lightpink"))
  
  dev.off()
}


print(wilcox.test(dt$prob.Severe~dt$a2_level))
print(wilcox.test(nb$prob.Severe~nb$a2_level))
print(wilcox.test(ensemble$prob.Severe~ensemble$a2_level))

print(wilcox.test(dt$prob.Severe~dt$c2_level))
print(wilcox.test(nb$prob.Severe~nb$c2_level))
print(wilcox.test(ensemble$prob.Severe~ensemble$c2_level))

print(wilcox.test(dt$prob.Severe~dt$a2_ag_level))
print(wilcox.test(nb$prob.Severe~nb$a2_ag_level))
print(wilcox.test(ensemble$prob.Severe~ensemble$a2_ag_level))

print(wilcox.test(dt$prob.Severe~dt$c2_ag_level))
print(wilcox.test(nb$prob.Severe~nb$c2_ag_level))
print(wilcox.test(ensemble$prob.Severe~ensemble$c2_ag_level))





