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
# This code will generate the plots from Figure 1 and Supplementary Figure 2 of the paper.
#


clean_dataset = function(dataset){
  mutPos = unique(dataset$Protein_Change)
  
  # First, remove instances that have more than one severity reported
  # for mutations at a single position.
  
  toRemove = vector()
  
  for(i in 1:length(mutPos)){
    if(length(unique(dataset$Calculated_Severity[which(dataset$Protein_Change == mutPos[i])])) > 1){
      toRemove = c(toRemove, which(dataset$Protein_Change == mutPos[i]))
    }
  }
  
  if(length(toRemove) > 0){
    dataset = dataset[-toRemove,]
  }

  ## Finally, leave one copy of each instance
  ## In other words, remove duplicated entries of instances,
  ## otherwise it would inflate the significance of the results.
  
  selectedCols = match(c("AA_Legacy", "Calculated_Severity"), colnames(dataset))
  
  dataset = dataset[!duplicated(dataset[,selectedCols]),]

  return(dataset)
}



######################################################################################
##
##
## Compare three classes, using one-way ANOVA, followed by Tukey's test
##
#######################################################################################

compare_protein_measures_and_severity = function(inputSet){
  
  # AA_dist
  dataset = inputSet
  a = TukeyHSD(aov(dataset$AA_dist~dataset$Calculated_Severity))
  print(a)
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$AA_dist[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$AA_dist[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$AA_dist[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="AA_dist")
  
  stripchart(dataset$AA_dist~dataset$Calculated_Severity, 
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19,
             vertical=T, method="jitter", add=T)
  
  
  ####################################
  ### Prepares the dataset for analysis
  ####################################
  
  dataset = clean_dataset(inputSet)
  
  print(paste("Mild:", length(which(dataset$Calculated_Severity == "Mild"))))
  print(paste("Moderate:", length(which(dataset$Calculated_Severity == "Moderate"))))
  print(paste("Severe:", length(which(dataset$Calculated_Severity == "Severe"))))
  
  # PSI
  a = TukeyHSD(aov(dataset$psi~dataset$Calculated_Severity))
  print(a)
  b = aov(dataset$psi~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$psi[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$psi[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$psi[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="PSI")
  
  stripchart(dataset$psi~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  # phi
  
  a = TukeyHSD(aov(dataset$phi~dataset$Calculated_Severity))
  print(a)
  b = aov(dataset$phi~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$phi[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$phi[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$phi[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="PHI")
  stripchart(dataset$phi~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # bfactor
  
  
  
  
  a = TukeyHSD(aov(dataset$bfactor~dataset$Calculated_Severity))
  print(a)

  b = aov(dataset$bfactor~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$bfactor[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$bfactor[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$bfactor[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="bFactor")
  
  stripchart(dataset$bfactor~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  # areaSAS
  
  
  
  
  a = TukeyHSD(aov(dataset$areaSAS~dataset$Calculated_Severity))
  print(a)

  b = aov(dataset$areaSAS~dataset$Calculated_Severity)
  print(summary(b)[[1]][["Pr(>F)"]][1])
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$areaSAS[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$areaSAS[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$areaSAS[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Area SAS")
  
  stripchart(dataset$areaSAS~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # areaSES
  
  
  
  
  a = TukeyHSD(aov(dataset$areaSES~dataset$Calculated_Severity))
  print(a)

  b = aov(dataset$areaSES~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$areaSES[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$areaSES[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$areaSES[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Area SES")
  
  stripchart(dataset$areaSES~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # kdHydrophobicity
  
  
  
  
  a = TukeyHSD(aov(dataset$kdHydrophobicity~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$kdHydrophobicity~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$kdHydrophobicity[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$kdHydrophobicity[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$kdHydrophobicity[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="kdHydrophobicity")
  
  stripchart(dataset$kdHydrophobicity~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # ConsurfDB
  
  
  
  
  a = TukeyHSD(aov(dataset$ConsurfDB~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$ConsurfDB~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$ConsurfDB[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$ConsurfDB[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$ConsurfDB[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="ConsurfDB Score")
  
  stripchart(dataset$ConsurfDB~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  # degree
  
  a = TukeyHSD(aov(dataset$degree~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$degree~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$degree[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$degree[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$degree[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Degree")
  
  stripchart(dataset$degree~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  # Betweenness
  
  
  
  
  a = TukeyHSD(aov(dataset$betweenness~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$betweenness~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$betweenness[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$betweenness[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$betweenness[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Betweenness")
  
  stripchart(dataset$betweenness~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # closeness
  
  
  
  
  a = TukeyHSD(aov(dataset$closeness~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$closeness~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$closeness[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$closeness[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$closeness[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Closeness")
  
  stripchart(dataset$closeness~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  
  # burts
  
  
  
  
  a = TukeyHSD(aov(dataset$burts~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$burts~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$burts[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$burts[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$burts[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Burt's contraint")
  
  
  stripchart(dataset$burts~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # pr
  
  a = TukeyHSD(aov(dataset$pr~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$pr~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$pr[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$pr[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$pr[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Page Rank like")
  
  stripchart(dataset$pr~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  # auth
  a = TukeyHSD(aov(dataset$auth~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$auth~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$auth[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$auth[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$auth[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="Authority Score")
  
  stripchart(dataset$auth~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  
  # kcore
  a = TukeyHSD(aov(dataset$kcore~dataset$Calculated_Severity))
  print(a)
  
  b = aov(dataset$kcore~dataset$Calculated_Severity)
  print(paste("ANOVA P-value:", summary(b)[[1]][["Pr(>F)"]][1]))
  
  myTitle = paste(format(round(a[[1]][1,4], 4), nsmall = 2), 
                  format(round(a[[1]][2,4], 4), nsmall = 2), 
                  format(round(a[[1]][3,4], 4), nsmall = 2))
  
  boxplot(na.omit(dataset$kcore[which(dataset$Calculated_Severity == "Mild")]), 
          na.omit(dataset$kcore[which(dataset$Calculated_Severity == "Moderate")]), 
          na.omit(dataset$kcore[which(dataset$Calculated_Severity == "Severe")]), 
          col="white", names=c("Mild", "Moderate", "Severe"), 
          las=1, main=paste("P-value:", myTitle), ylab="KCore")
  
  stripchart(dataset$kcore~dataset$Calculated_Severity,
             col=c("palegreen", " mediumpurple1", "goldenrod1"), pch=19, 
             vertical=T, method="jitter", add=T)
  
  
  return(dataset)
  
}  


### Use the code below to trigger the execution

mydata = read.table("../datasets/training_set_v3.csv", header=T, sep="\t", quote="", stringsAsFactors = F)

svg(filename = "Figure_2_boxplots.svg", width = 5, height = 20)

par(mfrow=c(8,2))

result = compare_protein_measures_and_severity(mydata)

dev.off()









