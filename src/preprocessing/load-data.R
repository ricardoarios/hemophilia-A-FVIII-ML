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
# Code implemented to load the analyzed dataset 
#	as well as the required libraries
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

# loading main packages
library(xgboost)
library(mlr)
library(parallelMap)
library(parallel)
library(MASS)
library(tidyr)
library(dplyr)
library(smotefamily)
library(corrplot)

# loading all required functions
source("src/preprocessing/createBinaryClass.R")
source("src/preprocessing/cvStratClass.R")
#source("R/createIndexes.R")

# loading attribute just removing the first 9 ones
# load("dataset/severity-FVIII-Activity-Regression.Rdata")
# relevant attributes considered in our experiments
# selecting the relevant attributes
# modeling.att<-which(! 1:ncol(dataHemophilia) %in% 1:9 )
#rm(dataHemophilia)

# loading the binary dataset
modeling.att<-10:25
dataHemophilia<-load.full.dataset(dataset = "dataset/severity-FVIII-Activity-Regression.Rdata", useful.att = modeling.att)
dataHemophilia$group <- dataHemophilia$group %>% 
  cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("Severe","Others")) %>% droplevels()


# remove possible duplicated instances
dataHemophilia<-dataHemophilia[-c(dataHemophilia %>% duplicated() %>% which()), ]

# create a k fold to be used in our cross validation approach
cv.10<-cv.bin.strat.class(dataset=dataHemophilia, seed=123456, cv=10)
###


