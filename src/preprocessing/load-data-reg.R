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

#setwd("/home/riosfsa/hemophilia/")

library(xgboost)
library(mlr)
library(parallelMap)
library(parallel)
library(MASS)
library(tidyr)

source("src/preprocessing/createBinaryClass.R")
source("src/preprocessing/cvStratReg.R")

modeling.att<-10:25

load("data/severity-FVIII-Activity-Regression-v4.Rdata")
modeling.att<-which(! 1:ncol(dataHemophilia) %in% 1:9 )
rm(dataHemophilia)
dataHemophilia<-load.full.dataset(dataset = "data/severity-FVIII-Activity-Regression-v4.Rdata", useful.att = modeling.att)

#scaling our data
scale.index<-which(!colnames(dataHemophilia) %in% c("degree", "kcore", "group"))
dataHemophilia[,scale.index] <-dataHemophilia[,scale.index] %>% scale()

# using the most relevant features (specialist and wrapper)
dataHemophilia<-dataHemophilia %>% select(AA_dist, areaSAS, areaSES, consurfDB, degree, burts, pr, group)

###
severe.limit=1
cv.10<-cv.bin.strat.reg(dataset=dataHemophilia, seed=123456, next.int=severe.limit, label.index=ncol(dataHemophilia), cv=10)
###




