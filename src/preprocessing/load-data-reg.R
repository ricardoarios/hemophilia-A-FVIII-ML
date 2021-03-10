#######################################################
# Code implemented to load the analyzed dataset 
#	as well as the required libraries
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
# GNU General Public License v3.0
# Permissions of this strong copyleft license are 
#	conditioned on making available complete 
#	source code of licensed works and 
#	modifications, which include larger works 
#	using a licensed work, under the same license. 
#	Copyright and license notices must be 
#	preserved. Contributors provide an express 
#	grant of patent rights.
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




