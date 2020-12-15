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

# loading main packages
library(xgboost)
library(mlr)
library(parallelMap)
library(parallel)
library(MASS)
library(tidyr)
library(dplyr)

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


