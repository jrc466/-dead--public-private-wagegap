# Code for sampling RAIS - Descriptive statistics
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(stargazer)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Reading sample
setwd(output.dir)
input.file = "04_toanalysis_onepc.csv"
data = fread(input.file,colClasses = "character",na.strings = "")

#####
# 0. Obtaining Summary Statistics of the data
#####



#####
# 1. Obtaining Summary Statistics for switchers in FE equation
#####





