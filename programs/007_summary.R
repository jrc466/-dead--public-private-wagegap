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
library(arsenal)

# Reading sample
setwd(output.dir)
input.file = paste("04_toanalysis",suffix,".csv",sep="")
data = fread(input.file,colClasses = "character",na.strings = "")

# Adjusting class of columns
setkey(data,pis,yr)
cols = c("mean_earn","hired_wage","hwage1","age1","tenure")
data[,(cols):=lapply(.SD,function(x) as.numeric(x)),.SDcols=cols]
data = data[mean_earn!=0&hired_wage!=0]
data[,skill:=factor(skill,levels=c("1","2","3"),labels=c("Low","Medium","High"))]
data[,group1:=factor(group1,levels=c("0","1"),labels=c("Public","Private"))]
data[,sex1:=factor(sex1,levels=c("0","1"),labels=c("Male","Female"))]
data[,nonwhite1:=factor(nonwhite1,levels=c("0","1"),labels=c("White","Nonwhite"))]

labels = list(hwage1 = "Hourly wage (2011 US$)",sex1="Gender",skill="Skill",nonwhite1="Race",age1="Age",tenure="Tenure")

#####
# 1. Obtaining Summary Statistics for switchers in FE equation
#####
data[,lag_g1:=shift(group1,n=1,type="lag"),pis]
data[,shifter:=ifelse(group1!=lag_g1,1,0)]
data[!is.na(shifter),shifter_pc:=100*sum(shifter)/.N,.(yr)]
data[,shifter:=factor(shifter,levels=c(0,1),labels=c("Non-shifter","Shifter"))]

table001 = tableby(shifter~age1+tenure+sex1+nonwhite1+skill,data=data[tenure>=0],test=F,numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss"))
summary(table001, labelTranslations = labels,text = T)

#####
# 0. Obtaining Summary Statistics of the data
#####
controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss"),
  cat.stats = c("countpct", "Nmiss"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss = "Missing"
  )
)

table002 = tableby(group1~hwage1+age1+tenure+sex1+nonwhite1+skill,data=data[tenure>=0],test=F,numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss"))
summary(table002, labelTranslations = labels,text = "latex")

save(table,file=paste(graphs.dir,"007_summary_onpc.RData",sep="/"))


