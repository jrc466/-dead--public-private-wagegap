# Code for sampling RAIS - Graphs and Tables 
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

setwd(analysis.dir)

########
## 1. FEs distribution 
########
obj_001 = fread(paste("000_fe",suffix,".csv",sep=""))
obj_001[,group1:=as.character(group1)]
obj_001[group1=="1",group1:="Private Sector"]
obj_001[group1=="0",group1:="Public Administration"]
mean.fe = obj_001[,mean(effect),group1]
graph_000 = ggplot(obj_001,aes(x=effect,fill=group1))+labs(fill="")+geom_segment(aes(x=mean.fe[1,2][[1]],y=0.25,xend=mean.fe[2,2][[1]],yend=1.25),color="red",linetype="dashed")+geom_segment(aes(x=mean.fe[1,2][[1]],y=0.25,xend=mean.fe[1,2][[1]],yend=1.25),color="blue",linetype="dashed")+theme(legend.position=c(0.85,0.85),legend.title=element_blank())+ggtitle("Distribution of Worker FE's")+geom_density(alpha=0.75)+scale_fill_brewer(palette="Set1")+xlab("Fixed Effects (log)")
ggsave(paste("001_fe",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

graph_alt = ggplot(obj_001,aes(x=effect,fill=group1))+labs(fill="")+theme(legend.position=c(0.85,0.85),legend.title=element_blank())+ggtitle("Distribution of Worker FE's")+geom_density(alpha=0.75)+scale_fill_brewer(palette="Set1")+xlab("Fixed Effects (log)")
ggsave(paste("001_fe_alt",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

### For later, add table with descriptive statistics

########
## 2. Oaxaca Blinder 
########

# 2.1 Whole Sample OB 
load(paste("001_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

load(paste("001_net_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

# 2.2 Yearly OB
obj_002 = fread(paste("001_ob_yearly",suffix,".csv",sep=""))
setnames(obj_002,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_002[,year:=as.character(year)]
obj_002 = melt(obj_002,id.vars = c("year","se.comp","se.coef"),measure.vars = c("Characteristics","Structural"))
obj_002[variable=="Characteristics",sd:=se.comp]
obj_002[variable=="Structural",sd:=se.coef]
graph_001 = ggplot(data=obj_002,aes(x=year,y=value,fill=variable))+ylim(-0.1,0.6)+scale_fill_brewer(palette="Paired")+geom_bar(stat="identity",color="black",position=position_dodge())+geom_errorbar(width=.4,position=position_dodge(0.9),aes(ymin=value-1.96*sd,ymax=value+1.96*sd))#+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B")+xlab("") + ylab("Log of hourly wage")

obj_003 = fread(paste("001_net_ob_yearly",suffix,".csv",sep=""))
setnames(obj_003,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_003[,year:=as.character(year)]
obj_003 = melt(obj_003,id.vars = c("year","se.comp","se.coef"),measure.vars = c("Characteristics","Structural"))
obj_003[variable=="Characteristics",sd:=se.comp]
obj_003[variable=="Structural",sd:=se.coef]
graph_002 = ggplot(data=obj_003,aes(x=year,y=value,fill=variable))+ylim(-0.1,0.3)+scale_fill_brewer(palette="Paired")+geom_bar(stat="identity",color="black",position=position_dodge())+geom_errorbar(width=.4,position=position_dodge(0.9),aes(ymin=value-1.96*sd,ymax=value+1.96*sd))#+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B")+xlab("") + ylab("Log of hourly wage")
#graph_002 = ggplot(data=obj_003,aes(x=year,y=value,fill=variable))+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B net of worker FE's")+xlab("") + ylab("Log of hourly wage")

ggarrange(graph_001,graph_002,ncol=1,nrow=2,common.legend=T,legend="bottom")
ggsave(paste("002_yearly_ob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

########
## 3. Quantile Oaxaca Blinder 
########
setwd(analysis.dir)
suffix="_debug" ### just for now

# 3.1 QOB on whole sample
load(paste("002_qob",suffix,".RData",sep=""),verbose=T)
obj_004 = data.table(quantiles = qob00$quantiles, Total = qob00$total_effect, Structural = qob00$structral_effect, Characteristics = qob00$composition_effect)
obj_004 = melt(obj_004,id.vars = "quantiles")
graph_003 = ggplot(obj_004,aes(x=quantiles,y=value,group=variable))+ggtitle("Endogenous Estimates")+geom_line(aes(linetype=variable),size=1.2)+xlab("Quantiles")+ylab("Log points")+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash")) + geom_hline(aes(yintercept = 0))

load(paste("002_net_qob",suffix,".RData",sep=""),verbose=T)
obj_005 = data.table(quantiles = qob00_net$quantiles, Total = qob00_net$total_effect, Structural = qob00_net$structral_effect, Characteristics = qob00_net$composition_effect)
obj_005 = melt(obj_005,id.vars = "quantiles")
graph_004 = ggplot(obj_005,aes(x=quantiles,y=value,group=variable))+ylim(0,0.6)+ggtitle("Net of Worker FE's")+geom_line(aes(linetype=variable),size=1.2)+xlab("Quantiles")+ylab("Log points")+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash")) + geom_hline(aes(yintercept = 0))

ggarrange(graph_003,graph_004,ncol=1,nrow=2,common.legend=T,legend="right")
ggsave(paste("003_qob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# 3.2 QOB by Gender and Skill
qob.gs = data.table(quantiles=numeric(),female=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("002_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,female:="Male"]}else{obj[,female:="Female"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","female","skill"))
    qob.gs = rbind(qob.gs,obj) 
  }
}
qob.gs[,female:=as.factor(female)]
qob.gs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Skill","Medium Skill","High Skill"))]

fig = ggplot(qob.gs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(female~skill)
ggsave(paste("002_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
qob.net.gs = data.table(quantiles=numeric(),female=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("002_net_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,female:="Male"]}else{obj[,female:="Female"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","female","skill"))
    qob.net.gs = rbind(qob.net.gs,obj) 
  }
}
qob.net.gs[,female:=as.factor(female)]
qob.net.gs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Skill","Medium Skill","High Skill"))]

fig = ggplot(qob.gs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(female~skill)
ggsave(paste("002_net_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# 3.3 QOB by Race and Skill
qob.rs = data.table(quantiles=numeric(),nonwhite=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("003_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,nonwhite:="White"]}else{obj[,nonwhite:="Nonwhite"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","nonwhite","skill"))
    qob.rs = rbind(qob.rs,obj) 
  }
}
qob.rs[,nonwhite:=as.factor(nonwhite)]
qob.rs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Skill","Medium Skill","High Skill"))]

fig = ggplot(qob.rs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(nonwhite~skill)
ggsave(paste("003_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
qob.net.rs = data.table(quantiles=numeric(),nonwhite=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("003_net_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,nonwhite:="White"]}else{obj[,nonwhite:="Nonwhite"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","nonwhite","skill"))
    qob.net.rs = rbind(qob.net.rs,obj) 
  }
}
qob.net.rs[,nonwhite:=as.factor(nonwhite)]
qob.net.rs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Skill","Medium Skill","High Skill"))]

fig = ggplot(qob.net.rs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(nonwhite~skill)
ggsave(paste("003_net_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

