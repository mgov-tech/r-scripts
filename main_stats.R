library(devtools)
library(causalTree)
library(stringr)

# Source personal configurations
source('config.R')
fig.path=paste0(main.path,"fig/")

# Load data

load(paste0(data.path,"data_final.RData"))

# Choose variations of treatments
treat=paste0("v",1:24)
treat.fit=treat[sapply(1:24,function(x) sum(dataset[!dataset$eduq_feed%in%c(2,9),paste0("v",x)]==1,na.rm=T))>0]

# Create variables

dataset$R_real=NA
dataset$R_proxy=NA

for(i in treat.fit){
   dataset$R_real=ifelse(dataset[,i]==1,dataset[,paste0(i,"_main_effect")],dataset$R_real)
   dataset$R_proxy=ifelse(dataset[,i]==1,dataset[,paste0(i,"_main_effect_fit")],dataset$R_proxy)
}

dataset$R_real_max=apply(dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),invert=T,fixed=T,value=T)],1,max)
dataset$R_proxy_max=apply(dataset[,grep("group_effect_fit",names(dataset),fixed=T,value=T)],1,max)

# Compute statistics

Delta_real=mean(dataset$R_real_max-dataset$R_real,na.rm=T)
Delta_proxy=mean(dataset$R_proxy_max-dataset$R_proxy,na.rm=T)

quality=Delta_proxy/Delta_real

print(quality)


