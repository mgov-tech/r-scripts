library(devtools)
library(causalTree)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Source personal configurations
source('config.R')

# Load data

load(paste0(data.path,"data_final.RData"))

# Outcomes
outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Characteristics vector
chars=c("parda","parda_resp","preta","preta_resp","mae","renda_1SM","renda_1a3SM","educ_EM","educ_baixa","menina","idade_resp")

# Choose variations of treatments
treat=paste0("v",1:24)
treat.fit=treat[sapply(1:24,function(x) sum(dataset[!dataset$eduq_feed%in%c(2,9),paste0("v",x)]==1,na.rm=T))>0]

# Create variables

dataset$R_oracle=NA
dataset$R_oracle_group=NA
dataset$R_oracle_all=NA
dataset$R_oracle_group_all=NA
dataset$R_proxy=NA

for(i in treat){
  dataset$R_oracle_all =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect")],dataset$R_oracle_all)
  dataset$R_oracle_group_all =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_group_effect")],dataset$R_oracle_group_all)
}

for(i in treat.fit){
  dataset$R_oracle =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect")],dataset$R_oracle)
  dataset$R_oracle_group =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_group_effect")],dataset$R_oracle_group)
  dataset$R_proxy=ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect_fit")],dataset$R_proxy)
}

dataset$R_oracle_max=apply(dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),invert=T,fixed=T,value=T)],1,max)
dataset$R_proxy_max=apply(dataset[,grep("group_effect_fit",names(dataset),fixed=T,value=T)],1,max,na.rm=T)

dataset$R_oracle_max_all=apply(dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),invert=T,fixed=T,value=T)],1,max)

# Compute statistics

Delta_oracle=mean(dataset$R_oracle_max-dataset$R_oracle_group,na.rm=T)
Delta_proxy=mean(dataset$R_proxy_max-dataset$R_proxy_group,na.rm=T)

proxy.quality=Delta_proxy/Delta_oracle

# Predicted impact

ols=felm(as.formula(paste0(chosen.outcome,"~",paste0(feat.select[-1],collapse="+"))),data=dataset)

dataset$postlasso=NA
dataset[dataset$eduq_feed!=9, ]$postlasso=ols$fitted.values

predicted_impact=mean(dataset$postlasso,na.rm=T)-mean(dataset[dataset$treat_cod==25,chosen.outcome])

# Re-assingment probabilities

prob=exp(main_effect_fit)/sum(exp(main_effect_fit))

### Simulation

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

weeks=8

DELTA_AI <- foreach(i=1:1000, .combine=rbind) %dopar% {
  
  rea=matrix(sample(names(prob),size=nrow(dataset)*weeks,replace=T,prob=prob),nrow(dataset),weeks)
  
  R_simul_avg=sapply(1:nrow(dataset), function(x) mean(as.numeric(dataset[x,paste0(rea[x,],"_group_effect")]),na.rm=T))
  
  delta=mean(R_simul_avg-dataset$R_oracle,na.rm=T)
  
  delta
  
}
stopCluster(cl)
save(DELTA_AI,file=paste0(data.path,"delta_ai.R"))

Delta_ai=mean(DELTA_AI)

quality=Delta_ai/Delta_oracle
print(quality)

# Graphs

ggplot(data=as.data.frame(DELTA_AI),aes(V1)) + 
  geom_histogram() +
  xlab("Delta_AI") +
  ggtitle("Distribution of Delta_AI (N = 1000)") +
  theme_minimal()
ggsave(paste0(fig.path,"delta_ai_dist.png"))

table = dataset %>% 
  group_by(treat_cod) %>% dplyr::summarise(p=sum(R_oracle_group_all==R_oracle_max_all,na.rm = T)/length(R_oracle_max_all))

ggplot(data=table, aes(x=as.factor(treat_cod),y=p)) +
  geom_col() +
  xlab("Variation") +
  ylab("Proportion") +
  theme_minimal()
ggsave(paste0(fig.path,"v_max.png"))








