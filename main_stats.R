library(devtools)
library(causalTree)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lfe)

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
dataset$R_proxy_group=NA

for(i in treat){
  dataset$R_oracle_all =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect")],dataset$R_oracle_all)
  dataset$R_oracle_group_all =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_group_effect")],dataset$R_oracle_group_all)
}

for(i in treat.fit){
  dataset$R_oracle =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect")],dataset$R_oracle)
  dataset$R_oracle_group =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_group_effect")],dataset$R_oracle_group)
  dataset$R_proxy=ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_main_effect_fit")],dataset$R_proxy)
  dataset$R_proxy_group =ifelse(dataset[,i]==1 & !is.na(dataset[,i]),dataset[,paste0(i,"_group_effect_fit")],dataset$R_proxy_group)
}

dataset$R_oracle_max=apply(dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),invert=T,fixed=T,value=T)],1,max)
dataset$R_proxy_max=apply(dataset[,grep("group_effect_fit",names(dataset),fixed=T,value=T)],1,max,na.rm=T)
dataset=dataset[dataset$R_proxy_max!=-Inf,]

dataset$R_oracle_max_all=apply(dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),invert=T,fixed=T,value=T)],1,max)

x=dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),fixed=T,value=T,invert=T)]
x=x[,substring(names(x), 0, nchar(names(x))-13)%in%treat.fit]

dataset$v_max=substring(names(x)[(apply(x,1,max,na.rm=T)==x)%*%(1:length(names(x)))], 0, nchar(names(x)[(apply(x,1,max,na.rm=T)==x)%*%(1:length(names(x)))])-13)

x2=dataset[,grep("_fit",grep("group_effect",names(dataset),fixed=T,value=T),fixed=T,value=T)]
x2=x2[,substring(names(x2), 0, nchar(names(x2))-17)%in%treat.fit]

dataset$v_max_proxy=substring(names(x2)[(apply(x2,1,max,na.rm=T)==x2)%*%(1:length(names(x2)))], 0, nchar(names(x2)[(apply(x2,1,max,na.rm=T)==x2)%*%(1:length(names(x2)))])-17)


# Compute statistics

Delta_oracle_ign=mean(dataset$R_oracle_max-dataset$R_oracle_group,na.rm=T)
Delta_oracle_ols=mean(dataset$R_oracle_max-max(dataset$R_oracle,na.rm=T),na.rm=T)
Delta_proxy=mean(dataset$R_proxy_max-dataset$R_proxy_group,na.rm=T)

proxy.quality=Delta_proxy/Delta_oracle_ols

# Predicted impact

# ols=felm(as.formula(paste0(chosen.outcome,"~",paste0(feat.select[-c(1)],collapse="+"))),data=dataset)
# 
# dataset$postlasso=NA
# dataset[dataset$eduq_feed!=9, ]$postlasso=ols$fitted.values
# 
# predicted_impact=mean(dataset$postlasso,na.rm=T)-mean(dataset[dataset$treat_cod==25,chosen.outcome])

# Re-assingment probabilities

#prob[which.min]

prob=exp(main_effect_fit)/sum(exp(main_effect_fit))

### Simulation

# cores=detectCores()
# cl <- makeCluster(cores[1]-1)
# registerDoParallel(cl)
# 
# weeks=8
# 
# M=1000
# 
# DELTA_AI <- foreach(i=1:M, .combine=rbind) %dopar% {
# 
#   rea=matrix(sample(names(prob),size=nrow(dataset)*weeks,replace=T,prob=prob),nrow(dataset),weeks)
# 
#   R_simul_avg=sapply(1:nrow(dataset), function(x) mean(as.numeric(dataset[x,paste0(rea[x,],"_group_effect")]),na.rm=T))
# 
#   delta_ign=mean(R_simul_avg-dataset$R_oracle_group,na.rm=T)
#   delta_ols=mean(R_simul_avg-max(dataset$R_oracle,na.rm=T),na.rm=T)
# 
#   delta=c(delta_ign,delta_ols)
# 
# }
# stopCluster(cl)
# save(DELTA_AI,file=paste0(data.path,"delta_ai.RData"))

load(paste0(data.path,"delta_ai.RData"))
Delta_ai_ign=mean(DELTA_AI[,1])
Delta_ai_ols=mean(DELTA_AI[,2])
delta_ai=data.frame(mean=c(rep(Delta_ai_ign,length(DELTA_AI[,1])),rep(Delta_ai_ols,length(DELTA_AI[,2]))),est=c(DELTA_AI[,1],DELTA_AI[,2]),model=c(rep("Ignorance",length(DELTA_AI[,1])),rep("OLS",length(DELTA_AI[,2]))))

quality=Delta_ai_ign/Delta_oracle_ign

delta_ai_text=delta_ai %>% group_by(model) %>% summarise(mean=unique(mean))

# Graphs

ggplot(data=delta_ai,aes(est)) + 
  geom_histogram() +
  geom_vline(aes(xintercept=mean), color="firebrick4", linetype = "longdash") +
  geom_text(data = delta_ai_text,aes(x=mean, label=paste0(" Mean = ",round(mean,3)), y=-5, hjust="left", family="Times"), color="firebrick4", angle=0, size=4) +
  xlab("Delta_AI") +
  ylab(paste0("Frequence (per ",M,")")) +
  ggtitle(paste0("Distribution of Delta_AI (Number of simulations = ",M,")")) +
  facet_grid(.~model, scales = "free") +
  theme_minimal()
ggsave(paste0(fig.path,"delta_ai_dist.png"))

table = dataset[dataset$treat_cod!=25,] %>% 
  group_by(treat_cod) %>% dplyr::summarise(p=sum(R_oracle_group_all>=R_oracle_max_all,na.rm = T)/length(R_oracle_max_all),freq=unique(eduq_freq),time=unique(eduq_time),time_altern=unique(eduq_time_altern),feed=unique(eduq_feed))

ggplot(data=table, aes(x=as.factor(treat_cod),y=p, fill=as.factor(freq), color= as.factor(time), linetype= as.factor(time_altern), alpha=as.factor(feed))) +
  geom_col() +
  xlab("Variation") +
  ylab("Proportion") +
  scale_fill_manual(breaks=c(1,2,3), label=c("1 - Small", "2 - Medium", "3 - Large"), name="# SMS", values=c("brown","royalblue","yellow2"), aesthetics = "fill") +
  scale_colour_manual(breaks=c(1,2), label=c("Yes", "No"), name="Working Hours", values=c("palegreen","black"), guide = guide_legend(override.aes=aes(fill=NA))) +
  scale_alpha_manual(values=c(1, 0.3), name = "Feedback", label=c("Yes","No"), breaks=c(1,2)) +
  scale_linetype_manual(name="Alternating", breaks=c(1,2), label=c("Solid - Yes","Dashed - No"), values=c("solid","dashed"), guide = guide_legend(override.aes=aes(fill=NA))) +
  theme_minimal()
ggsave(paste0(fig.path,"v_max.png"))

table = dataset %>% 
  group_by(treat_cod) %>% dplyr::summarise(p=sum(R_oracle_group_all>=R_oracle_all,na.rm = T)/length(R_oracle_max_all),freq=unique(eduq_freq),time=unique(eduq_time),time_altern=unique(eduq_time_altern),feed=unique(eduq_feed))

ggplot(data=table, aes(x=as.factor(treat_cod),y=p, fill=as.factor(freq), color= as.factor(time), linetype= as.factor(time_altern), alpha=as.factor(feed))) +
  geom_col() +
  xlab("Variation") +
  ylab("Proportion") +
  scale_fill_manual(breaks=c(1,2,3), label=c("1 - Small", "2 - Medium", "3 - Large"), name="# SMS", values=c("brown","royalblue","yellow2"), aesthetics = "fill") +
  scale_colour_manual(breaks=c(1,2), label=c("Yes", "No"), name="Working Hours", values=c("palegreen","black"), guide = guide_legend(override.aes=aes(fill=NA))) +
  scale_alpha_manual(values=c(1, 0.3), name = "Feedback", label=c("Yes","No"), breaks=c(1,2)) +
  scale_linetype_manual(name="Alternating", breaks=c(1,2), label=c("Solid - Yes","Dashed - No"), values=c("solid","dashed"), guide = guide_legend(override.aes=aes(fill=NA))) +
  theme_minimal()
ggsave(paste0(fig.path,"v_mean.png"))





