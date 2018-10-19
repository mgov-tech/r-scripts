library(devtools)
library(grf)
library(stringr)

# Source personal configurations
source('config.R')

# Load data

load(paste0(data.path,"data_tree.RData"))

# Outcomes
outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Choose variations of treatments
treat=paste0("v",1:24)
treat.fit=treat[sapply(1:24,function(x) sum(dataset[!dataset$eduq_feed%in%c(2,9),paste0("v",x)]==1,na.rm=T))>0]

#

char.vars=c("parda","parda_resp","preta","preta_resp","mae","renda_1SM","renda_1a3SM","educ_EM","educ_baixa","menina","idade_resp")

X.all=dataset[,char.vars]

main_effect=NA
main_effect_fit=NA

for(k in c("","_fit")) {
  
  for(i in treat){
    
    nona.subset=!is.na(dataset[,paste0(chosen.outcome,k)]) & !is.na(dataset[,i]) & rowSums(sapply(char.vars,function(x) is.na(dataset[,x])))==0
    
    X=dataset[nona.subset,char.vars]
    Y=dataset[nona.subset,paste0(chosen.outcome,k)]
    W=dataset[nona.subset,i]
    
    tau.forest = causal_forest(X, Y, W, min.node.size=10, seed = 1234)
    
    if(k!="_fit"){
      main_effect[i]=average_treatment_effect(tau.forest, target.sample = "all")[1]
      
    } else {
      main_effect_fit[i]=average_treatment_effect(tau.forest, target.sample = "all")[1]
      
    }
    
    dataset[,paste0(i,"_main_effect",k)] = average_treatment_effect(tau.forest, target.sample = "all")[1]
    
    dataset[,paste0(i,"_group_effect",k)] = predict(tau.forest, X.all)
    
  }
  
}

main_effect=main_effect[!is.na(main_effect)]
main_effect_fit=main_effect_fit[!is.na(main_effect_fit)]

save(dataset,main_effect,main_effect_fit,feat.select,file=paste0(data.path,"data_final_cf.RData"))
