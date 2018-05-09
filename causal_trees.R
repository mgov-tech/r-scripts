library(devtools)
library(causalTree)

# Source personal configurations
source('config.R')
fig.path=paste0(main.path,"fig/")

# Load data

load(paste0(data.path,"data_tree.RData"))

# Chosen outcomes
outcome = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")
outcome.fit = paste0(outcome,"_fit")

# Choose variations of treatments
treat=paste0("v",1:24)
treat.fit=treat[sapply(1:24,function(x) sum(dataset[!dataset$eduq_feed%in%c(2,9),paste0("v",x)]==1,na.rm=T))>0]


opcp.fit=list()
# Formula and outcome

#fmla=paste0(outcome.fit[1]," ~ parda + parda_resp + preta + preta_resp + mae + renda_1SM + renda_1a3SM + educ_EM + educ_baixa + menina + idade_resp")
fmla=paste0(outcome[1],     " ~ parda + parda_resp + preta + preta_resp + mae + renda_1SM + renda_1a3SM + educ_EM + educ_baixa + menina + idade_resp")

# Trees
for(i in treat.fit){
  
  dt.temp=dataset[!is.na(dataset[,i]),]
  
  tree = causalTree(fmla, 
                    data = dt.temp, treatment = dt.temp[,i],
                    split.Rule = "TOT", 
                    cv.option = "fit", 
                    minsize =50, 
                    cv.Honest = T, 
                    split.Bucket = T, 
                    xval = 10, 
                    propensity = 0.8)
  
  opcp = tree$cptable[, 1][which.min(tree$cptable[,4])]
  opcp.fit[[i]] = prune(tree, cp = opcp)
  
  
}

#png(filename=paste0("/Users/guicoelhonetto/Documents/Projetos/Reg_Trees/",outcomes[i],".png"),width=480,height=480)
rpart.plot(opcp.fit[[1]],           
           type = 1, 
           extra=101, 
           under=T, 
           digits=2, 
           box.palette=0)
#dev.off()

