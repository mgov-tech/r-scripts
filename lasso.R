library(glmnet)
library(ggplot2)
library(plyr)
library(lfe)

# No scientific notation
options(scipen=999)

# Source personal configurations
source('config.R')
fig.path=paste0(main.path,"fig/")

# Outcomes

outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Read data

load(paste0(data.path,"dataset.RData"))
dataset.lasso=dataset[complete.cases(dataset[,c(chosen.outcome,lasso.vars)]),]

# Keep variables without residualization for prediction
x.old=x
y.old=y

# Residualize data

resid.fe=function(x){
  reg=felm(x ~ 0 | fix.effect)
  resid=reg$resid
  return(resid) 
}

fix.effect=dataset.lasso$bimester

if(T){
  x=as.matrix(apply(dataset.lasso[,lasso.vars], 2, resid.fe))
  y=resid.fe(dataset.lasso[,chosen.outcome])
} else {
  x=as.matrix(dataset.lasso[,lasso.vars])
  y=dataset.lasso[,chosen.outcome]
}

# Estimation

# Lasso with cross-validation

lambda.grid=exp(seq(-12,0,length.out=100))

lasso=glmnet(x,y,lambda = lambda.grid)
set.seed(1234)
cvlasso=cv.glmnet(x,y,nfolds=5,lambda = lambda.grid)

# Save plot
#png(filename=paste0(fig.path,"cvlasso.png"))
plot(cvlasso)
#dev.off()

# Get proxy

lambda.str=cvlasso$lambda.min
feat.select=rownames(as.matrix(coef(cvlasso,lambda.str)))[as.matrix(coef(cvlasso,lambda.str))!=0]

dataset.lasso[,paste0(chosen.outcome,"_fit")]=as.numeric(predict(cvlasso,newx=x.old,s=lambda.str,type="response"))
#dataset.lasso=rbind.fill(dataset.lasso,dataset[dataset$eduq_feed==9,])

dataset=merge(dataset,dataset.lasso[,c("phone","bimester",paste0(chosen.outcome,"_fit"))],by=c("phone","bimester"),all.x=T)
dataset[,paste0(chosen.outcome,"_fit")]=ifelse(dataset$eduq_feed==9 & !is.na(dataset[,paste0(chosen.outcome)]),coef(cvlasso,"lambda.min")[1,],dataset[,paste0(chosen.outcome,"_fit")])
dataset=dataset[!is.na(dataset[,chosen.outcome]),]

# Save datasets for trees

save(dataset.lasso,dataset,file=paste0(data.path,"data_tree.RData"))





