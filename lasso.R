library(glmnet)
library(ggplot2)
library(plyr)
library(lfe)

# No scientific notation
options(scipen=999)

# Source personal configurations
source('config.R')

# Outcomes

outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Transform outcomes (z-score)

if(T){
  for(i in outcomes) {
    dataset[,i] = as.numeric(scale(dataset[,i]))
  }
}

# Load data

load(paste0(data.path,"dataset.RData"))
load(paste0(data.path,"dbsms.RData"))

# Drop missing
dataset.lasso=dataset[complete.cases(dataset[,c(chosen.outcome,lasso.vars)]),]

# Drop cancel
# dataset.lasso=dataset.lasso[dataset.lasso$ans.cancel==0,]

# Drop never interacted
#dataset.lasso=dataset.lasso[rowSums(dataset.lasso[,lasso.vars]==0)<length(lasso.vars),]

# Keep variables without residualization for prediction
x.old=as.matrix(dataset.lasso[,lasso.vars])
y.old=dataset.lasso[,chosen.outcome]

# Residualize data

resid.fe=function(z){
  reg=felm(z ~ 0 | fix.effect)
  resid=reg$resid
  return(resid) 
}

fix.effect=dataset.lasso$phone

if(T){
  y=resid.fe(dataset.lasso[,chosen.outcome])
} else {
  y=dataset.lasso[,chosen.outcome]
}

if(T){
  x=as.matrix(apply(dataset.lasso[,lasso.vars], 2, resid.fe))
} else {
  x=x.old
}

# Estimation

# Lasso with cross-validation

lambda.grid=exp(seq(-12,0,length.out=100))

lasso=glmnet(x,y,lambda = lambda.grid)
set.seed(1234)
cvlasso=cv.glmnet(x,y,nfolds=10)

# Save plot
png(filename=paste0(fig.path,"cvlasso.png"))
plot(cvlasso)
dev.off()

# Get proxy

lambda.str=cvlasso$lambda.min
feat.select=rownames(as.matrix(coef(cvlasso,lambda.str)))[as.matrix(coef(cvlasso,lambda.str))!=0]

dataset.lasso[,paste0(chosen.outcome,"_fit")]=as.numeric(predict(cvlasso,newx=x.old,s=lambda.str,type="response"))
#dataset.lasso=rbind.fill(dataset.lasso,dataset[dataset$eduq_feed==9,])

dataset=merge(dataset,dataset.lasso[,c("phone","bimester",paste0(chosen.outcome,"_fit"))],by=c("phone","bimester"),all.x=T)
dataset[,paste0(chosen.outcome,"_fit")]=ifelse(dataset$eduq_feed==9 & !is.na(dataset[,paste0(chosen.outcome)]),coef(cvlasso,"lambda.min")[1,],dataset[,paste0(chosen.outcome,"_fit")])
dataset=dataset[!is.na(dataset[,chosen.outcome]),]

# Save datasets for trees

save(cvlasso,lasso.vars,file=paste0(data.path,"cvlasso.RData"))
save(dataset.lasso,dataset,feat.select,file=paste0(data.path,"data_tree.RData"))
