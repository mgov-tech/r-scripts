library(glmnet)
library(ggplot2)

# No scientific notation
options(scipen=999)

# Source personal configurations
source('config.R')
fig.path=paste0(main.path,"fig/")

# Outcome of choice

outcome = c("perc_freq_lp","boletim_mat","perc_freq_mat","boletim_lp")

# Read data

load(paste0(data.path,"dataset.RData"))
dataset.lasso=dataset[complete.cases(dataset[,c(outcome[2],lasso.vars)]),]

# Residualize data

resid.fe=function(x){
  reg=felm(x ~ 0 | bimester)
  resid=reg$resid
  return(resid) 
}

# Lasso

x=as.matrix(dataset.lasso[,lasso.vars])
y=as.matrix(dataset.lasso[,outcome[2]])

# Lasso with cross-validation

lambda.grid=exp(seq(-12,0,length.out=100))

lasso=glmnet(x,y,lambda = lambda.grid)
cvlasso=cv.glmnet(x,y,nfolds=5,lambda = lambda.grid)

# Save plot
png(filename=paste0(fig.path,"cvlasso.png"))
plot(cvlasso)
dev.off()

# Get proxy

lambda.str=cvlasso$lambda.min
feat.select=rownames(as.matrix(coef(cvlasso,lambda.str)))[as.matrix(coef(cvlasso,lambda.str))!=0]

dataset.lasso[,paste0(outcome[2],"_fit")]=predict(cvlasso,newx=x,s=lambda.str,type="response")

# Save datasets for trees

save(dataset.lasso,dataset,file=paste0(data.path,"data_tree.RData"))





