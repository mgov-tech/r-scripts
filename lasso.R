library(glmnet)

# No scientif notation
options(scipen=999)

# Main paths
source('config.R')

data.path=paste0(main.path,"data/")
fig.path=paste0(main.path,"fig/")

# Read data

load(paste0(data.path,"dataset.RData"))
dataset.lasso=dataset[complete.cases(dataset[,c("perc_freq_lp",lasso.vars)]),]

# Residualize data

resid.fe=function(x){
  reg=felm(x ~ 0 | cod_turma)
  resid=reg$resid
  return(resid) 
}

# Lasso

x=as.matrix(dataset.lasso[,lasso.vars])
y=as.matrix(dataset.lasso$perc_freq_lp)

# Lasso with cross-validation

lambda.grid=exp(seq(-12,0,length.out=100))
cvlasso=cv.glmnet(x,y,nfolds=5,lambda = lambda.grid)

# Save plot
png(filename=paste0(fig.path,"cvlasso.png"))
plot(cvlasso)
dev.off()














