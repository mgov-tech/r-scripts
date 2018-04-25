library(glmnet)
library(ggplot2)

# No scientific notation
options(scipen=999)

source('config.R')
fig.path=paste0(main.path,"fig/")

# Read data
outcome = 'boletim_mat'

load(paste0(data.path,"dataset.RData"))
dataset.lasso=dataset[complete.cases(dataset[,c(outcome,lasso.vars)]),]

# Residualize data

resid.fe=function(x){
  reg=felm(x ~ 0 | cod_turma)
  resid=reg$resid
  return(resid) 
}

# Lasso

x=as.matrix(dataset.lasso[,lasso.vars])
y=as.matrix(dataset.lasso[,outcome])

# Lasso with cross-validation

lambda.grid=exp(seq(-12,0,length.out=100))
cvlasso=cv.glmnet(x,y,nfolds=5,lambda = lambda.grid)

# Save plot
png(filename=paste0(fig.path,"cvlasso.png"))
plot(cvlasso)
dev.off()

ggplot(data=db.sms[db.sms$question %in% unique(db.sms$question)[1:10],]) + 
  stat_summary_bin(aes(x=substring(question,0,30),y=nchar),fun.y='mean', geom='point') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
