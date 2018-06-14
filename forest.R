library(randomForest)
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

# Transform outcomes (z-score)

if(T){
  for(i in outcomes) {
    dataset[,i] = as.numeric(scale(dataset[,i]))
  }
}

# Make forest

dataset.rf=dataset[complete.cases(dataset[,c(chosen.outcome,lasso.vars)]),]

# Train

fmla=as.formula(paste0(chosen.outcome,"~",paste0(lasso.vars,collapse="+")))

train=sample(1:nrow(dataset.rf),nrow(dataset.rf)/2)
rf=randomForest(fmla , data = dataset.rf , subset = train)

# Test

oob.err=double(length(lasso.vars))
test.err=double(length(lasso.vars))

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:length(lasso.vars)){
  rf=randomForest(fmla , data = dataset.rf , subset = train, mtry=mtry, ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,dataset.rf[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(dataset.rf[-train,], mean( (boletim_mat - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

