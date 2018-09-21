library(readstata13)
library(readxl)
library(dplyr)
library(genderBR)
library(data.table)

# No scientif notation
options(scipen=999)

source('config.R')

# Load data

db.sms=read.csv("/Users/guicoelhonetto/Downloads/zenvia_output - zenvia_output.csv",colClasses = c("character","character","character","numeric"))

### Create sms variables

ncol.oldvars=ncol(db.sms)

# Number of characters
db.sms$nchar=nchar(db.sms$answer)
#db.sms[is.na(db.sms$nchar),]$nchar=0

# Number of words
db.sms$nword=sapply(db.sms$answer,function(str) sum(sapply(attr(gregexpr("[[:alpha:]]+", str)[[1]],"match.length"), function(x) sum(x > 0))))
#db.sms[is.na(db.sms$nword),]$nword=0

# Long Words

db.sms$long=sapply(db.sms$answer,function(str) sum(sapply(attr(gregexpr("[[:alpha:]]+", str)[[1]],"match.length"), function(x) sum(x > 3))))
#db.sms[is.na(db.sms$long),]$long=0

# Presence of SIM/NAO
db.sms$ans.yes=ifelse(grepl("*sim*",db.sms$answer,ignore.case = T)==T,1,0)

# cancelling words
db.sms$ans.cancel=ifelse(grepl("*sair*|*cancelar*",db.sms$answer,ignore.case = T)==T,1,0)

# Keep created variables
created.vars=names(db.sms)[(ncol.oldvars+1):ncol(db.sms)]

save(db.sms,file=paste0(data.path,"dbsms_prof.RData"))

# Load lasso results

load(paste0(data.path,"cvlasso.RData"))

weeks=31:35

DF.stats=data.frame()

for(i in weeks){
  
  load(paste0(data.path,"dbsms_prof.RData"))
  
  week.sms=db.sms[db.sms$week%in%c(min(weeks):i),] %>% group_by(phone) %>% summarise_at(c(created.vars,"week"),funs(mean))
  
  if(i!=max(weeks)){
  week.sms=rbindlist(list(week.sms,data.frame(phone=setdiff(db.sms$phone,week.sms$phone),week=i)),fill=T)
  }
  
  week.sms = week.sms %>% mutate_at(lasso.vars,funs(replace(., which(is.na(.)), 0)))
  
  x=as.matrix(week.sms[,lasso.vars])
  
  week.sms[,paste0(chosen.outcome,"_fit")]=as.numeric(predict(cvlasso,newx=x,s=cvlasso$lambda.min,type="response"))

  df.wide=week.sms[,c("phone",paste0(chosen.outcome,"_fit"))]
  
  names(df.wide)=c("phone","dy")
  
  df.wide=df.wide[complete.cases(df.wide$dy),]
  
  if(i == min(weeks)){ thold.dy=median(df.wide[df.wide!=coef(cvlasso,cvlasso$lambda.min)[1],]$dy,na.rm=T) }
  
  df.wide$persona=ifelse(df.wide$dy<=  thold.dy,1,NA)
  df.wide$persona=ifelse(df.wide$dy>  thold.dy,2,df.wide$persona)
  df.wide$persona=factor(df.wide$persona,levels=1:3)
  
  df.stats=df.wide%>%group_by(persona)%>%dplyr::summarise(p=length(persona)/nrow(df.wide), c=length(persona))
  
  DF.stats=rbind(DF.stats,cbind(df.stats,week=i))
  
}

ggplot(DF.stats, aes(x=week, y=p, fill=persona)) + 
  geom_area() +
  ylab("Proporção") +
  xlab("Semana") +
  scale_x_continuous(breaks=min(weeks):max(weeks), limits=c(min(weeks),max(weeks))) +
  #scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1)) +
  scale_fill_discrete(name="Personas",
                      breaks=c("1","2"),
                      labels=c("Esquerda","Direita")) +
  theme_minimal()
ggsave(paste0(fig.path,"evolution_personas.png"))




