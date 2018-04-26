library(readstata13)
library(readxl)
library(dplyr)

# No scientif notation
options(scipen=999)

source('config.R')

### Read data

# Read outcomes data
dataset1=read.dta13(paste0(data.path,"diff_ago8.dta"))

# Read characteristics data
dataset2=read.dta13(paste0(data.path,"sms_escola_mar21.dta"))

# Rename boeltim to boletim
names(dataset1)=gsub("bolteim","boletim",names(dataset1))
names(dataset2)=gsub("bolteim","boletim",names(dataset2))

# Select variables
select.vars=c("menina","idade","parda","preta","mae","idade_resp","parda_resp","preta_resp","educ_baixa","educ_EF","educ_EM","renda_1SM","renda_1a3SM")

dataset2=dataset2[,c("ra",select.vars)]

# Read sms data
db.sms=read_excel(paste0(data.path,"interacoes_smsescola.xlsx"))

# Change "Resposta extra" to preceeding question
db.sms$extra_ans=ifelse(db.sms$question=="Resposta extra",1,0)
while(length(which(db.sms$question=="Resposta extra"))>0){
db.sms[which(db.sms$question=="Resposta extra"),]$question=db.sms[which(db.sms$question=="Resposta extra")-1,]$question
}

## Drop unnecessary data

dataset1=dataset1[,!grepl("t_eduq",names(dataset1))]
for(i in 1:4){dataset1[,paste0("boletim_lp",i)]=ifelse(dataset1$t!=i,NA,dataset1[,paste0("boletim_lp",i)])}

dataset1$boletim_lp=rowSums(dataset1[,paste0("boletim_lp",1:4)],na.rm=T)
dataset1=dataset1[,!names(dataset1)%in%paste0("boletim_lp",1:4)]
dataset1=dataset1[,!names(dataset1)%in%paste0("eduq_puro",1:3)]
dataset1=dataset1[,!names(dataset1)%in%c("s","boletim_mat_z","boletim_lp_z","t2")]

## Find phone-ra correspondence through another database

phone.ra=read.dta13(paste0(data.path,"/base_alunos_completa_dadossociodem_matched copy.dta"))
phone.ra=phone.ra %>% group_by(ra) %>% summarise(phone=unique(celular))
phone.ra$phone=as.numeric(substring(phone.ra$phone,2,nchar(phone.ra$phone)))

# Merge phone.ra and dataset1
dataset1=merge(dataset1,phone.ra,by="ra",all.x=T)

# Merge dataset2 and dataset1
dataset=merge(dataset1,dataset2,by="ra",all.x=T)

# Drop obs

# eduq_freq - Tratamento Engajamento - Frequencia (frequency)
# eduq_time - Tratamento Engajamento - Hora do dia (time)
# eduq_feed - Tratamento Engajamento - Feedback (interactivity)

dataset=dataset[dataset$t%in%c(3,4),]
dataset.temp=dataset
dataset=dataset[dataset$eduq_feed==1,]

### Create sms variables

ncol.oldvars=ncol(db.sms)

# Number of characters
db.sms$nchar=nchar(db.sms$answer)
db.sms[is.na(db.sms$nchar),]$nchar=0

# Punctuation
#db.sms$punct=grepl("[[:punct:]]",db.sms$answer)

# Lower case
db.sms$lower=ifelse(!grepl("[[:lower:]]",db.sms$answer)==T,1,0)

# Presence of "SIM"
db.sms$ans.yes=ifelse(grepl("*sim*",db.sms$answer,ignore.case = T)==T,1,0)

# Keep created variables
created.vars=names(db.sms)[(ncol.oldvars+1):ncol(db.sms)]

### Collapse sms dataset

# Change names
names(db.sms)[names(db.sms)=="semana"]="week"
names(dataset)[names(dataset)=="t"]="bimester"

# Create bimester variable
db.sms$bimester=ifelse(db.sms$week%in%1:9,3,NA)
db.sms$bimester=ifelse(db.sms$week%in%10:18,4,db.sms$bimester)

# Collapse
dataset.sms=db.sms %>% group_by(phone,bimester) %>% summarise_at(c(created.vars),funs(mean,max,sum,min))
dataset.sms.week=db.sms %>% group_by(phone,bimester) %>% summarise_at("week",funs(mean,min,max))
names(dataset.sms.week)[-c(1,2)]=paste0("week_",names(dataset.sms.week)[-c(1,2)])

dataset.sms=left_join(dataset.sms,dataset.sms.week)

# Merge dataset1 and db.sms

dataset=merge(dataset,dataset.sms,by=c("phone","bimester"),all.x=T)
#dataset[]

# Lasso variables

lasso.vars=grep(paste0(c(created.vars,"week"),collapse="|"),names(dataset),value=T)

dataset = dataset %>% mutate_at(lasso.vars,funs(replace(., which(is.na(.)), 0)))

save(dataset,lasso.vars,file=paste0(data.path,"dataset.RData"))

# Data analysis

db.sms$cod_question=as.factor(as.numeric(as.factor(db.sms$question)))
db.sms$cod_question=factor(db.sms$cod_question,levels=names(summary(db.sms$cod_question)))
db.sms.mfq=db.sms[db.sms$cod_question%in%names(summary(db.sms$cod_question)[2:15]),]

ggplot(data=db.sms.mfq) + 
  stat_summary_bin(aes(x=substring(question,0,30),y=nchar),fun.y='length', geom='bar') +
  stat_summary_bin(aes(x=substring(question,0,30),y=nchar),fun.y='mean', geom='point') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataset1.clean=dataset1[with(dataset1,paste0(eduq_freq,eduq_time,eduq_time_altern,eduq_feed)!="0000"),]

ggplot(data=dataset1.clean) +
  geom_freqpoly(aes(boletim_mat, ..density..,colour=as.factor(paste0(eduq_freq,eduq_time,eduq_time_altern,eduq_feed)=="9999")),bins=11) +
  scale_x_continuous(breaks=0:10) +
  theme(legend.position="none")




