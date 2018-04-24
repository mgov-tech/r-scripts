library(readstata13)
library(readxl)
library(dplyr)

# No scientif notation
options(scipen=999)

# Main paths
main.path="/Users/guicoelhonetto/Documents/GitHub/r-scripts/"
data.path=paste0("/Users/guicoelhonetto/Dropbox/","data/")

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
dataset1=merge(dataset1,dataset2,by="ra",all.x=T)

# Drop obs

# eduq_freq - Tratamento Engajamento - Frequencia (frequency)
# eduq_time - Tratamento Engajamento - Hora do dia (time)
# eduq_feed - Tratamento Engajamento - Feedback (interactivity)

dataset1=dataset1[dataset1$t%in%c(3,4),]
dataset1=dataset1[dataset1$eduq_feed==1,]

### Create sms variables

ncol.oldvars=ncol(db.sms)

# Number of characters
db.sms$nchar=nchar(db.sms$answer)

# Punctuation
#db.sms$punct=grepl("[[:punct:]]",db.sms$answer)

# Lower case
db.sms$lower=ifelse(!grepl("[[:lower:]]",db.sms$answer)==T,1,0)

# Presence of "SIM"
db.sms$ans.yes=ifelse(grepl("sim",db.sms$answer)==T,1,0)

# Keep created variables
created.vars=names(db.sms)[(ncol.oldvars+1):ncol(db.sms)]

### Collapse sms dataset

# Create bimester variable
db.sms$t=ifelse(db.sms$semana%in%1:9,3,NA)
db.sms$t=ifelse(db.sms$semana%in%10:18,3,db.sms$t)

# Change name
names(db.sms)[names(db.sms)=="semana"]="week"

# Collapse
dataset.sms=db.sms %>% group_by(phone,t) %>% summarise_at(c("week",created.vars),funs(mean,max,sum,min))

# Merge dataset1 and db.sms

dataset1=merge(dataset1,dataset.sms,by=c("phone","t"),all.x=T)

# Rename dataset1
dataset=dataset1

lasso.vars=grep(paste0(created.vars,collapse="|"),names(dataset),value=T)

save(dataset,lasso.vars,file=paste0(data.path,"dataset.RData"))


