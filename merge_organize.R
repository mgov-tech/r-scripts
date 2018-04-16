library(readstata13)
library(readxl)
library(xlsx)
library(dplyr)

# Main paths
main.path="/Users/guicoelhonetto/Documents/GitHub/r-scripts/"
data.path=paste0(main.path,"data/")

# Read data
dataset1=read.dta13(paste0(data.path,"diff_ago8.dta"))

dataset2=read.dta13(paste0(data.path,"sms_escola_mar21.dta"))
select.vars=c("menina","idade","parda","preta","bolteim_mat1","mae","idade_resp","parda_resp","preta_resp","educ_baixa","educ_EF","educ_EM","renda_1SM","renda_1a3SM")

dataset2=dataset2[,c("ra",select.vars)]

db.sms=read_excel(paste0(data.path,"interacoes_smsescola.xlsx"))

## Find phone-ra correspondence through another database

phone.ra=read.dta13(paste0(data.path,"/base_alunos_completa_dadossociodem_matched copy.dta"))
phone.ra=phone.ra %>% group_by(ra) %>% summarise(phone=unique(celular))
phone.ra$phone=as.numeric(substring(phone.ra$phone,2,nchar(phone.ra$phone)))

# Merge phone.ra and dataset1
dataset1=merge(dataset1,phone.ra,by="ra",all.x=T)

# Merge dataset2 and dataset1
dataset1=merge(dataset1,dataset2,by="ra",all.x=T)

# Drop obs

dataset1=dataset1[dataset1$t%in%c(3,4),]

### Create sms variables

# Number of characters
db.sms$nchar=nchar(db.sms$answer)

# Punctuation
#db.sms$punct=grepl("[[:punct:]]",db.sms$answer)

# Lower case
db.sms$punct=ifelse(!grepl("[[:lower:]]",db.sms$answer)==T,1,0)

# Presence of "SIM"
db.sms$ans.yes=ifelse(grepl("sim",db.sms$answer)==T,1,0)








