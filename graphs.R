library(readstata13)
library(readxl)
library(dplyr)
library(genderBR)
library(data.table)
library(lfe)
library(glmnet)
library(ggplot2)
library(plyr)
library(gtable)
library(gridExtra)

#### Compliers graph ####

rm(list = ls())

source('config.R')

# Outcomes

outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Load lasso results

load(paste0(data.path,"cvlasso.RData"))

weeks=1:8

DF.stats_f=data.frame()
DF.stats_t=data.frame()

for(i in weeks){
  
  load(paste0(data.path,"dataset.RData"))
  load(paste0(data.path,"dbsms.RData"))
  
  week.sms=db.sms[db.sms$week%in%c(1:i,10:c(i+9)),] %>% group_by(phone,bimester) %>% summarise_at(c(created.vars,"week"),funs(mean))
  
  # Residualize data
  
  resid.fe=function(z){
    fix.effect=week.sms$bimester
    reg=felm(z ~ 0 | fix.effect)
    resid=reg$resid
    return(resid) 
  }
  
  week.sms=week.sms[complete.cases(week.sms[,c(lasso.vars)]),]
  temp=list()
  k=0
  for(j in 3:4){
    k=k+1
    temp[[k]]=week.sms[week.sms$bimester==j,][week.sms[week.sms$bimester==j,]$phone%in%unique(dataset[!is.na(dataset[,chosen.outcome]) & dataset$bimester==j,]$phone),]
  }
  week.sms=rbindlist(temp); setDF(week.sms)
  week.sms=merge(week.sms,dataset[,c("phone","bimester",chosen.outcome)],by=c("phone","bimester"),all=T)
  week.sms = week.sms %>% mutate_at(lasso.vars,funs(replace(., which(is.na(.)), 0)))
  
  if(T){
    x=as.matrix(apply(week.sms[,lasso.vars], 2, resid.fe))
  } else {
    x=as.matrix(week.sms[,lasso.vars])
  }
  
  week.sms[,paste0(chosen.outcome,"_fit")]=as.numeric(predict(cvlasso,newx=x,s=cvlasso$lambda.min,type="response"))
  week.sms=week.sms[complete.cases(week.sms[,c(chosen.outcome)]),]
  week.sms[,paste0(chosen.outcome,"_tru")]=resid.fe(week.sms[,c(chosen.outcome)])
  
  df.wide=merge(week.sms[week.sms$bimester==3,c("phone",chosen.outcome)],week.sms[week.sms$bimester==4,c("phone",paste0(chosen.outcome,"_fit"))],by="phone",all.x=T)
  df.wide=merge(df.wide,week.sms[week.sms$bimester==4,c("phone",paste0(chosen.outcome,"_tru"))],by="phone",all.x=T)
  
  names(df.wide)=c("phone","y0","dy","dy_t")
  
  df.wide=df.wide[complete.cases(df.wide$dy),]
  
  if(i == min(weeks)){ thold.dy=median(df.wide$dy,na.rm=T) }
  if(i == min(weeks)){ thold.dy_t=median(df.wide$dy_t,na.rm=T) }
  thold.y0=median(df.wide$y0,na.rm=T)
  
  df.wide$persona=ifelse(df.wide$y0<=thold.y0 & df.wide$dy<=thold.dy,1,NA)
  df.wide$persona=ifelse(df.wide$y0<=thold.y0 & df.wide$dy> thold.dy,2,df.wide$persona)
  df.wide$persona=ifelse(df.wide$y0> thold.y0 & df.wide$dy<=thold.dy,3,df.wide$persona)
  df.wide$persona=ifelse(df.wide$y0> thold.y0 & df.wide$dy> thold.dy,4,df.wide$persona)
  df.wide$persona=factor(df.wide$persona,levels=1:4)
  
  df.wide$persona_t=ifelse(df.wide$y0<=thold.y0 & df.wide$dy_t<=thold.dy_t,1,NA)
  df.wide$persona_t=ifelse(df.wide$y0<=thold.y0 & df.wide$dy_t> thold.dy_t,2,df.wide$persona_t)
  df.wide$persona_t=ifelse(df.wide$y0> thold.y0 & df.wide$dy_t<=thold.dy_t,3,df.wide$persona_t)
  df.wide$persona_t=ifelse(df.wide$y0> thold.y0 & df.wide$dy_t> thold.dy_t,4,df.wide$persona_t)
  df.wide$persona_t=factor(df.wide$persona_t,levels=1:4)
  
  df.stats=df.wide%>%group_by(persona)%>%dplyr::summarise(p=length(persona)/nrow(df.wide), c=length(persona))
  df.stats_t=df.wide%>%group_by(persona_t)%>%dplyr::summarise(p=length(persona_t)/nrow(df.wide), c=length(persona_t))
  
  df.stats.cross=df.wide%>%dplyr::count(persona,persona_t)
  df.stats.cross=merge(df.stats.cross,df.wide%>%dplyr::count(persona),by="persona",suffixes = c("","_e"))
  df.stats.cross$p=df.stats.cross$n/df.stats.cross$n_e
  
  ggplot(df.wide,aes(x=dy,y=y0)) +
    geom_jitter(width = 0, height = 0) +
    geom_hline(yintercept = thold.y0, linetype="dotted") +
    geom_vline(xintercept = thold.dy, linetype="dotted") +
    ylab("Nota inicial") +
    xlab("Mudança de nota prevista") +
    ggtitle(paste0("Semana ",i)) +
    #scale_x_continuous(breaks=seq(-0.5,0.5,0.25), limits=c(-0.6,0.6)) +
    theme_minimal()
  ggsave(paste0(fig.path,"compliers_graph_week",i,".png"))
  
  ggplot(df.stats, aes(x=persona,y=p)) +
    geom_bar(stat="identity") +
    #xlab("Personas") +
    ylab("Proporção") +
    ggtitle(paste0("Proporção pro Persona - Semana ",i)) +
    #scale_y_continuous(breaks=seq(0,0.6,0.1), limits=c(0,0.6)) +
    scale_x_discrete("Personas", labels = c("1" = "Preguiçoso","2" = "Quase Lá", "3" = "Pró-Ativo","4" = "Role Model")) +
    theme_minimal()
  ggsave(paste0(fig.path,"prop_persona_week",i,".png"))
  
  ggplot(df.wide,aes(x=dy_t,y=y0)) +
    geom_jitter(width = 0, height = 0) +
    geom_hline(yintercept = thold.y0, linetype="dotted") +
    geom_vline(xintercept = thold.dy_t, linetype="dotted") +
    ylab("Nota inicial") +
    xlab("Mudança de nota prevista") +
    ggtitle(paste0("Semana ",i," - Dados Reais")) +
    #scale_x_continuous(breaks=seq(-0.5,0.5,0.25), limits=c(-0.6,0.6)) +
    theme_minimal()
  ggsave(paste0(fig.path,"compliers_graph_week",i,"_tru.png"))
  
  ggplot(df.stats_t, aes(x=persona_t,y=p)) +
    geom_bar(stat="identity") +
    #xlab("Personas") +
    ylab("Proporção") +
    ggtitle(paste0("Proporção pro Persona - Semana ",i)) +
    scale_y_continuous(breaks=seq(0,0.6,0.1), limits=c(0,0.6)) +
    scale_x_discrete("Personas", labels = c("1" = "Preguiçoso","2" = "Quase Lá", "3" = "Pró-Ativo","4" = "Role Model")) +
    theme_minimal()
  ggsave(paste0(fig.path,"prop_persona_week",i,"_tru.png"))
  
  ggplot(df.stats.cross, aes(x=persona, y=persona_t)) +
    geom_tile(aes(fill = p), color = "white") +
    scale_fill_gradient(low = "red", high = "steelblue") +
    ylab("Persona Verdadeiro") +
    xlab("Persona Estimado") +
    ggtitle(paste0("Mapa de Calor - Semana ",i)) +
    scale_y_discrete(labels = c("1" = "Preguiçoso","2" = "Quase Lá", "3" = "Pró-Ativo","4" = "Role Model")) +
    scale_x_discrete(labels = c("1" = "Preguiçoso","2" = "Quase Lá", "3" = "Pró-Ativo","4" = "Role Model")) +
    theme_minimal()
  ggsave(paste0(fig.path,"heatmap_week",i,".png"))
  
  DF.stats_f=rbind(DF.stats_f,cbind(df.stats,week=i))
  DF.stats_t=rbind(DF.stats_t,cbind(df.stats_t,week=i))
  
}

names(DF.stats_t)[names(DF.stats_t)=="persona_t"]="persona"
DF.stats=rbind(cbind(DF.stats_f,f="Estimado"),cbind(DF.stats_t,f="Verdadeiro"))

ggplot(DF.stats, aes(x=week, y=p, fill=persona)) + 
  geom_area() +
  ylab("Proporção") +
  xlab("Semana") +
  scale_x_continuous(breaks=min(weeks):max(weeks), limits=c(min(weeks),max(weeks))) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1)) +
  scale_fill_discrete(name="Personas",
                      breaks=c("1","2","3","4"),
                      labels=c("Preguiçoso","Quase Lá","Pró-Ativo","Role Model")) +
  theme_minimal() + facet_grid(.~f)
ggsave(paste0(fig.path,"evolution_personas.png"))


