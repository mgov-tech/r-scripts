library(devtools)
library(causalTree)
library(stringr)

# Source personal configurations
source('config.R')

# Load data

load(paste0(data.path,"data_tree.RData"))

# Outcomes
outcomes = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")

chosen.outcome=outcomes[1]

# Choose variations of treatments
treat=paste0("v",1:24)
treat.fit=treat[sapply(1:24,function(x) sum(dataset[!dataset$eduq_feed%in%c(2,9),paste0("v",x)]==1,na.rm=T))>0]

opfit=list()

# Functions for finding treatment groups

find.branches=function(opfit){
  
  out = capture.output(opfit)
  
  root.loc=grep("root",out,fixed=T)
  
  all.nodes=out[root.loc:length(out)]
  end.nodes.loc=grep("\\*$",all.nodes)
  
  rank.nodes=nchar(all.nodes)-nchar(str_trim(all.nodes,"left"))
  
  branches=list()
  
  for(i in seq_along(end.nodes.loc)){
    
    node.loc=end.nodes.loc[i]
    branches[[i]]=c(all.nodes[node.loc])
    
    while(grepl("root",branches[[i]][length(branches[[i]])],fixed=T)!=T){
      
      lower.nodes=which(all.nodes[1:node.loc]%in%all.nodes[rank.nodes<rank.nodes[node.loc]])
      argmin.lower.nodes=which.min(abs(node.loc-lower.nodes))
      
      clost.node.loc=lower.nodes[argmin.lower.nodes]
      closest.node=all.nodes[clost.node.loc]
      
      branches[[i]]=str_trim(c(branches[[i]],closest.node),"left")
      
      node.loc=clost.node.loc
      
    }
  }
  return(branches)
}

find.groups=function(branch){
  noroot.branch=branch[-length(branch)]
  split.branch=unlist(strsplit(noroot.branch,split=" "))
  
  root.branch=strsplit(branch[length(branch)],split=" ")
  
  conds=c()
  groups=list()
  
  conds=split.branch[grep(">=",split.branch,fixed=T)]
  
  for(i in seq_along(split.branch)){
    if(grepl("<",split.branch[i],fixed=T)){
      conds=c(conds,paste0(split.branch[i],split.branch[i+1]))
    }
  }
  
  group.effect=split.branch[which(split.branch=="*")-1]
  
  main.effect=as.numeric(root.branch[[1]][length(root.branch[[1]])-1])
  
  groups=list(conds=conds,group.effect=group.effect,main.effect=main.effect)
  return(groups)
}

# Trees

main_effect=NA
main_effect_fit=NA
groups=list()

for(k in c("","_fit")){
  
  fmla=paste0(chosen.outcome,k," ~ parda + parda_resp + preta + preta_resp + mae + renda_1SM + renda_1a3SM + educ_EM + educ_baixa + menina + idade_resp")
  
  for(i in treat){
    
    dt.temp=dataset[!is.na(dataset[,i]),]
    
    tree = causalTree(fmla, 
                      data = dt.temp, treatment = dt.temp[,i],
                      split.Rule = "TOT", 
                      cv.option = "fit", 
                      minsize = 20,  
                      cv.Honest = T, 
                      split.Bucket = T, 
                      xval = 10, 
                      propensity = 0.8)
    
    opcp = tree$cptable[, 1][which.min(tree$cptable[,4])]
    opfit = prune(tree, cp = opcp)
    
    png(filename=paste0(fig.path,i,k,".png"),width=480,height=480)
    rpart.plot(opfit,           
               type = 1, 
               extra=101, 
               under=T, 
               digits=2, 
               box.palette=0)
    dev.off()
    
    branches=find.branches(opfit)
    
    tree.groups=list()
    for(j in seq_along(branches)){
      tree.groups[[j]]=find.groups(branches[[j]])
    }
    groups[[i]]=tree.groups
    
  }
  
  for(i in treat){
    
    dataset[,paste0(i,"_group_effect",k)]=NA
    dataset[,paste0(i,"_main_effect",k)]=NA
    
    if(k!="_fit"){
      main_effect[i]=groups[[i]][[1]]$main.effect
      
    } else {
      main_effect_fit[i]=groups[[i]][[1]]$main.effect
      
    }
    
  }
  
  for(i in seq_along(groups)){
    if(!is.null(groups[[i]][[1]]$conds)){
      for(j in seq_along(groups[[i]])){
        eval(parse(text=paste0("dataset$",names(groups[i]),"_group_effect",k,"=","ifelse(",paste0("dataset$",groups[[i]][[j]]$conds,collapse="&"),",",groups[[i]][[j]]$group.effect,",","dataset$",names(groups[i]),"_group_effect",k,")")))
        eval(parse(text=paste0("dataset$",names(groups)[i],"_main_effect",k,"=",groups[[i]][[j]]$main.effect)))
      }
    } else {
      eval(parse(text=paste0("dataset$",names(groups[i]),"_group_effect",k,"=",groups[[i]][[1]]$main.effect))) 
      eval(parse(text=paste0("dataset$",names(groups)[i],"_main_effect",k,"=",groups[[i]][[1]]$main.effect)))
    }
    
    
    
  }
  
}

main_effect=main_effect[!is.na(main_effect)]
main_effect_fit=main_effect_fit[!is.na(main_effect_fit)]

save(dataset,main_effect,main_effect_fit,groups,feat.select,file=paste0(data.path,"data_final.RData"))



