library(devtools)
library(causalTree)

# Load data

load()

# Chosen outcomes
outcome = c("boletim_mat","perc_freq_mat","boletim_lp","perc_freq_lp")
outcome.fit = paste0(outcome,"_fit")

i=1
fmla=paste0(outcomes[i]," ~ pp + pp_resp + mae + renda_1SM + educ_baixa + menina + idade_resp")
tree = causalTree(fmla, 
                  data = data, treatment = data$aluno_tratado,
                  split.Rule = "TOT", 
                  cv.option = "fit", 
                  minsize =250, 
                  cv.Honest = T, 
                  split.Bucket = T, 
                  xval = 10, 
                  propensity = 0.8)

opcp = tree$cptable[, 1][which.min(tree$cptable[,4])]
opfit = prune(tree, cp = opcp)

png(filename=paste0("/Users/guicoelhonetto/Documents/Projetos/Reg_Trees/",outcomes[i],".png"),width=480,height=480)
rpart.plot(opfit,           
           type = 1, 
           extra=101, 
           under=T, 
           digits=2, 
           box.palette=0)
dev.off()