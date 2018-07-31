library(randomForest)
library(metricsgraphics)
library(tm)
library(SnowballC)
library(wordcloud)

load(paste0(data.path,"dataset.RData"))

cluster.vars=lasso.vars

db.scale=dataset[,lasso.vars]

db.scale=db.scale[,sapply(db.scale,var,na.rm=T)!=0]
db.scale=db.scale[complete.cases(db.scale),]

db.scale.pc = prcomp(db.scale, center = FALSE, scale. = FALSE)$x %>% as.data.frame()

rf.fit = randomForest(x = db.scale, y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf = hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
db.scale.pc$rf.clusters = rf.cluster
db.scale$rf.clusters=rf.cluster
table(rf.cluster, db$bimester)

mjs_plot(db.scale.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=rf.clusters) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")

# Word Cloud

# corpus = Corpus(VectorSource(paste0(db[db.scale.pc$rf.clusters==1,]$answer,collapse=" ")))
# 
# corpus = tm_map(corpus, PlainTextDocument)
# 
# corpus = tm_map(corpus, removePunctuation)
# corpus = tm_map(corpus, removeWords, stopwords('portuguese'))
# 
# corpus = tm_map(corpus, stemDocument, 'portuguese')
# 
# wordcloud(corpus, max.words = 25, random.order = FALSE)

# Statistics

vars=paste0(c("nchar","nword","long","ans.yes"),"_mean")
ngroup=3

table=data.frame(var=as.character(rep(vars,ngroup)),group=rep(1:ngroup,each=length(vars)), estat=NA)
table$var=as.character(table$var)

for(i in 1:nrow(table)){
  
 table[i,]$estat=mean(db.scale[db.scale$rf.clusters==table[i,]$group,table[i,]$var])
  
}






