library(randomForest)
library(metricsgraphics)
library(tm)
library(SnowballC)
library(wordcloud)

# Load data

load(paste0(data.path,"dataset.RData"))

# Variables for cluster
cluster.vars=lasso.vars

# Create dataframe with data
db.cluster=dataset[,c("phone","bimester",cluster.vars)]

# Drop zero variance variables
db.cluster=db.cluster[,sapply(db.cluster,var,na.rm=T)!=0]

# Update cluster.vars
cluster.vars=intersect(names(db.cluster),cluster.vars)

# Drop missing obs'
db.cluster=db.cluster[complete.cases(db.cluster[,cluster.vars]),]

# Compute principal component
db.cluster.pc = prcomp(db.cluster[,cluster.vars], center = FALSE, scale. = FALSE)$x %>% as.data.frame()

# Fit a random forest for clusters
rf.fit = randomForest(x = db.cluster[,cluster.vars], y = NULL, ntree = 100, proximity = TRUE, oob.prox = TRUE)
hclust.rf = hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")

# Prune trees for the number of groups
rf.cluster = cutree(hclust.rf, k=3)

# Attach groups to exisiting dataframes
db.cluster.pc$rf.clusters = rf.cluster
db.cluster$rf.clusters=rf.cluster

# Table of groups per bimester
table(rf.cluster, db.cluster$bimester)

# Graph groups by principal component 1 and 2
mjs_plot(db.cluster.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=rf.clusters) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")

## Statistics

# Select variables for table
vars=paste0(c("nchar","nword","long","ans.yes"),"_mean")

# Number of groups
ngroup=3

# Create table
table=data.frame(var=as.character(rep(vars,ngroup)),group=rep(1:ngroup,each=length(vars)), estat=NA)
table$var=as.character(table$var)

for(i in 1:nrow(table)){
  
 table[i,]$estat=mean(db.cluster[db.cluster$rf.clusters==table[i,]$group,table[i,]$var])
  
}

# Merge with sms data
db.sms=merge(db.sms,db.cluster[,c("phone","bimester","rf.clusters")],by=c("phone","bimester"))

## Word Cloud

corpus = Corpus(VectorSource(paste0(db.sms[db.sms$rf.clusters==3,]$answer,collapse=" ")))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('portuguese'))

corpus = tm_map(corpus, stemDocument, 'portuguese')

wordcloud(corpus, max.words = 25, random.order = FALSE)



