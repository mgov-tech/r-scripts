library(randomForest)
library(metricsgraphics)
library(tm)
library(SnowballC)
library(wordcloud)

cluster.vars=created.vars

db.sms.scale=scale(db.sms[,cluster.vars])

db.sms.scale.pc = prcomp(db.sms.scale, center = FALSE, scale. = FALSE)$x %>% as.data.frame()

rf.fit = randomForest(x = db.sms.scale, y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf = hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
db.sms.scale.pc$rf.clusters = rf.cluster
table(rf.cluster, db.sms$bimester)

mjs_plot(db.sms.scale.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=rf.clusters) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")



# Word Cloud

corpus = Corpus(VectorSource(paste0(db.sms[db.sms.scale.pc$rf.clusters==1,]$answer,collapse=" ")))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('portuguese'))

corpus = tm_map(corpus, stemDocument)

wordcloud(corpus, max.words = 5, random.order = FALSE)