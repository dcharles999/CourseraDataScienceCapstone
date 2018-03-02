require(data.table)
require(quanteda)
require(parallel)
source("predFunctions.R")

# Remove anything with numbers
biTable <- readRDS("biTable.rds")
biTable <- biTable[-grep("[0-9]+",as.matrix(biTable[,1])),]
saveRDS(biTable, "biTable.rds")
rm(biTable)

triTable <- readRDS("triTable.rds")
triTable <- triTable[-grep("[0-9]+",as.matrix(triTable[,1])),]
saveRDS(triTable, "triTable.rds")
rm(triTable)

quadTable <- readRDS("quadTable.rds")
quadTable <- quadTable[-grep("[0-9]+",as.matrix(quadTable[,1])),]
saveRDS(quadTable, "quadTable.rds")
rm(quadTable)

# object.size(uniTable)[1]/1024^2
# object.size(biTable)[1]/1024^2


# lookupBiTable <-sapply(as.matrix(uniTable[,1]), predFromFreq1, table = biTable)
# saveRDS(lookupBiTable,"BiVector.rds")

# biTable <- readRDS("biTable.rds")
# triTable <- readRDS("triTable.rds")
# quadTable <- readRDS("quadTable.rds")
# smallBiTable <- subset(biTable, count > 1)
# saveRDS(smallBiTable, "WordPredict/smallBiTable.rds")
# rm(smallBiTable)
# rm(biTable)
# x <- subset(quadTable, count > 2)
# x <- subset(x, count > 3)
# saveRDS(x, "WordPredict/smallQuadTable.rds")
# rm(x)
# rm(quadTable)
# smallBiTable <- subset(biTable, count > 1)

# Generate tables based on min count
bi <- readRDS("biTable.rds")
tri <- readRDS("triTable.rds")
quad <- readRDS("quadTable.rds")
bi <- subset(bi, count>4)
tri <- subset(tri, count >4)
quad <- subset(quad, count >4)
sum(object.size(bi)[1], object.size(tri)[1],object.size(quad)[1])/1024^2


sum(object.size(biTable)[1], object.size(biTable)[1],object.size(biTable)[1])/1024^2
object.size(biTable)

saveRDS(subset(readRDS("biTable.rds"),count > 10), "WordPredict/smallBiTable.rds")
saveRDS(subset(readRDS("triTable.rds"),count > 10), "WordPredict/smallTriTable.rds")
saveRDS(subset(readRDS("quadTable.rds"),count > 10), "WordPredict/smallQuadTable.rds")

# x <- head(uniTable)
# x1<- as.character(as.matrix(x[,1]))
# sapply(as.matrix(x[,1]), predFromFreq1, table = biTable)
# x
# cbind(ngram=paste(x[,1],sapply(as.matrix(x[,1]), predFromFreq1, table = biTable),collapse = " "),x[,2])
# 

# test <- head(bi,40)
# test[, c("input", "prediction") := tstrsplit(test$ngram, " ", fixed=TRUE)]
# setorder(test, input, -count)


#find the input words
biRef <- data.table(colnames(dfm(unlist(tstrsplit(bi$ngram, " ", fixed=TRUE, keep = 1)))))
test <- head(biRef,100)

t1 <- Sys.time()
test$pred <- sapply(unlist(test$V1),predFromFreq1,table = bi)
t2 <- Sys.time()
t2-t1

# parallel find input words
cores <- max(1, detectCores()-1)
cl <- makeCluster(cores)
clusterExport(cl, list("predFromFreq1"))
t3 <- Sys.time()
test$pred <- parSapply(cl, unlist(test$V1),predFromFreq1,table = bi)
t4 <- Sys.time()
t4-t3
stopCluster(cl)

# Evaluate sizes of data sets
sizes <- data.frame()
for(i in 1:10){
   sizes[i,1] <- object.size(subset(bi, count > i))
   sizes[i,2] <- object.size(subset(tri, count > i))
   sizes[i,3] <- object.size(subset(quad, count > i))
}
colnames(sizes) <- c("bi","tri","quad")
sizes$minFreq <- 1:10
sizes %>%
   gather(ngram, size, -minFreq) %>%
   ggplot(aes(minFreq, size/1024^2)) + 
   geom_point(aes(color = ngram)) + 
   ggtitle("Lookup table size exploration") +
   xlab("Minimum Observations") +
   ylab("size in Mbytes")


rm(quad)
rm(tri)

biRefLarge <- data.table(colnames(dfm(unlist(tstrsplit(bi$ngram, " ", fixed=TRUE, keep = 1)))))
biSmall <- subset(bi, count > 4)
biRefsmall <- data.table(colnames(dfm(unlist(tstrsplit(biSmall$ngram, " ", fixed=TRUE, keep = 1)))))


# parallel find input words - bigram
cores <- max(1, detectCores()-1)
cl <- makeCluster(cores)
clusterExport(cl, list("predFromFreq1"))
clusterCall(cl, function() library(data.table))
t3 <- Sys.time()
biRefsmall$pred <- parSapply(cl, unlist(biRefsmall$V1),predFromFreq1,table = biSmall)
t4 <- Sys.time()
t4-t3
stopCluster(cl)
saveRDS(biRefsmall, "biRefSmall.rds")

# parallel find input words - trigram
triRef <- data.table(colnames(dfm(sapply(strsplit(tri$ngram, " ", fixed=TRUE),function(x){paste(x[1],x[2])}),ngrams=2, concatenator = " ")))

cores <- max(1, detectCores()-1)
cl <- makeCluster(cores)
clusterExport(cl, list("predFromFreq1", "predFromFreq2", "predFromFreq3","predictNext"))
clusterCall(cl, function() library(data.table))
t5 <- Sys.time()
triRef$pred <- parSapply(cl, unlist(triRef$V1),predictNext,biTable = bi, triTable=tri, quadTable = quad)
t6 <- Sys.time()
t6-t5
stopCluster(cl)
saveRDS(triRef, "triRefSmall.rds")

# parallel find input words - quadgram
quadRef <- data.table(colnames(dfm(sapply(strsplit(quad$ngram, " ", fixed=TRUE),function(x){paste(x[1],x[2],x[3])}),ngrams=3, concatenator = " ")))
cores <- max(1, detectCores()-1)
cl <- makeCluster(cores)
clusterExport(cl, list("predFromFreq1", "predFromFreq2", "predFromFreq3","predictNext"))
clusterCall(cl, function() library(data.table))
t7 <- Sys.time()
quadRef$pred <- parSapply(cl, unlist(quadRef$V1),predictNext,biTable = bi, triTable=tri, quadTable = quad)
t8 <- Sys.time()
t8-t7
stopCluster(cl)
saveRDS(quadRef, "quadRefSmall.rds")
