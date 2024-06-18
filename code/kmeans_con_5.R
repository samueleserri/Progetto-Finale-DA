

km.out2 <- kmeans(oliveoil[, 3:10], centers=5, nstart = 15)

par(mfrow=c(1,2))

# macro aree per cluster
barplot(prop.table(table(oliveoil$macro.area, km.out2$cluster),1), beside = T, legend = T, main = "proporzione relativa alle macro aree", col = 2:4) # somma macro.aree fa 1
barplot(prop.table(table(oliveoil$macro.area, km.out2$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:4)


# regione per cluster
barplot(prop.table(table(oliveoil$region, km.out2$cluster),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:6) #
barplot(prop.table(table(oliveoil$region, km.out2$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:6) #

# meglio questi
barplot(prop.table(table(km.out2$cluster, oliveoil$region),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:6) # 
barplot(prop.table(table(km.out2$cluster, oliveoil$region),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:6) #

par(mfrow=c(1,1))
