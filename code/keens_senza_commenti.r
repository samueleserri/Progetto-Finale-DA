library(cluster)
library(clustMixType)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(KernSmooth)
library(lattice)
library(MASS)
library(pdfCluster)
library(stats)
library(maps)
library(dbscan)



# IMPORTARE DATASET


library(pdfCluster)
data("oliveoil")


oliveoil[,3:10] <- oliveoil[,3:10]+1

for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}



# Hist e boxplot di tutte le variabili

hist(oliveoil$palmitic, prob = T)
hist(oliveoil$palmitoleic, prob = T)
hist(oliveoil$stearic, prob = T)
hist(oliveoil$oleic, prob = T)
hist(oliveoil$linoleic, prob = T)
hist(oliveoil$linolenic, prob = T)
hist(oliveoil$arachidic, prob = T)
hist(oliveoil$eicosenoic, prob = T)

ggplot(oliveoil, aes(x = as.factor(region), y = palmitic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = palmitoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = stearic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = oleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = linoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = linolenic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = arachidic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = eicosenoic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")


pairs(oliveoil[,3:10])
ggcorrplot(cor(oliveoil[,3:10]), type = "lower", lab = TRUE)








#elbow

withins <- c(1:9)
for (i in 1:9){
  km.out <- kmeans(oliveoil[, 3:10], centers = i, nstart = 15)
  withins[i] <- km.out$tot.withinss
}
plot(1:9, withins)




# cluster con k-means


km.out <- kmeans(oliveoil[, 3:10], centers=4, nstart = 15)



true_cl <- oliveoil$macro.area
adj.rand.index(true_cl, km.out$cluster)




par(mfrow=c(2,2))

# palmitic palmitoleic
plot(oliveoil$palmitic, oliveoil$palmitoleic, col = km.out$cluster)

# oleic palmitic
plot(oliveoil$oleic, oliveoil$palmitic, col = km.out$cluster)

# oleic palmitoleic
plot(oliveoil$oleic, oliveoil$palmitoleic, col = km.out$cluster)

# linoleic palmitoleic
plot(oliveoil$linoleic, oliveoil$palmitoleic, col = km.out$cluster)

# linoleic oleic
plot(oliveoil$palmitoleic, oliveoil$linoleic, col = km.out$cluster)

# arachidic linolenic
plot(oliveoil$linolenic, oliveoil$arachidic, col = km.out$cluster)

# eicosenoic palmitic
plot(oliveoil$palmitic, oliveoil$eicosenoic, col = km.out$cluster)

# eicosenoic linolenic
plot(oliveoil$linolenic, oliveoil$eicosenoic, col = km.out$cluster)

par(mfrow=c(1,1))




# boxplot

ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = palmitic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = palmitoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = stearic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = oleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = linoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = linolenic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = arachidic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = eicosenoic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)



# macro aree per cluster
barplot(prop.table(table(oliveoil$macro.area, km.out$cluster),2), beside = T, legend = T) 
barplot(prop.table(table(oliveoil$macro.area, km.out$cluster),1), beside = T, legend = T) 


# regione per cluster
barplot(prop.table(table(oliveoil$region, km.out$cluster),2), beside = T, legend = T) #
barplot(prop.table(table(oliveoil$region, km.out$cluster),1), beside = T, legend = T) #

barplot(prop.table(table(km.out$cluster, oliveoil$region),1), beside = T, legend = T) # 
barplot(prop.table(table(km.out$cluster, oliveoil$region),2), beside = T, legend = T) #








