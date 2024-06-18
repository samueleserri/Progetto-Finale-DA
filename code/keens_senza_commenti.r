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

# i dati sono stati normalizzati dopo aver sommato 1, 
# questo perché nel dataset sono presenti numerosi valori uguali a zero per la precisione degli strumenti di misurazione
# inoltre le somme per riga è circa 10000 in quanto i dati sono di natura compositiva

# magari stampare il numero di zeri o la media e varianza delle somme per riga




# Hist e boxplot di tutte le variabili


# aggiungere i summary
hist(oliveoil$palmitic, prob = T)
hist(oliveoil$palmitoleic, prob = T)
hist(oliveoil$stearic, prob = T)
hist(oliveoil$oleic, prob = T)
hist(oliveoil$linoleic, prob = T)
hist(oliveoil$linolenic, prob = T)
hist(oliveoil$arachidic, prob = T)
hist(oliveoil$eicosenoic, prob = T)

# bivariata
ggplot(oliveoil, aes(x = as.factor(region), y = palmitic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = palmitoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = stearic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = oleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = linoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = linolenic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = arachidic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(region), y = eicosenoic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")

ggplot(oliveoil, aes(x = as.factor(macro.area), y = palmitic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = palmitoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = stearic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = oleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = linoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = linolenic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = arachidic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveoil, aes(x = as.factor(macro.area), y = eicosenoic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")




pairs(oliveoil[,3:10]) # togliere forse


ggcorrplot(cor(oliveoil[,3:10]), type = "lower", lab = TRUE)

# notiamio che le variabili con correlazione maggiore sono
# palmitic palmitoleic
# oleic palmitic
# oleic palmitoleic
# linoleic palmitoleic
# linoleic oleic
# arachidic linolenic
# eicosenoic palmitic
# eicosenoic linolenic




# iniziamo l'analisi con l'algoritmo k-means per dividere in cluster il dataset


# si utilizza il metodo dell'albow per sciegliere il miglior numero di cluster

withins <- c(1:9)
for (i in 1:9){
  km.out <- kmeans(oliveoil[, 3:10], centers = i, nstart = 15)
  withins[i] <- km.out$tot.withinss
}
plot(1:9, withins)

# dal grafico si nota che il numeor migliore di cluster è 3 o 4


# cluster con k-means

km.out <- kmeans(oliveoil[, 3:10], centers=4, nstart = 15)



# si è deciso qundi di scegliere le variabili più correlate per il plot dei cluster, in sostituzione a pairs

par(mfrow=c(3,3))

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
# da descriver uno per uno
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = palmitic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = palmitoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = stearic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = oleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = linoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = linolenic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = arachidic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(km.out$cluster), y = eicosenoic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)


par(mfrow=c(1,2))

# macro aree per cluster
barplot(prop.table(table(oliveoil$macro.area, km.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle macro aree") # somma macro.aree fa 1
barplot(prop.table(table(oliveoil$macro.area, km.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster")

# cluster 3 è composto solamente da aree del sud
# il cluster 2 comprende la maggior parte degli oli dal centro nord
# il cluster 1 comprendesolamente oli dal sud e dalla sardegna
# aanche se in proporzione i 3 oli sono vicini, si vede dal primo grafico che il cluster 4 comprende la gran pare deglio oli provenienti dalla sardegna

# gli oli del sud sono per il 40% nel cluster 1, il 10% nel cluster 2, il 20% nel 3 e il 30% nel 4
# quelli della sardegna sono il 30% nel gluster 1 ,e il 70% nle 4
# mentre quelli del centro nord sono per l'80% nel 2 ,e il 20% nel 4

# solo gli oli del sud sono presenti in tutti e 4 i cluster, quindi hanno caratteristiche più diversivficate tra loro





# regione per cluster
barplot(prop.table(table(oliveoil$region, km.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle regioni") #
barplot(prop.table(table(oliveoil$region, km.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster") #

# meglio questi
barplot(prop.table(table(km.out$cluster, oliveoil$region),1), beside = T, legend = T, main = "proporzione relativa alle regioni") # 
barplot(prop.table(table(km.out$cluster, oliveoil$region),2), beside = T, legend = T, main = "proporzione relativa ai cluster") #

par(mfrow=c(1,1))







