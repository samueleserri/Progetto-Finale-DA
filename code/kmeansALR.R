# IMPORTARE DATASET
library(pdfCluster)
data("oliveoil")

#USIAMO ALR
oliveoil[,3:10] <- oliveoil[,3:10]+1
for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}
oliveALR <- -log(oliveoil[,-c(1,2,6)]/oliveoil[,6])
oliveALR <- cbind(oliveoil[,1:2], oliveALR)

# ISTOGRAMMI DELLE VARIABILI
hist(oliveALR$palmitic, prob = T)
hist(oliveALR$palmitoleic, prob = T)
hist(oliveALR$stearic, prob = T)
hist(oliveALR$linoleic, prob = T)
hist(oliveALR$linolenic, prob = T)
hist(oliveALR$arachidic, prob = T)
hist(oliveALR$eicosenoic, prob = T)

# ANALISI BIVARIATA DELLE VARIABILI (REGIONI)
ggplot(oliveALR, aes(x = as.factor(region), y = palmitic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = palmitoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = stearic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = linoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = linolenic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = arachidic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(region), y = eicosenoic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")

# ANALISI BIVARIATA DELLE VARIABILI (MACRO AREE)
ggplot(oliveALR, aes(x = as.factor(macro.area), y = palmitic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = palmitoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = stearic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = linoleic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = linolenic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = arachidic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(oliveALR, aes(x = as.factor(macro.area), y = eicosenoic)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")

# PAIRS E CORRPLOT
pairs(oliveALR) # togliere forse
ggcorrplot(cor(oliveALR[,3:9]), type = "lower", lab = TRUE)
# SCEGLIAMO LE COPPIE CON MIGLIORE CORRELAZIONE

# METODO DEL ELBOW
withins <- c(1:9)
for (i in 1:9){
  km.out <- kmeans(oliveALR[,3:9], centers = i, nstart = 15)
  withins[i] <- km.out$tot.withinss
}
par(mfrow=c(1,1))
plot(1:9, withins)
# 4/5 SONO LA QUANTITà DI CENTRI MIGLIORE

# CLUSTER CON K-MEANS
km.out <- kmeans(oliveALR[,3:9], centers=4, nstart = 15)
# si è deciso qundi di scegliere le variabili più correlate per il plot dei cluster, in sostituzione a pairs

par(mfrow=c(2,3))

# palmitic palmitoleic
plot(oliveALR$palmitic, oliveALR$palmitoleic, col = km.out$cluster, pch = 19)

# linoleic palmitoleic
plot(oliveALR$linoleic, oliveALR$palmitic, col = km.out$cluster, pch = 19)

# linoleic palmitoleic
plot(oliveALR$palmitoleic, oliveALR$linoleic, col = km.out$cluster, pch = 19)

# arachidic linolenic
plot(oliveALR$linolenic, oliveALR$arachidic, col = km.out$cluster, pch = 19)

# eicosenoic palmitic
plot(oliveALR$palmitic, oliveALR$eicosenoic, col = km.out$cluster, pch = 19)

# eicosenoic palmitoleic
plot(oliveALR$palmitoleic, oliveALR$eicosenoic, col = km.out$cluster, pch = 19)

par(mfrow=c(1,1))
# BOXPLOT PER CONFRONTO TRA CLUSTER E VARIABILI QUANTITATIVE
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = palmitic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = palmitoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = stearic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = linoleic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = linolenic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = arachidic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(km.out$cluster), y = eicosenoic, fill = as.factor(km.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)

# BORPLOT PER CONFRONTO TRA CLUSTER E VARIABILI QUALITATIVE
par(mfrow=c(1,2))

# MACRO AREE
barplot(prop.table(table(oliveALR$macro.area, km.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle macro aree", col = 2:4) # somma macro.aree fa 1
barplot(prop.table(table(oliveALR$macro.area, km.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:4)

# REGIONI
barplot(prop.table(table(km.out$cluster, oliveALR$region),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:5) # 
barplot(prop.table(table(km.out$cluster, oliveALR$region),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:5) #

par(mfrow=c(1,1))