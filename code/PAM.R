
library(pdfCluster)
data("oliveoil")

oliveoil[,3:10] <- oliveoil[,3:10]+1

for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}

oliveALR <- -log(oliveoil[,-c(1,2,6)]/oliveoil[,6])
oliveALR <- cbind(oliveoil[,1:2], oliveALR)

# trovare il miglior numero di cluster per il metodo pam


# DISTANZA MANHATTAN
larghezza_media_m <- c(1:9)
for (i in 2:10){
  pam.out<-pam(oliveALR, i, metric="manhattan", stand=TRUE, nstart = 10)
  larghezza_media_m[i-1] <- pam.out$silinfo$avg.width
}

# DISTANZA EUCLIDEA
larghezza_media_e <- c(1:9)
for (i in 2:10){
  pam.out<-pam(oliveALR, i, metric="euclidean", stand=TRUE, nstart = 10)
  larghezza_media_e[i-1] <- pam.out$silinfo$avg.width
}

plot(2:10, larghezza_media_m, col = "red")
points(2:10, larghezza_media_e, col = "blue")

# il numero di cluster migliore sembra essere 6
# la distanza manhattan è nettamente migliore della distanza euclidea a parità di numero di cluster, come si vede dal grafico


pam.out<-pam(oliveALR, 6, metric="manhattan", stand=TRUE, nstart = 10)
plot(pam.out, which=2, main="")


par(mfrow=c(2,3))

# palmitic palmitoleic
plot(oliveALR$palmitic, oliveALR$palmitoleic, col = pam.out$cluster, pch = 19)
# linoleic palmitoleic
plot(oliveALR$linoleic, oliveALR$palmitoleic, col = pam.out$cluster, pch = 19)

# linoleic oleic
plot(oliveALR$palmitoleic, oliveALR$linoleic, col = pam.out$cluster, pch = 19)

# arachidic linolenic
plot(oliveALR$linolenic, oliveALR$arachidic, col = pam.out$cluster, pch = 19)

# eicosenoic palmitic
plot(oliveALR$palmitic, oliveALR$eicosenoic, col = pam.out$cluster, pch = 19)

# eicosenoic linolenic
plot(oliveALR$linolenic, oliveALR$eicosenoic, col = pam.out$cluster, pch = 19)

par(mfrow=c(1,1))




# boxplot
# da descriver uno per uno
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = palmitic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = palmitoleic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = stearic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = oleic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = linolenic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = arachidic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pam.out$cluster), y = eicosenoic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)




par(mfrow=c(1,2))

# macro aree per cluster
barplot(prop.table(table(oliveALR$macro.area, pam.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle macro aree", col = 2:4) # somma macro.aree fa 1
barplot(prop.table(table(oliveALR$macro.area, pam.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:4)

# meglio questi
barplot(prop.table(table(pam.out$cluster, oliveALR$region),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:7) # 
barplot(prop.table(table(pam.out$cluster, oliveALR$region),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:7) #

par(mfrow=c(1,1))




# da controllare (fatto da copilot)
ggplot(data = oliveALR, aes(x = as.factor(pam.out$cluster), fill = as.factor(pam.out$cluster))) + geom_bar() + facet_wrap(~region) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill = FALSE)
