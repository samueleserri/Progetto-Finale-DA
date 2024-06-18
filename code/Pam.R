


# trovare il miglior numero di cluster per il metodo pam


# DISTANZA MANHATTAN
larghezza_media_m <- c(1:9)
for (i in 2:10){
  pam.out<-pam(oliveoil[,3:10], i, metric="manhattan", stand=TRUE, nstart = 10)
  larghezza_media_m[i-1] <- pam.out$silinfo$avg.width
}

# DISTANZA EUCLIDEA
larghezza_media_e <- c(1:9)
for (i in 2:10){
  pam.out<-pam(oliveoil[,3:10], i, metric="euclidean", stand=TRUE, nstart = 10)
  larghezza_media_e[i-1] <- pam.out$silinfo$avg.width
}

plot(2:10, larghezza_media_m, col = "red")
points(2:10, larghezza_media_e, col = "blue")

# il numero di cluster migliore sembra essere 5
# la distanza manhattan è nettamente migliore della distanza euclidea a parità di numero di cluster, come si vede dal grafico


pam.out<-pam(oliveoil[,3:10], 5, metric="manhattan", stand=TRUE, nstart = 10)
plot(pam.out, which=2, main="")


par(mfrow=c(3,3))

# palmitic palmitoleic
plot(oliveoil$palmitic, oliveoil$palmitoleic, col = pam.out$cluster, pch = 19)

# oleic palmitic
plot(oliveoil$oleic, oliveoil$palmitic, col = pam.out$cluster, pch = 19)

# oleic palmitoleic
plot(oliveoil$oleic, oliveoil$palmitoleic, col = pam.out$cluster, pch = 19)

# linoleic palmitoleic
plot(oliveoil$linoleic, oliveoil$palmitoleic, col = pam.out$cluster, pch = 19)

# linoleic oleic
plot(oliveoil$palmitoleic, oliveoil$linoleic, col = pam.out$cluster, pch = 19)

# arachidic linolenic
plot(oliveoil$linolenic, oliveoil$arachidic, col = pam.out$cluster, pch = 19)

# eicosenoic palmitic
plot(oliveoil$palmitic, oliveoil$eicosenoic, col = pam.out$cluster, pch = 19)

# eicosenoic linolenic
plot(oliveoil$linolenic, oliveoil$eicosenoic, col = pam.out$cluster, pch = 19)

par(mfrow=c(1,1))




# boxplot
# da descriver uno per uno
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = palmitic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = palmitoleic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = stearic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = oleic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = linoleic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = linolenic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = arachidic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(pam.out$cluster), y = eicosenoic, fill = as.factor(pam.out$cluster))) + geom_boxplot(width=0.7) + guides(fill = FALSE)




par(mfrow=c(1,2))

# macro aree per cluster
barplot(prop.table(table(oliveoil$macro.area, pam.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle macro aree", col = 2:4) # somma macro.aree fa 1
barplot(prop.table(table(oliveoil$macro.area, pam.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:4)


# regione per cluster
barplot(prop.table(table(oliveoil$region, pam.out$cluster),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:6) #
barplot(prop.table(table(oliveoil$region, pam.out$cluster),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:6) #

# meglio questi
barplot(prop.table(table(pam.out$cluster, oliveoil$region),1), beside = T, legend = T, main = "proporzione relativa alle regioni", col = 2:6) # 
barplot(prop.table(table(pam.out$cluster, oliveoil$region),2), beside = T, legend = T, main = "proporzione relativa ai cluster", col = 2:6) #

par(mfrow=c(1,1))



