library(pdfCluster)
data("oliveoil")

oliveoil[,3:10] <- oliveoil[,3:10]+1

for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}


oliveALR <- -log(oliveoil[,-c(1,2,6)]/oliveoil[,6])
oliveALR <- cbind(oliveoil[,1:2], oliveALR)

#pdf cluster
set.seed(17)
pdf.out <- pdfCluster(oliveALR[, 3:9], graphtype = 'pairs', lambda = 0.10)
str(pdf.out)

par(mfrow=c(2,3))

# palmitic palmitoleic
plot(oliveALR$palmitic, oliveALR$palmitoleic, col = pdf.out@clusters, pch = 19)
# linoleic palmitoleic
plot(oliveALR$linoleic, oliveALR$palmitoleic, col = pdf.out@clusters, pch = 19)

# arachidic linolenic
plot(oliveALR$linolenic, oliveALR$arachidic, col = pdf.out@clusters, pch = 19)

# eicosenoic palmitic
plot(oliveALR$palmitic, oliveALR$eicosenoic, col = pdf.out@clusters, pch = 19)

# eicosenoic linolenic
plot(oliveALR$linolenic, oliveALR$eicosenoic, col = pdf.out@clusters, pch = 19)

par(mfrow=c(1,1))




# boxplot

ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = palmitic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = palmitoleic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = stearic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = linolenic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = arachidic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = eicosenoic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)


# macro aree per cluster

prop.table(table(oliveALR$macro.area, pdf.out@clusters),1)

barplot(prop.table(table(oliveALR$macro.area, pdf.out@clusters),1), beside = T, legend = F, main = "Poporzione all'interno dei cluster", col = 2:4)
legend("topright", legend = rownames(prop.table(table(oliveALR$macro.area, pdf.out@clusters),1)
), fill = 2:4, cex = 0.8, bty = "n")

barplot(prop.table(table(oliveALR$macro.area, pdf.out@clusters),2), beside = T, legend = F, main = "", col = 2:4)
legend("topright", legend = rownames(prop.table(table(oliveALR$macro.area, pdf.out@clusters),1)
), fill = 2:4, cex = 0.8, bty = "n")


# confusion matrix macro aree

confusion_matrix <- table(Cluster = oliveALR$macro.area, Aree = pdf.out@clusters)

table( Aree = pdf.out@clusters, Cluster = oliveALR$macro.area)

ggplot(data = as.data.frame(as.table(confusion_matrix)), aes(x = Cluster, y = Aree, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Aree", y = "Cluster", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# regioni per cluster

prop.table(table(pdf.out@clusters, oliveALR$region),1)

barplot(prop.table(table(pdf.out@clusters, oliveALR$region),1), beside = T, legend = F, main = "Poporzione all'interno dei cluster", col = 2:5, cex.names = 0.70, las=2) 
legend("topright", legend = rownames(prop.table(table(pdf.out@clusters, oliveALR$region),1)
), fill = 2:5, cex = 0.8, bty = "n")

barplot(prop.table(table(pdf.out@clusters, oliveALR$region),2), beside = T, legend = F, main = "", col = 2:5, cex.names = 0.70, las=2)
legend("topright", legend = rownames(prop.table(table(pdf.out@clusters, oliveALR$region),1)
), fill = 2:5, cex = 0.8, bty = "n")

# confusion matrix regioni


confusion_matrix_regioni <- table(Cluster = pdf.out@clusters, Aree = oliveALR$region)
ggplot(data = as.data.frame(as.table(confusion_matrix_regioni)), aes(x = Cluster, y = Aree, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Aree", y = "Cluster", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#mappa dei cluster

map("italy", col="white", fill=TRUE, lty=1, lwd=1, border="black")
points(oliveGPS$long, oliveGPS$lat, col=pdf.out@clusters+ 1, pch=19, cex=0.3)

