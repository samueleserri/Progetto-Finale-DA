library(pdfCluster)
data("oliveoil")

oliveoil[,3:10] <- oliveoil[,3:10]+1

for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}


#pdf cluster
set.seed(17)
pdf.out <- pdfCluster(oliveALR[, 3:9])
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
# da descriver uno per uno
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = palmitic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = palmitoleic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = stearic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = linolenic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = arachidic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveALR, aes(x = as.factor(pdf.out@clusters), y = eicosenoic, fill = as.factor(pdf.out@clusters))) + geom_boxplot(width=0.7) + guides(fill = FALSE)




par(mfrow=c(1,2))

# macro aree per cluster

prop.table(table(oliveoil$macro.area, pdf.out@clusters),1)

barplot(prop.table(table(oliveoil$macro.area, pdf.out@clusters),1), beside = T, legend = F, main = "Poporzione all'interno dei cluster", col = 2:4)
legend("topright", legend = rownames(prop.table(table(oliveoil$macro.area, pdf.out@clusters),1)
), fill = 2:4, cex = 0.8, bty = "n")

barplot(prop.table(table(oliveoil$macro.area, pdf.out@clusters),2), beside = T, legend = F, main = "", col = 2:4)
legend("topright", legend = rownames(prop.table(table(oliveoil$macro.area, pdf.out@clusters),1)
), fill = 2:4, cex = 0.8, bty = "n")


prop.table(table(pdf.out@clusters, oliveoil$region),1)

barplot(prop.table(table(pdf.out@clusters, oliveoil$region),1), beside = T, legend = F, main = "Poporzione all'interno dei cluster", col = 2:5, cex.names = 0.70, las=2) 
legend("topright", legend = rownames(prop.table(table(pdf.out@clusters, oliveoil$region),1)
), fill = 2:5, cex = 0.8, bty = "n")

barplot(prop.table(table(pdf.out@clusters, oliveoil$region),2), beside = T, legend = F, main = "", col = 2:5, cex.names = 0.70, las=2)
legend("topright", legend = rownames(prop.table(table(pdf.out@clusters, oliveoil$region),1)
), fill = 2:5, cex = 0.8, bty = "n")



confusion_matrix_regioni <- table(Cluster = pdf.out@clusters, Aree = oliveoil$region)
ggplot(data = as.data.frame(as.table(confusion_matrix_regioni)), aes(x = Cluster, y = Aree, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Aree", y = "Cluster", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
par(mfrow=c(1,1))


