
library(maps)
library(mapdata)



oliveCOO <- oliveoil






oliveCOO$lat <- NA
oliveCOO$long <- NA

region_coords <- list(
  "Apulia.north" = c(lat = 41.4, long = 15.5),
  "Calabria" = c(lat = 39.0, long = 16.5),
  "Apulia.south" = c(lat = 40.0, long = 18.0),
  "Sicily" = c(lat = 37.6, long = 14.1),
  "Sardinia.inland" = c(lat = 40.0, long = 9.0),
  "Sardinia.coast" = c(lat = 40.0, long = 9.5),
  "Liguria.east" = c(lat = 44.3, long = 9.5),
  "Liguria.west" = c(lat = 44.0, long = 8.0),
  "Umbria" = c(lat = 42.9, long = 12.6)
)

for (i in 1:nrow(oliveCOO)) {
  region <- oliveCOO$region[i]
  if (region %in% names(region_coords)) {
    oliveCOO$lat[i] <- region_coords[[region]]["lat"]
    oliveCOO$long[i] <- region_coords[[region]]["long"]
  }
}



for (i in 1:nrow(oliveCOO)) {
  oliveCOO$lat[i] <- oliveCOO$lat[i] + runif(1, min = -0.4, max = 0.4)
  oliveCOO$long[i] <- oliveCOO$long[i] + runif(1, min = -0.4, max = 0.4)
}



map("italy", col="white", fill=TRUE, lty=1, lwd=1, border="black")
points(oliveCOO$long, oliveCOO$lat, col=pam.out$cluster, pch=19, cex=0.3)
  






















##########
#example 1
###########
# not run here for time reasons
#loading data
data(oliveoil)

#preparing data
olive1 <- 1 + oliveoil[, 3:10]
margin <- apply(data.matrix(olive1),1,sum)
olive1 <- olive1/margin
alr <- (-log( olive1[, -4]/olive1[, 4]))
#select the first 5 principal components
x <- princomp(alr, cor=TRUE)$scores[, 1:5]

#clustering
# not run here for time reasons
cl <- pdfCluster(alr, h = h.norm(alr), hmult=0.75)
summary(cl)
plot(cl)

#comparing groups with original macro-area membership
groups <- groups(cl)
table(oliveoil$macro.area, groups)

#cluster cores
table(groups(cl, stage = 0))



oliveoil_s <- oliveoil
oliveoil_s$cluster <- cl@clusters
subset(oliveoil_s[c(1,2,11)], oliveoil_s$cluster == 0)








confusion_matrix <- table(Cluster = km.out$cluster, Aree = oliveoil$macro.area)

ggplot(data = as.data.frame(as.table(confusion_matrix)), aes(x = Cluster, y = Aree, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Aree", y = "Cluster", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# Convertire l'output della silhouette in un data frame
sil_df <- as.data.frame(silhouette(pam.out)[, 1:3])
colnames(sil_df) <- c("cluster", "neighbor", "sil_width")

# Aggiungere una colonna per l'osservazione originale
sil_df$obs <- 1:nrow(sil_df)

# Ordinare i dati per cluster e silhouette width
sil_df <- sil_df %>% arrange(cluster, desc(sil_width))

# Aggiungere una colonna per l'ordinamento delle osservazioni
sil_df$obs_ordered <- factor(sil_df$obs, levels = sil_df$obs)

# Creare un grafico della silhouette colorato
ggplot(sil_df, aes(x = sil_width, y = obs_ordered, fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(5)) +
  labs(title = "Silhouette Plot", x = "Observation", y = "Silhouette Width") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())






# Convertire l'output della silhouette in un data frame
sil_df <- as.data.frame(silhouette(pam.out)[, 1:3])
colnames(sil_df) <- c("cluster", "neighbor", "sil_width")
sil_df$obs <- 1:nrow(sil_df)
sil_df <- sil_df %>% arrange(cluster, desc(sil_width))
sil_df$obs_ordered <- factor(sil_df$obs, levels = sil_df$obs)

# Creare un grafico della silhouette colorato
ggplot(sil_df, aes(x = obs_ordered, y = sil_width, fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(5)) +
  labs(title = "Silhouette Plot", x = "Observation", y = "Silhouette Width") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



























par(mfrow(1,1))
prop.table(table(db.out$cluster, oliveoil$region),1)

counts <- prop.table(table(db.out$cluster, oliveoil$region), 1)

barplot(prop.table(table(db.out$cluster, oliveoil$region),1), beside = T, legend = F, main = "Poporzione all'interno dei cluster", col = 2:5, cex.names = 0.50)
legend("topright", legend = rownames(counts), fill = 2:5, cex = 0.8, bty = "n")
barplot(prop.table(table(db.out$cluster, oliveoil$region),2), beside = T, legend = F, main = "", col = 2:5, cex.names = 0.50)
legend("topright", legend = rownames(counts), fill = 2:5, cex = 0.8, bty = "n")







table_cluster_region <- prop.table(table(db.out$cluster, oliveoil$region), 1)
data_cluster_region <- as.data.frame(table_cluster_region)
colnames(data_cluster_region) <- c("Cluster", "Region", "Proportion")

table_region_cluster <- prop.table(table(db.out$cluster, oliveoil$region), 2)
data_region_cluster <- as.data.frame(table_region_cluster)
colnames(data_region_cluster) <- c("Cluster", "Region", "Proportion")


ggplot(data_cluster_region, aes(x = Region, y = Proportion, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proporzione all'interno dei cluster", x = "Region", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red", "blue", "green", "purple"))

# Grafico delle proporzioni all'interno delle regioni
ggplot(data_region_cluster, aes(x = Region, y = Proportion, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "", x = "Region", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red", "blue", "green", "purple"))




