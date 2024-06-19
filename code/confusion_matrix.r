


confusion_matrix <- table(Cluster = km.out$cluster, Aree = oliveoil$macro.area)

ggplot(data = as.data.frame(as.table(confusion_matrix)), aes(x = Cluster, y = Aree, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Aree", y = "Cluster", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


