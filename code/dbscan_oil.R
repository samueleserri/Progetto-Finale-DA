# DB SCAN

kNNdistplot(oliveoil[,3:10], k = 17)
abline(h=0.013)

db.out <- dbscan(oliveoil[,3:10], eps = 0.013, minPts = 18)
db.out


par(mfrow=c(3,3))

# palmitic palmitoleic
plot(oliveoil$palmitic, oliveoil$palmitoleic, col = db.out$cluster+1, pch = 19)

# oleic palmitic
plot(oliveoil$oleic, oliveoil$palmitic, col = db.out$cluster+1, pch = 19)

# oleic palmitoleic
plot(oliveoil$oleic, oliveoil$palmitoleic, col = db.out$cluster+1, pch = 19)

# linoleic palmitoleic
plot(oliveoil$linoleic, oliveoil$palmitoleic, col = db.out$cluster+1, pch = 19)

# linoleic oleic
plot(oliveoil$palmitoleic, oliveoil$linoleic, col = db.out$cluster+1, pch = 19)

# arachidic linolenic
plot(oliveoil$linolenic, oliveoil$arachidic, col = db.out$cluster+1, pch = 19)

# eicosenoic palmitic
plot(oliveoil$palmitic, oliveoil$eicosenoic, col = db.out$cluster+1, pch = 19)

# eicosenoic linolenic
plot(oliveoil$linolenic, oliveoil$eicosenoic, col = db.out$cluster+1, pch = 19)

par(mfrow=c(1,1))




# boxplot
# da descriver uno per uno
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = palmitic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = palmitoleic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = stearic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = oleic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = linoleic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = linolenic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = arachidic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)
ggplot(oliveoil, aes(x = as.factor(db.out$cluster), y = eicosenoic, fill = as.factor(db.out$cluster+1))) + geom_boxplot(width=0.7) + guides(fill = FALSE)






