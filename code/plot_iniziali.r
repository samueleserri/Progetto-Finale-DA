
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


