library(pdfCluster)
library(moments)
data("oliveoil")

oliveoil[,3:10] <- oliveoil[,3:10]+1

for (i in 1:nrow(oliveoil)){
  oliveoil[i,3:10] <- oliveoil[i,3:10]/sum(oliveoil[i,3:10])
}

#----------------

#Distribuzioni Var. Quant.
display_summary_and_var <- function(variabile){
  c(summary(variabile), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T),
    sk = skewness(variabile, na.rm = T))
}

#Funzione per le Variabili Qualitative
display_table <- function(variabile, titolo){
  DistAs <- table(variabile)
  DistRe <- prop.table(table(variabile))
  barplot(table(variabile), main = titolo)
  print(rbind(DistAs, DistRe))
}

#----------------

par(mfrow = c(1,1))

#Macro Area
display_table(oliveoil$macro.area, "Macro Area")

#Regioni
display_table(oliveoil$region, "Regioni")

#----------------

par(mfrow = c(1,3))

#Acido palmitic
display_summary_and_var(oliveoil$palmitic)
hist(oliveoil$palmitic, probability = T,col = 5, main = "palmitic")
boxplot(oliveoil$palmitic~oliveoil$macro.area)
boxplot(oliveoil$palmitic~oliveoil$region)

#Acido palmitoleic
display_summary_and_var(oliveoil$palmitoleic)
hist(oliveoil$palmitoleic,col = 5, main = "palmitoleic")
boxplot(oliveoil$palmitoleic~oliveoil$macro.area)
boxplot(oliveoil$palmitoleic~oliveoil$region)

#Acido stearic
display_summary_and_var(oliveoil$stearic)
hist(oliveoil$stearic, col = 5, main = "stearic")
boxplot(oliveoil$stearic~oliveoil$macro.area)
boxplot(oliveoil$stearic~oliveoil$region)

#Acido oleic
display_summary_and_var(oliveoil$oleic)
hist(oliveoil$oleic, col = 5, main = "oleic")
boxplot(oliveoil$oleic~oliveoil$macro.area)
boxplot(oliveoil$oleic~oliveoil$region)

#Acido linoleic
display_summary_and_var(oliveoil$linoleic)
hist(oliveoil$linoleic, col = 5, main = "linoleic")
boxplot(oliveoil$linoleic~oliveoil$macro.area)
boxplot(oliveoil$linoleic~oliveoil$region)

#Acido linolenic
display_summary_and_var(oliveoil$linolenic)
hist(oliveoil$linolenic, col = 5, main = "linolenic")
boxplot(oliveoil$linolenic~oliveoil$macro.area)
boxplot(oliveoil$linolenic~oliveoil$region)

#Acido arachidic
display_summary_and_var(oliveoil$arachidic)
hist(oliveoil$arachidic, col = 5, main = "arachidic")
boxplot(oliveoil$arachidic~oliveoil$macro.area)
boxplot(oliveoil$arachidic~oliveoil$region)

#Acido eicosenoic
display_summary_and_var(oliveoil$eicosenoic)
hist(oliveoil$eicosenoic, col = 5, main = "eicosenoic")
boxplot(oliveoil$eicosenoic~oliveoil$macro.area)
boxplot(oliveoil$eicosenoic~oliveoil$region)

#----------------
