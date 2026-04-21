install.packages("klaR")
install.packages("FactoMineR")

library(klaR)
dadosk = dados[,-14]
set.seed(1)
modelo = kmodes(dadosk, modes = 2)
cluster = modelo$cluster

classe = dados$classe
table(kmodes = modelo$cluster, lca = dados$classe)

library(FactoMineR)
dadosk <- data.frame(lapply(dadosk, as.factor))
dadosk <- dadosk[, sapply(dadosk, function(x) length(unique(x)) > 1)]
mca <- MCA(dadosk, graph = FALSE)

  plot(
  mca$ind$coord[,1],
  mca$ind$coord[,2],
  col = modelo$cluster,
  pch = 19, main = "KMODE"
)

plot(
  mca$ind$coord[,1],
  mca$ind$coord[,2],
  col = dados$classe,
  pch = 19, main = "LCA"
)
dadosk$kmode = cluster
comp = data.frame(dados$classe,dadosk$kmode)

#IRA
install.packages("mclust")  # se não tiver
library(mclust)
adjustedRandIndex(dados$classe, modelo$cluster)

#write.csv(dados, "C:/Users/Hamon/Desktop/TCC/dados_Com_Grupo_LCA.csv", row.names = FALSE)

#write.csv(dadosk, "C:/Users/Hamon/Desktop/TCC/dados_Com_Grupo_KMODES.csv", row.names = FALSE)
