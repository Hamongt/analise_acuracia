install.packages("klaR")
install.packages("FactoMineR")
install.packages("plotly")

library(klaR)
dadosk = dados[,-13]
set.seed(2)
modelo = kmodes(dadosk, modes = 2)
cluster = modelo$cluster
dadosk$kmode = cluster
comp = data.frame(dados$classe,dadosk$kmode)

#IRA
install.packages("mclust")  # se não tiver
library(mclust)
adjustedRandIndex(dados$classe, modelo$cluster)
#MUDAR ISSO PQ N É LEGAL A MEDIA E SIM A FREQUENCIA 
dadosk %>%
  group_by(kmode) %>%
  summarise(across(everything(),
                   ~mean(. == 2)))
table(modelo$cluster)


classe = dados$classe
table(kmodes = modelo$cluster, lca = dados$classe)

library(FactoMineR)
dadosm <- data.frame(lapply(dadosm, as.factor))
dadosm <- dadosm[, sapply(dadosm, function(x) length(unique(x)) > 1)]
mca <- MCA(dadosm, graph = FALSE)


  # Definir cores mais bonitas
  cores <- c("#1b9e77", "#d95f02")  # verde + laranja
  
  plot(
    mca$ind$coord[,1],
    mca$ind$coord[,2],
    col = cores[modelo$cluster],
    pch = 19,
    xlab = "Dimensão 1",
    ylab = "Dimensão 2",
    main = "Clusters (K-modes) no espaço do MCA"
  )
  
  # Adicionar linhas de referência
  abline(h = 0, v = 0, col = "gray80", lty = 2)
  
  # Adicionar legenda
  legend(
    "topright",
    legend = c("Cluster 1", "Cluster 2"),
    col = cores,
    pch = 19,
    bty = "n"
  )
  
  

# Definir cores mais bonitas
cores <- c("#1b9e77", "#d95f02")  # verde + laranja

plot(
  mca$ind$coord[,1],
  mca$ind$coord[,2],
  col = cores[dados$classe],
  pch = 19,
  xlab = "Dimensão 1",
  ylab = "Dimensão 2",
  main = "Clusters (LCA) no espaço do MCA"
)

# Adicionar linhas de referência
abline(h = 0, v = 0, col = "gray80", lty = 2)

# Adicionar legenda
legend(
  "topright",
  legend = c("Classe 1", "Classe 2"),
  col = cores,
  pch = 19,
  bty = "n"
)


mca$var$contrib
mca$var$coord
mca$var$cos2

#write.csv(dados, "C:/Users/Hamon/Desktop/TCC/dados_Com_Grupo_LCA.csv", row.names = FALSE)

#write.csv(dadosk, "C:/Users/Hamon/Desktop/TCC/dados_Com_Grupo_KMODES.csv", row.names = FALSE)



#grafico 3d
# instalar se precisar
# install.packages("plotly")

library(plotly)

cores <- c("#1b9e77", "#d95f02")

plot_ly(
  x = mca$ind$coord[,1],
  y = mca$ind$coord[,2],
  z = mca$ind$coord[,3],
  color = as.factor(modelo$cluster),
  colors = cores,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
) %>%
  layout(
    title = "K-modes no espaço 3D do MCA",
    scene = list(
      xaxis = list(title = paste0("Dim 1 (", round(mca$eig[1,2],1), "%)")),
      yaxis = list(title = paste0("Dim 2 (", round(mca$eig[2,2],1), "%)")),
      zaxis = list(title = paste0("Dim 3 (", round(mca$eig[3,2],1), "%)"))
    )
  )


#Variancia explicada
# variância e acumulada
variancia <- mca$eig[,2]
acumulada <- cumsum(variancia)

# barplot
bp <- barplot(
  variancia,
  names.arg = paste0("Dim ", 1:length(variancia)),
  col = "#4c72b0",
  ylim = c(0, 110),
  main = "Variância explicada por dimensão (MCA)",
  ylab = "Percentual (%)"
)

# 🔢 valores em cima das barras (variância individual)
text(
  x = bp,
  y = variancia,
  labels = round(variancia,1),
  pos = 3,
  cex = 0.8
)

# 📈 linha acumulada (passando pelo topo das barras)
lines(
  x = bp,
  y = acumulada,
  type = "b",
  pch = 19,
  lty = 1,
  lwd = 2
)

# 🔢 valores da acumulada
text(
  x = bp,
  y = acumulada,
  labels = round(acumulada,1),
  pos = 3,
  cex = 0.8,
  col = "black"
)

# legenda
legend(
  "topleft",
  legend = c("Variância", "Acumulada"),
  pch = c(15, 19),
  lty = c(NA, 1),
  col = c("#4c72b0", "black"),
  bty = "n"
)














library(ggplot2)
library(dplyr)
library(tidyr)

# Médias por cluster
perfil <- aggregate(. ~ kmode, data = dadosk, mean)

# Converter médias em proporções
perfil[,-1] <- perfil[,-1] - 1

# Formato longo
perfil_long <- pivot_longer(
  perfil,
  cols = -kmode,
  names_to = "Variavel",
  values_to = "Proporcao"
)

# Renomear clusters
perfil_long$kmode <- factor(
  perfil_long$kmode,
  labels = c("Cluster 1", "Cluster 2")
)

# Barplot agrupado
ggplot(
  perfil_long,
  aes(
    x = Variavel,
    y = Proporcao,
    fill = kmode
  )
) +
  
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0,1)
  ) +
  
  scale_fill_manual(
    values = c(
      "Cluster 1" = "#0072B2",
      "Cluster 2" = "#D55E00"
    )
  ) +
  
  labs(
    x = "Fatores de risco",
    y = "Proporção de presença",
    fill = "Agrupamentos"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position = "top",
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12
    ),
    
    axis.text.y = element_text(size = 13),
    
    axis.title = element_text(size = 18),
    
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    
    panel.grid.minor = element_blank()
  )
