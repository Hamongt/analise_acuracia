#mca para cada variavel
dadosm = dadosk[,-13]
vars <- names(dadosm)
library(FactoMineR)
dadosm <- data.frame(lapply(dadosm, as.factor))
dadosm <- dadosm[, sapply(dadosm, function(x) length(unique(x)) > 1)]
mca <- MCA(dadosm, graph = FALSE)


# Definir cores mais bonitas
cores <- c("#1b9e77", "#d95f02")  # verde + laranja

par(
  mar = c(5, 6, 5, 2)  
)

plot(
  mca$ind$coord[,2],
  mca$ind$coord[,3],
  
  col = cores[modelo$cluster],
  
  pch = 19,
  cex = 1.4,
  
  xlab = "Dimensão 2",
  ylab = "Dimensão 3",

  
  cex.lab = 2.2,
  cex.axis = 1.8,
  cex.main = 2,
  
  font.main = 2
)



# Adicionar linhas de referência
abline(h = 0, v = 0, col = "gray80", lty = 2)

# Adicionar legenda
legend(
  "topright",
  legend = c("Cluster 1", "Cluster 2"),
  col = cores,
  pch = 19,
  bty = "n",
  cex = 1.9
)



# Definir cores mais bonitas
cores <- c("#1b9e77", "#d95f02")  # verde + laranja

plot(
  mca$ind$coord[,2],
  mca$ind$coord[,3],
  
  col = cores[dados$classe],
  
  pch = 19,
  cex = 1.4,
  
  xlab = "Dimensão 2",
  ylab = "Dimensão 3",
  
  
  cex.lab = 2.2,
  cex.axis = 1.8,
  cex.main = 2,
  
  font.main = 2
)


# Adicionar linhas de referência
abline(h = 0, v = 0, col = "gray80", lty = 2)

# Adicionar legenda
legend(
  "topright",
  legend = c("Classe 1", "Classe 2"),
  col = cores,
  pch = 19,
  bty = "n",
  cex = 1.9
)


mca$var$contrib
mca$var$coord
mca$var$cos2






table(dadosk$kmode,dados$classe)

# nomes das variáveis (exceto a classe)


# cores (ausente vs presente)
cores <- c("#1b9e77", "#d95f02")

# layout: ajustar conforme 13 gráficos
par(mfrow = c(3,4))  # 16 espaços (sobra 3)

for (v in vars) {
  
  # pegar variável
  grupo <- as.factor(dadosm[[v]]
                     )
  
  plot(
    mca$ind$coord[,1],
    mca$ind$coord[,2],
    col = cores[grupo],
    pch = 19,
    main = v,
    xlab = paste0("Dim 1 (", round(mca$eig[1,2],1), "%)"),
    ylab = paste0("Dim 2 (", round(mca$eig[2,2],1), "%)")
  )
  
  abline(h = 0, v = 0, col = "gray85", lty = 2)
  
  legend(
    "topright",
    legend = c("Ausente", "Presente"),
    col = cores,
    pch = 19,
    bty = "n",
    cex = 0.7
  )
}

install.packages("factoextra")
library(factoextra)
#CONTRIBUIÇÃO EM CADA DIMENSAO
par(mfrow = c(1,2))

fviz_contrib(mca, choice = "var", axes = 1, top = 15)
fviz_contrib(mca, choice = "var", axes = 2, top = 15)
fviz_contrib(mca, choice = "var", axes = 3, top = 15)



library(factoextra)
library(ggplot2)

fviz_contrib(mca, 
             choice = "var", 
             axes = 1, 
             top = 15,
             title = NULL) +              # Remove o título
  labs(x = "Variáveis",                   # Título do eixo X
       y = "Contribuição (%)") +          # Título do eixo Y
  theme(
    axis.title.x = element_text(size = 25),   # Tamanho título eixo X
    axis.title.y = element_text(size = 25),   # Tamanho título eixo Y
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  # Números/nomes eixo X
    axis.text.y = element_text(size = 20)      # Números eixo Y
  )

fviz_contrib(mca, 
             choice = "var", 
             axes = 2, 
             top = 15,
             title = NULL) +              # Remove o título
  labs(x = "Variáveis",                   # Título do eixo X
       y = "Contribuição (%)") +          # Título do eixo Y
  theme(
    axis.title.x = element_text(size = 25),   # Tamanho título eixo X
    axis.title.y = element_text(size = 25),   # Tamanho título eixo Y
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  # Números/nomes eixo X
    axis.text.y = element_text(size = 20)      # Números eixo Y
  )

fviz_contrib(mca, 
             choice = "var", 
             axes = 3, 
             top = 15,
             title = NULL) +              # Remove o título
  labs(x = "Variáveis",                   # Título do eixo X
       y = "Contribuição (%)") +          # Título do eixo Y
  theme(
    axis.title.x = element_text(size = 25),   # Tamanho título eixo X
    axis.title.y = element_text(size = 25),   # Tamanho título eixo Y
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  # Números/nomes eixo X
    axis.text.y = element_text(size = 20)      # Números eixo Y
  )





















library(ggplot2)

# Criar dataframe
eig <- data.frame(
  Dimensao = factor(
    paste0("Dim ", 1:nrow(mca$eig)),
    levels = paste0("Dim ", 1:nrow(mca$eig))
  ),
  
  Variancia = mca$eig[,2],
  Acumulada = cumsum(mca$eig[,2])
)

# Scree plot
ggplot(eig, aes(x = Dimensao, y = Variancia)) +
  
  geom_bar(
    stat = "identity",
    fill = "#4C72B0"
  ) +
  
  geom_line(
    aes(y = Acumulada, group = 1),
    color = "#D55E00",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = Acumulada),
    color = "#D55E00",
    size = 3
  ) +
  
  labs(
    x = "Dimensões",
    y = "Percentual de variância explicada"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    axis.title = element_text(size = 25),
    
    axis.text.x = element_text(
      size = 25,
      angle = 45,
      hjust = 1
    ),
    
    axis.text.y = element_text(size = 25),
    
    panel.grid.minor = element_blank()
  )



