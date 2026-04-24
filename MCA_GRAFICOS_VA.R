#mca para cada variavel

# nomes das variáveis (exceto a classe)
vars <- names(dadosk[-13])

# cores (ausente vs presente)
cores <- c("#1b9e77", "#d95f02")

# layout: ajustar conforme 13 gráficos
par(mfrow = c(3, 4))  # 16 espaços (sobra 3)

for (v in vars) {
  
  # pegar variável
  grupo <- as.factor(dadosk[[v]])
  
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
