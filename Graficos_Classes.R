library(poLCA)
library(ggplot2)

bic <- numeric()
aic <- numeric()

for(k in 1:10){
  modelo <- poLCA(f, dados[,-14],
                  nclass = k,
                  maxiter = 1000,
                  graphs = FALSE)
  bic[k] <- modelo$bic
  aic[k] <- modelo$aic
}

# Criar dataframe para ggplot
df <- data.frame(
  classes = rep(1:10, 2),
  valor = c(bic, aic),
  criterio = rep(c("BIC", "AIC"), each = 10)
)

# Encontrar o melhor
melhor_bic <- which.min(bic)
melhor_aic <- which.min(aic)

# Gráfico com ggplot2
ggplot(df, aes(x = classes, y = valor, color = criterio, group = criterio)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_point(data = df[df$classes == melhor_bic & df$criterio == "BIC", ],
             aes(x = classes, y = valor), 
             color = "blue", size = 5, shape = 1, stroke = 1.5) +
  geom_point(data = df[df$classes == melhor_aic & df$criterio == "AIC", ],
             aes(x = classes, y = valor), 
             color = "red", size = 5, shape = 1, stroke = 1.5) +
  labs(title = "Critérios de Informação por Número de Classes",
       subtitle = paste("Melhor BIC: k =", melhor_bic, "| Melhor AIC: k =", melhor_aic),
       x = "Número de classes latentes",
       y = "Valor do critério",
       color = "Critério") +
  scale_color_manual(values = c("AIC" = "#E69F00", "BIC" = "#56B4E9")) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.3)
  )
