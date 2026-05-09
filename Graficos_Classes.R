library(poLCA)
library(ggplot2)
set.seed(1)
bic <- numeric()
aic <- numeric()

for(k in 1:10){
  modelo <- poLCA(f, dados[,-13],
                  nclass = k,
                  maxiter = 500,
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
  labs(x = "Número de classes latentes",
       y = "Valor do critério",
       color = "Critério") +
  scale_color_manual(values = c("AIC" = "#E69F00", "BIC" = "#56B4E9")) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 20, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    # AUMENTAR TAMANHO DOS NÚMEROS NOS EIXOS
    axis.text.x = element_text(size = 20),  # números do eixo X
    axis.text.y = element_text(size = 20),  # números do eixo Y
    # AUMENTAR TAMANHO DO NOME DOS EIXOS
    axis.title.x = element_text(size = 25),  # título do eixo X
    axis.title.y = element_text(size = 25)   # título do eixo Y
  )





































































library(poLCA)
library(ggplot2)

set.seed(1)

bic <- numeric()
aic <- numeric()
g2  <- numeric()
x2  <- numeric()

for(k in 1:10){
  
  modelo <- poLCA(
    f,
    dados[,-13],
    nclass = k,
    maxiter = 500,
    graphs = FALSE
  )
  
  bic[k] <- modelo$bic
  aic[k] <- modelo$aic
  g2[k]  <- modelo$Gsq
  x2[k]  <- modelo$Chisq
}

# Criar dataframe para ggplot
df <- data.frame(
  classes = rep(1:10, 4),
  valor = c(bic, aic, g2, x2),
  criterio = rep(c("BIC", "AIC", "G2", "X2"), each = 10)
)

# Encontrar melhores modelos
melhor_bic <- which.min(bic)
melhor_aic <- which.min(aic)
melhor_g2  <- which.min(g2)
melhor_x2  <- which.min(x2)

# Gráfico
ggplot(df, aes(x = classes, y = valor, color = criterio, group = criterio)) +
  
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  # Destacar menor BIC
  geom_point(
    data = df[df$classes == melhor_bic & df$criterio == "BIC", ],
    aes(x = classes, y = valor),
    color = "#56B4E9",
    size = 5,
    shape = 1,
    stroke = 1.5
  ) +
  
  # Destacar menor AIC
  geom_point(
    data = df[df$classes == melhor_aic & df$criterio == "AIC", ],
    aes(x = classes, y = valor),
    color = "#E69F00",
    size = 5,
    shape = 1,
    stroke = 1.5
  ) +
  
  # Destacar menor G2
  geom_point(
    data = df[df$classes == melhor_g2 & df$criterio == "G2", ],
    aes(x = classes, y = valor),
    color = "#009E73",
    size = 5,
    shape = 1,
    stroke = 1.5
  ) +
  
  # Destacar menor X2
  geom_point(
    data = df[df$classes == melhor_x2 & df$criterio == "X2", ],
    aes(x = classes, y = valor),
    color = "#CC79A7",
    size = 5,
    shape = 1,
    stroke = 1.5
  ) +
  
  labs(
    x = "Número de classes latentes",
    y = "Valor da estatística",
    color = "Critério"
  ) +
  
  scale_color_manual(
    values = c(
      "AIC" = "#E69F00",
      "BIC" = "#56B4E9",
      "G2"  = "#009E73",
      "X2"  = "#CC79A7"
    )
  ) +
  
  scale_x_continuous(breaks = 1:10) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    
    legend.position = "bottom",
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

