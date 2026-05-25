library(poLCA)
library(dplyr)
si = simu[,-13]
colnames(si)
colnames(si) <- c("mobilidade",
                     "nutricao",
                     "pele_ressecada",
                     "tabagismo",
                     "umidade",
                     "temperatura",
                     "mucosas",
                     "edema",
                     "desidratacao",
                     "conhecimento_cuidador",
                     "anemia",
                     "incontinencia")
set.seed(4)
sim = cbind(mobilidade,
          nutricao,
          pele_ressecada,
          tabagismo,
          umidade,
          temperatura,
          mucosas,
          edema,
          desidratacao,
          conhecimento_cuidador,
          anemia,
          incontinencia) ~ 1

l2 = poLCA(sim, si, nclass = 2, maxiter = 1000, graphs = TRUE)
l2$bic
l2$probs
l2$posterior
si$LCA = l2$predclass
table(si$LCA)
table(simu$classe_real,si$LCA)
table(simu$classe_real)
































library(poLCA)
library(ggplot2)
set.seed(1)
bic <- numeric()
aic <- numeric()

for(k in 1:10){
  modelo <- poLCA(sim, si[,-13],
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
library(dplyr)
library(tidyr)

# Ajustar modelo
l2 <- poLCA(sim, simu, nclass = 2, maxiter = 1000, graphs = FALSE)

# Extrair probabilidades condicionais
probs <- l2$probs

# Organizar dataframe
df_plot <- data.frame(
  variavel = names(probs),
  Classe1 = sapply(probs, function(x) x[2,2]),
  Classe2 = sapply(probs, function(x) x[1,2])
)

# Transformar formato
df_long <- pivot_longer(
  df_plot,
  cols = starts_with("Classe"),
  names_to = "Classe",
  values_to = "Probabilidade"
)

# Gráfico
ggplot(df_long,
       aes(x = variavel,
           y = Probabilidade,
           color = Classe,
           group = Classe)) +
  
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,1,0.2)
  ) +
  
  scale_color_manual(
    values = c(
      "Classe1" = "#0072B2",
      "Classe2" = "#D55E00"
    ),
    labels = c(
      "Classe 1",
      "Classe 2"
    )
  ) +
  
  labs(
    x = "Variáveis",
    y = "Probabilidade condicional",
    color = "Classes Latentes"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 22
    ),
    
    axis.text.y = element_text(size = 22),
    
    axis.title = element_text(size = 22),
    
    legend.position = "top",
    
    legend.title = element_text(size = 23),
    legend.text = element_text(size = 23),
    
    panel.grid.minor = element_blank()
  )

library(mclust)
adjustedRandIndex(simu$classe_real,si$LCA )




























































