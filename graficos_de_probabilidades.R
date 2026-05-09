
library(poLCA)
library(ggplot2)
library(dplyr)
library(tidyr)

# Ajustar modelo
lca2 <- poLCA(f, dados, nclass = 2, maxiter = 1000, graphs = FALSE)

# Extrair probabilidades condicionais
probs <- lca2$probs

# Organizar dataframe
df_plot <- data.frame(
  variavel = names(probs),
  Classe1 = sapply(probs, function(x) x[1,2]),
  Classe2 = sapply(probs, function(x) x[2,2])
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

