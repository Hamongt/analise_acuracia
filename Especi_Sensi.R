variaveis <- c(
  "mobilidade",
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
  "incontinencia"
)

resultado <- data.frame()

for(v in variaveis){
  
  tab <- table(dados[[v]], dados$classe)
  
  VP <- tab[2,2]
  FP <- tab[2,1]
  FN <- tab[1,2]
  VN <- tab[1,1]
  
  Se <- VP/(VP+FN)
  Sp <- VN/(VN+FP)
  
  VPP <- VP/(VP+FP)
  VPN <- VN/(VN+FN)
  
  LRp <- Se/(1-Sp)
  LRn <- (1-Se)/Sp
  
  resultado <- rbind(resultado,
                     data.frame(
                       Variavel = v,
                       Sensibilidade = Se,
                       Especificidade = Sp,
                       VPP = VPP,
                       VPN = VPN,
                       LR_pos = LRp,
                       LR_neg = LRn
                     ))
  
}

resultado
tab <- table(dados$desidratacao, dados$classe)

VP <- tab[2,2]
FP <- tab[2,1]
FN <- tab[1,2]
VN <- tab[1,1]

prop.test(VP, VP+FN)  # IC sensibilidade
prop.test(VN, VN+FP)  # IC especificidade

install.packages("kableExtra")
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# =========================
# TABELA FINAL FORMATADA
# =========================

resultado_formatado <- resultado %>%
  mutate(
    Sensibilidade = round(Sensibilidade, 3),
    Especificidade = round(Especificidade, 3),
    VPP = round(VPP, 3),
    VPN = round(VPN, 3),
    LR_pos = round(LR_pos, 3),
    LR_neg = round(LR_neg, 3)
  )

# Visualizar tabela
kable(
  resultado_formatado,
  caption = "Medidas de acurácia diagnóstica dos indicadores clínicos",
  col.names = c(
    "Indicador",
    "Sensibilidade",
    "Especificidade",
    "VPP",
    "VPN",
    "RV+",
    "RV-"
  ),
  align = "c"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  )

# =========================
# GRÁFICO 1
# Sensibilidade vs Especificidade
# =========================

resultado_long <- resultado %>%
  select(Variavel, Sensibilidade, Especificidade) %>%
  pivot_longer(
    cols = c(Sensibilidade, Especificidade),
    names_to = "Medida",
    values_to = "Valor"
  )

ggplot(resultado_long,
       aes(x = reorder(Variavel, Valor),
           y = Valor,
           fill = Medida)) +
  
  geom_col(position = "dodge") +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "Sensibilidade" = "#2E86AB",
      "Especificidade" = "#D62828"
    )
  ) +
  
  labs(
    title = "Sensibilidade e Especificidade dos Indicadores",
    x = "Indicadores Clínicos",
    y = "Valor",
    fill = ""
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(face = "bold"),
    
    legend.position = "top"
  )

# =========================
# GRÁFICO 2
# Razão de Verossimilhança Positiva
# =========================

ggplot(resultado,
       aes(x = reorder(Variavel, LR_pos),
           y = LR_pos)) +
  
  geom_col(fill = "#1B4965", alpha = 0.85) +
  
  geom_hline(
    yintercept = 10,
    linetype = "dashed",
    color = "#D62828",
    linewidth = 1
  ) +
  
  coord_flip() +
  
  labs(
    title = "Razão de Verossimilhança Positiva (RV+)",
    subtitle = "Linha tracejada indica RV+ = 10",
    x = "Indicadores Clínicos",
    y = "RV+"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    axis.text.y = element_text(face = "bold")
  )

# =========================
# GRÁFICO 3
# Heatmap das métricas
# =========================

heatmap_dados <- resultado %>%
  select(
    Variavel,
    Sensibilidade,
    Especificidade,
    VPP,
    VPN
  ) %>%
  pivot_longer(
    cols = -Variavel,
    names_to = "Metrica",
    values_to = "Valor"
  )

ggplot(
  heatmap_dados,
  aes(
    x = Metrica,
    y = Variavel,
    fill = Valor
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(
    aes(label = round(Valor, 2)),
    color = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  
  scale_fill_gradient(
    low = "#E0ECF4",
    high = "#023858"
  ) +
  
  labs(
    title = "Mapa de Calor das Medidas de Acurácia",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      face = "bold",
      angle = 15
    ),
    
    axis.text.y = element_text(face = "bold")
  )
