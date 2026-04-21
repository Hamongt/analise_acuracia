hist(socio$Idade, 
     main = "Distribuição da Idade",
     xlab = "Idade",
     col = "lightblue",
     breaks = 20)
# Pacotes
library(ggplot2)

# Histograma + densidade normal
ggplot(socio, aes(x = Idade)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15,
                 fill = "#2E86AB",  # azul mais sofisticado
                 color = "#1B4965",  # borda mais escura
                 alpha = 0.7,
                 size = 0.3) +
  stat_function(fun = dnorm,
                args = list(mean = mean(socio$Idade, na.rm = TRUE),
                            sd = sd(socio$Idade, na.rm = TRUE)),
                color = "#D62828",  # vermelho que contrasta bem
                size = 1.2,
                linetype = "solid") +
  labs(title = "Distribuição da Idade",
       subtitle = paste0("Média = ", round(mean(socio$Idade, na.rm = TRUE), 1),
                         " | Desvio Padrão = ", round(sd(socio$Idade, na.rm = TRUE), 1)),
       x = "Idade (anos)",
       y = "Densidade") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1B4965", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "#5C677D", hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(size = 12, face = "bold", color = "#1B4965"),
    axis.text = element_text(size = 10, color = "#5C677D"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", size = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
# Pacote necessário
install.packages("car")
library(car)

qqPlot(socio$Idade,
       main = "QQ-Plot com Envelope",
       col = "black",
       pch = 19,
       envelope = 0.95)
summary(socio$Idade)
sd(socio$Idade)
shapiro.test(socio$Idade)
table(socio$Idade)
#bimodal 53 e 62

########### Renda #########
ggplot(socio, aes(x = Renda)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60,
                 fill = "#2E86AB",  # azul sofisticado
                 color = "#1B4965",  # borda mais escura
                 alpha = 0.7,
                 size = 0.2) +
  stat_function(fun = dnorm,
                args = list(mean = mean(socio$Renda, na.rm = TRUE),
                            sd = sd(socio$Renda, na.rm = TRUE)),
                color = "#D62828",  # vermelho que contrasta bem
                size = 1.2,
                linetype = "solid") +
  scale_x_continuous(labels = scales::comma_format(prefix = "R$ ")) +
  labs(title = "Distribuição da Renda",
       subtitle = paste0("Média = R$ ", round(mean(socio$Renda, na.rm = TRUE), 0),
                         " | Desvio Padrão = R$ ", round(sd(socio$Renda, na.rm = TRUE), 0)
                         ),
       x = "Renda (em Reais)",
       y = "Densidade") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1B4965", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "#5C677D", hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(size = 12, face = "bold", color = "#1B4965"),
    axis.text = element_text(size = 10, color = "#5C677D"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", size = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
summary(socio$Renda)
sd(socio$Renda)
shapiro.test(socio$Renda)
table(socio$Renda)
#moda 131


########## Quantidade de doenças##########
m = socio$Idade~socio$Quantidade
plot(socio$idade,socio$Quantidade)
hist(socio$Quantidade,
     main = "Distribuição de Renda",
     xlab = "Renda",
     col = "pink",
     breaks = 60)
plot(density(socio$Quantidade),
     main = "Densidade de Renda",
     col = "pink",
     lwd = 2)
summary(socio$Quantidade)
sd(socio$Quantidade)
shapiro.test(socio$Quantidade)
table(socio$Quantidade)
#moda 1
boxplot(socio$Idade~socio$Quantidade)

library(ggplot2)
library(ggplot2)

ggplot(socio, aes(x = as.factor(Quantidade), y = Idade)) +
  geom_boxplot(fill = "#00BFFF", alpha = 0.7) +
  geom_jitter(aes(color = as.factor(Quantidade)), 
              width = 0.2, height = 0, alpha = 0.8, size = 1.5) +
  labs(title = "Boxplot Idades por Quantidade de Comorbidades",
       x = "Quantidade",
       y = "Idade",
       color = "Quantidade") +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Calcular estatísticas para cada categoria
stats_renda <- socio %>%
  group_by(Quantidade) %>%
  summarise(
    n = n(),
    media_idade = round(mean(Idade, na.rm = TRUE), 1),
    sd_idade = round(sd(Idade, na.rm = TRUE), 1),
    max_idade = max(Idade, na.rm = TRUE),
    min_idade = min(Idade, na.rm = TRUE),
    .groups = 'drop'
  )

# Calcular o limite superior do gráfico para posicionar os textos
y_max <- max(socio$Idade, na.rm = TRUE)
y_min <- min(socio$Idade, na.rm = TRUE)
y_range <- y_max - y_min

ggplot(socio, aes(x = as.factor(Quantidade), y = Idade)) +
  geom_boxplot(fill = "#2E86AB",
               color = "#1B4965",
               alpha = 0.7,
               outlier.color = "#D62828",
               outlier.size = 1.5) +
  geom_jitter(aes(color = as.factor(Quantidade)), 
              width = 0.2, 
              height = 0, 
              alpha = 0.5, 
              size = 1) +
  # Posicionar textos ACIMA do boxplot (fora do gráfico)
  geom_text(data = stats_renda,
            aes(x = as.factor(Quantidade), 
                y = y_max + (y_range * 0.05),  # 5% acima do máximo
                label = paste0("Média: ", media_idade, "\nn=", n)),
            size = 3.2,
            color = "#1B4965",
            fontface = "bold",
            hjust = 0.5) +
  scale_color_manual(values = c("#483D8B", "#483D8B", "#483D8B", "#483D8B", "#D62828", "#E58A8A")) +
  # Ajustar limites do eixo Y para acomodar os textos
  coord_cartesian(ylim = c(y_min, y_max + (y_range * 0.15))) +
  labs(title = "Distribuição da Idade por Número de Comorbidades",
       subtitle = paste0("Média global de comorbidades = ", round(mean(socio$Quantidade, na.rm = TRUE), 2),
                         " | Desvio Padrão = ", round(sd(socio$Quantidade, na.rm = TRUE), 2)),
       x = "Número de Comorbidades",
       y = "Idade (anos)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1B4965", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "#5C677D", hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(size = 12, face = "bold", color = "#1B4965"),
    axis.text = element_text(size = 10, color = "#5C677D"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", size = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )

#install.packages("ggridges")
#library(ggridges)

#ggplot(socio, aes(x = Idade, y = factor(Quantidade), fill = factor(Quantidade))) +
 # geom_density_ridges(alpha = 0.7) 


 
