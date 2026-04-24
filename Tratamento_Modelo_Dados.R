install.packages("poLCA")
install.packages("dplyr")
library(poLCA)
library(dplyr)
dados = DadosDeVDD[,-13]
colnames(dados)
colnames(dados) <- c("mobilidade",
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
dados = dados %>%
  mutate(across(everything(),
                ~ifelse(. == "Presente",2,
                        ifelse(. == "Ausente",1,NA))))
set.seed(123)
f = cbind(mobilidade,
           nutricao,
           pele_ressecada,
           tabagismo,
           temperatura,
           mucosas,
           edema,
           desidratacao,
           conhecimento_cuidador,
           anemia,
           incontinencia) ~ 1

lca2 = poLCA(f, dados, nclass = 2, maxiter = 1000, graphs = TRUE)
lca3 = poLCA(f, dados, nclass = 3, maxiter = 1000, na.rm = FALSE)
lca2$bic
lca3$bic
lca2$probs
dados$classe = lca2$predclass
table(dados$classe)
lca2$posterior


#write.csv(dados, "C:/Users/Hamon/Desktop/TCC/dados_completos.csv", row.names = FALSE)
