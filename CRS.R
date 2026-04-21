variaveis_lca <- c("mobilidade",
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
                   "incontinencia",
                   "temperatura_sem_ulcera")

# contar quantas estão presentes (== 2)
dados$num_presentes <- rowSums(dados[, variaveis_lca] == 2, na.rm = TRUE)

# CRS com limiar
dados$CRS_full <- as.numeric(dados$num_presentes >= 6)

table(dados$classe, dados$CRS_full)

adjustedRandIndex(dados$classe, dados$CRS_full)

