n = 250 
#Número de pacientes simulados
set.seed(12)
#gerando a classe real dos pacientes
#foram gerados o total de 250 simulaçoes usando a binomial com uma tentativa com probabilidade de sucesso de 0.4
# Oque signfica que 40% dos pacientes estarão dentro da classe de risco real
classe_real = rbinom(n, 1, 0.4)

#atribuindo probabilidades baixas para associar posteriormente cada paciente sem esta na classe de risco
probs_baixo = c(
  0.10, 0.20, 0.15, 0.25,
  0.10, 0.30, 0.20, 0.15,
  0.25, 0.10, 0.20, 0.15
)
#atribuindo probabilidades altas para associar posteriormente cada paciente sem esta na classe de risco
probs_alto = c(
  0.80, 0.75, 0.85, 0.70,
  0.90, 0.80, 0.75, 0.85,
  0.70, 0.80, 0.90, 0.75
)


simu = matrix(0, nrow = n, ncol = 12)
# cria uma matriz com 12 colunsa com informações iguais a 0 e "n" linhas
for(j in 1:12){

  # indivíduos baixo risco
  a0 = which(classe_real == 0)
# A função which verifica quais sao os pacientes com informações iguais a 0 em cada coluna 
# ou seja a0 é um vetor onde so existe os pacientes sem risco real
  
  simu[a0, j] =
    rbinom(length(a0), 1, probs_baixo[j])
# gera o total de pacientes sem risco de uma dist binomial com 1 tentativa e prbabilidade de sucesso atribuido 
  #a cada baixa probabilidade das probs_baixo
  # indivíduos alto risco
  a1 = which(classe_real == 1) #o mesmo para a1 e os demais

  simu[a1, j] = #Vai preencher as colunas == 0 
    rbinom(length(a1), 1, probs_alto[j] )
}

# -----------------------------
# DATA FRAME
# -----------------------------

simu = as.data.frame(simu)

names(simu) = c(
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

simu$classe_real = classe_real
simu = replace(simu, simu == 1, 2)
simu = replace(simu, simu == 0, 1)

head(simu)
table(simu$classe_real)

for(i in 1:12){
  print(names(simu)[i])
  print(table(simu[,i]))
}

