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
  "incontinencia",
  "temperatura_sem_ulcera"
)

resultado <- data.frame()

for(v in variaveis){
  
  tab <- table(dados[[v]], dadosk$kmode)
  
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
tab <- table(dados$desidratacao, dadosk$kmode)

VP <- tab[2,2]
FP <- tab[2,1]
FN <- tab[1,2]
VN <- tab[1,1]

prop.test(VP, VP+FN)  # IC sensibilidade
prop.test(VN, VN+FP)  # IC especificidade
