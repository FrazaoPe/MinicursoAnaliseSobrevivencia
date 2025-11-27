#AULA2.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.2
#Professora: Núbia Karla
#Programador: Pedro Frazão Dutra
#Carregamento de pacotes necessários
require(tidyverse)
require(survival)

## Carregamento dos dados ##
leite = read_table(file = "leite.txt")

## Montando tabela com os dados de sobrevivência gerados pela função survfit
k = survival::survfit(Surv(leite$tempo,leite$status)~1, data = leite)
tempo = k[["time"]]
R = k[["n.risk"]]
N = k[["n.event"]]
tabela = data.frame(tempo,R,N)
dimensao <- dim(tabela)

## Delta
tabela$delta[1]= tabela$tempo[1] 
for(i in 2:nrow(tabela)){
  tabela$delta[i]= tabela$tempo[i]-tabela$tempo[i-1]
}

## Densidade
tabela$densidade= round(tabela$N/(tabela$R[1]*tabela$delta),4)

## Sobrevivência
tabela$sobrevivencia= round(tabela$R/tabela$R[1],4)

## Risco
tabela$λ = round(tabela$N/(tabela$R*tabela$delta),4)

## Risco acumulado
tabela$Λ[1] = 0
S = tabela$Λ[1] 
i = 2
while (i < (dimensao[1]+1)){
  S = round(S + (tabela$λ[i-1]*tabela$delta[i-1]),4)
  tabela$Λ[i] = S
  i = i + 1
}

## Limite inferior dos intervalos de tempo
tabela$limInf[1] = 0
for(i in 2:nrow(tabela)){
  tabela$limInf[i]= tabela$tempo[i-1]
}

## Apenas melhorando o aspecto da tabela
tabela <- tabela |> rename(limSup = tempo)|> 
  select(limInf,limSup, everything())

## Curva de sobrevivência
ggplot(tabela, aes(limInf,sobrevivencia)) + geom_step(color = "#008c28") +
  scale_y_continuous(name= "S(t)", breaks = seq(0,1,0.10)) +
  scale_x_continuous(name= "meses", breaks = seq(0,12,1)) +
  theme_bw()

## Curva de densidade
ggplot(tabela, aes(limInf,densidade)) + geom_step(color = "#008c28") +
  scale_y_continuous(name = "f(t)", breaks = seq(0,0.2,0.02), limits = c(0,0.2)) +
  scale_x_continuous(name = "meses", breaks = seq(0,12,1)) +
  theme_bw()

## Curva de risco
ggplot(tabela, aes(limInf,λ)) + geom_step(color = "#700103") +
  scale_y_continuous(name= "λ(t)", breaks = seq(0,1,0.05)) +
  scale_x_continuous(name= "meses", breaks = seq(0,12,1)) +
  theme_bw()

## Curva de risco acumulado
ggplot(tabela, aes(limInf,Λ)) + geom_step(color = "#700103") +
  scale_y_continuous(name= "Λ(t)", breaks = seq(0,3.5,0.25)) +
  scale_x_continuous(name= "meses", breaks = seq(0,12,1)) +
  theme_bw()

## Terminação
