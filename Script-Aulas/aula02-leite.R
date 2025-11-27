#AULA2.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.2
#Professora: Núbia Karla
#Programadores: Pedro Frazão Dutra e Lucas Oliveira
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
dimensao=dim(tabela)

## Delta
tabela$delta[1]=tabela$tempo[1]
for(i in 2:dimensao[1]){
  tabela$delta[i]=tabela$tempo[i]-tabela$tempo[i-1]
}

## Função de densidade
tabela$densidade=tabela$N/(tabela$R[1]*tabela$delta)

## Função de sobrevivência
tabela$sobrevivencia=tabela$R/tabela$R[1]

## Limite inferior dos intervalos de tempo 
tabela$limInf[1] = 0
for(i in 2:8){
  tabela$limInf[i]= tabela$tempo[i-1]
}

## Curva de sobrevivência
ggplot(tabela, aes(limInf,sobrevivencia)) + geom_step(color = "#008c28") +
  scale_y_continuous(name= "S(t)", breaks = seq(0,1,0.10)) +
  scale_x_continuous(name= "Meses", breaks = seq(0,12,1)) +
  theme_classic()

## Curva de densidade
ggplot(tabela, aes(limInf,densidade)) + geom_step(color = "#008c28") +
  scale_y_continuous(name= "f(t)", breaks = seq(0,0.20,0.02)) +
  scale_x_continuous(name= "Meses", breaks = seq(0,12,1)) +
  theme_classic()

## Terminação
