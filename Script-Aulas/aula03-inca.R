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
library(tidyverse)
library(survival)

## Carregamento dos dados 
inca = read_csv2(file = "inca.csv")

## Renomeação de variáveis
inca <- inca |> rename(inicio = DATAINITRT,
                        fim = DATAOBITO)

## Transformando colunas em data
inca <- inca |> 
  mutate(
    inicio = as.Date(inicio, format = "%d/%m/%Y"),
    fim = as.Date(fim, format = "%d/%m/%Y")
  )

## Criando variável status
inca <- inca |> mutate(status = ifelse(is.na(fim),0,1))

## Trocando NA's pela data de terminação do estudo ##
inca$fim[is.na(inca$fim) == TRUE] = as.Date("2023-02-23")

## Criando variável tempo
inca <- inca |> 
  mutate(tempo = as.numeric(fim - inicio))

## selecionando só os que falharam

inca1 = inca[inca$status ==1,]

## Montando tabela com os dados de sobrevivência gerados pela função survfit
k = survival::survfit(Surv(inca1$tempo,inca1$status)~1, data = inca1)
tempo = k[["time"]]
R = k[["n.risk"]]
N = k[["n.event"]]
tabela = data.frame(tempo,R,N)
dimensao = dim(tabela)

## delta (tamanho do intervalo)
tabela$delta[1]=tabela$tempo[1]-0
for(i in 2:dimensao[1]){
  tabela$delta[i]=tabela$tempo[i]-tabela$tempo[i-1]
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

## Curva de sobrevivência
ggplot(tabela, aes(limInf,sobrevivencia)) + geom_step(color = "#008c28") +
  scale_y_continuous(name= "S(t)") +
  scale_x_continuous(name= "meses") +
  theme_bw()

## Curva de densidade
ggplot(tabela, aes(limInf,densidade)) + geom_step(color = "#008c28") +
  scale_y_continuous(name = "f(t)",) +
  scale_x_continuous(name = "meses") +
  theme_bw()

## Curva de risco
ggplot(tabela, aes(limInf,λ)) + geom_step(color = "#700103") +
  scale_y_continuous(name= "λ(t)") +
  scale_x_continuous(name= "meses", breaks = seq(0,800,100)) +
  theme_bw()

## Curva de risco acumulado
ggplot(tabela, aes(limInf,Λ)) + geom_step(color = "#700103") +
  scale_y_continuous(name= "Λ(t)") +
  scale_x_continuous(name= "meses", breaks = seq(0,800,100)) +
  theme_bw()

## Terminação
