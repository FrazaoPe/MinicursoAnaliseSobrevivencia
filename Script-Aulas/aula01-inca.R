#Aula01-inca.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.1
#Professora: Núbia Carla
#Programadores: Pedro Frazão Dutra e Lucas Oliveira
## Carregamento dos pacotes necessários ##
require(tidyverse)
require(survival)

## Carregamentos dos dados ##
inca <- read.csv2(file = "inca.csv")

## Renomeação de variáveis ##
inca1 <- inca |> rename(inicio = DATAINITRT,
                        fim = DATAOBITO)

## Transformando colunas em data ##
inca1 <- inca1 |> 
  mutate(
    inicio = as.Date(inicio, format = "%d/%m/%Y"),
    fim = as.Date(fim, format = "%d/%m/%Y")
  )

## Criando variável status ##
inca1 <- inca1 |> mutate(status = ifelse(is.na(fim),0,1))
  

## Trocando NA's pela data de terminação do estudo ##
inca1 <- inca1 |> mutate(fim = if_else(is.na(fim), as.Date("2023-02-23"), fim))

## Criando variável tempo ##
inca1 <- inca1 |> 
  mutate(tempo = as.numeric(fim - inicio))

## Histograma do tempo decorrido até o óbito
hist(inca1$tempo[inca1$status == 1], 
     main="Histograma do tempo decorrido até o desfecho (óbito)", 
     xlab="tempo")

## Histograma do tempo decorrido até a censura
hist(inca1$tempo[inca1$status == 0], 
     main="Histograma do tempo decorrido até o desfecho (censura)", 
     xlab="tempo")
