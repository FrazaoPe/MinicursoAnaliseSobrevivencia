#AULA04-Inca.R
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
require(survminer)

## Carregamentos dos dados ##
inca <- read.csv2(file = "inca.csv")

## Renomeação de variáveis ##
inca <- inca |> rename(inicio = DATAINITRT,
                        fim = DATAOBITO)

## Transformando colunas em data ##
inca <- inca |> 
  mutate(
    inicio = as.Date(inicio, format = "%d/%m/%Y"),
    fim = as.Date(fim, format = "%d/%m/%Y")
  )

## Criando variável status ##
inca <- inca |> mutate(status = ifelse(is.na(fim),0,1))

## Trocando NA's pela data de terminação do estudo ##
inca$fim[is.na(inca$fim) == TRUE] = as.Date("2023-02-23")

## Criando variável tempo ##
inca <- inca |> 
  mutate(tempo = as.numeric(fim - inicio))

## Rodando o modelo Km
y <- Surv(inca$tempo,inca$status)
km <- survfit(y~1, data = inca)

## Gráficos de sobrevivência
graf = ggsurvplot(km, censor = F, conf.int = F, break.time.by = 100,
                  color = "#017530", xlab = "Tempo (dias)", ylab = "S(t)",
                  ggtheme = theme_classic(), surv.median.line = "hv")

graf$plot + 
scale_x_continuous(breaks = seq(0,774,75)) +
scale_y_continuous(breaks = seq(0,1,0.20))

## Rodando o modelo Nelson-Aalen
nelson <- survfit(coxph(y~1,data = inca))

## Gráfico de risco
graf1 = ggsurvplot(nelson,censor = F, data = inca, fun = "cumhaz", conf.int = F,
                   break.time.by = 50, color = "red", xlab = "Tempo (dias)", ylab = "Λ(t)",
                   ggtheme = theme_classic())

graf1$plot +
  scale_x_continuous(breaks = seq(0,774,100)) +
  scale_y_continuous(limits = c(0,0.3), breaks = seq(0,0.3,0.1))
