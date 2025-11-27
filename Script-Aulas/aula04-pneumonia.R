#AULA04-Pneumonia.R
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

## Carregamento dos dados 
pneu = read_csv2(file = "pneumonia.csv")

## Rodando o modelo Km
y <- Surv(pneu$dias_perm,pneu$alta)
km <- survfit(y~1, data = pneu)

## Gráfico de sobrevivência
graf = ggsurvplot(km, censor = F, conf.int = F, break.time.by = 100,
      color = "#017530", xlab = "Tempo (dias)", ylab = "S(t)",
      ggtheme = theme_bw(), surv.median.line = "hv")

graf$plot + 
scale_x_continuous(breaks = seq(0,400,50))


## Rodando o modelo Nelson-Aalen
nelson <- survfit(coxph(y~1,data = pneu))

## Gráfico de risco
ggsurvplot(nelson,censor = F, data = pneu, fun = "cumhaz", conf.int = F,
           break.time.by = 50, color = "red", xlab = "Tempo (dias)", ylab = "Λ(t)",
          ggtheme = theme_bw())

## Terminação
