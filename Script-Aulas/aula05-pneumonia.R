#AULA05-pneumonia.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Semestre 2024.1
#Professora: Núbia Karla
#Programador: Pedro Frazão Dutra
#Carregamento de pacotes necessários
require(tidyverse)
require(survival)
require(survminer)

## Carregamento dos dados
pneu = read_csv2(file = "pneumonia.csv")

## Categorização das idades e das raças
pneu <- pneu %>%
  mutate(faixa_etaria = case_when(
    idade < 13 ~ "criança",
    idade < 18 ~ "adolescente",
    idade < 65 ~ "adulto",
    TRUE ~ "idoso")) |> 
  mutate(raca = case_when(
    raca_cor == 1 ~ "branco",
    raca_cor == 2 ~ "pardo",
    raca_cor == 3 ~ "negro",
    raca_cor == 4 ~ "outros",
    raca_cor == 5 ~ "outros"))

## Rodando o modelo Kaplan-Meier estratificado por faixa-etária
y <- Surv(pneu$dias_perm,pneu$alta)
km1 <- survfit(y~pneu$faixa_etaria, data = pneu)

## Função de aalen segundo faixa etária
aalen1 <- survfit(coxph(y~strata(pneu$faixa_etaria), data = pneu))

## Gráfico de sobrevivência segundo faixa-etária
graf1 <- ggsurvplot(km1, censor = F,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   conf.int = F,
                   title = "Sobrevivência segundo faixa etária",
                   legend.labs = c("criança","adolescente","adulto","idoso"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf1 <- graf1$plot + 
  scale_x_continuous(breaks = seq(0,350,50)) +
  scale_y_continuous(breaks = seq(0,1,0.20)) +
  scale_color_manual(values = c("criança" = "#020305", 
                                "adolescente" = "#ec225e",
                                "adulto" = "#269199",
                                "idoso" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf1

## Gráfico de risco acumulado segundo faixa etária
graf2 <- ggsurvplot(aalen1, data = pneu, fun = "cumhaz", censor = F,
                    xlab = "Tempo (dias)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    title = "Risco acumulado segundo faixa etária",
                    legend.labs = c("criança","adolescente","adulto","idoso"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf2 <- graf2$plot + 
  scale_x_continuous(breaks = seq(0,350,50)) +
  scale_y_continuous(breaks = seq(0,10,1),
                     limits = c(0,10)) +
  scale_color_manual(values = c("criança" = "#020305", 
                                "adolescente" = "#ec225e",
                                "adulto" = "#269199",
                                "idoso" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf2

## Teste de log-rank para faixa etária
logrank1 <- survdiff(y~pneu$faixa_etaria, data = pneu)
logrank1

## Rodando o modelo km estratificado por raça/cor/etnia
km2 <- survfit(y~pneu$raca, data = pneu)

## Função de aalen segunda etnia
aalen2 <- survfit(y~strata(pneu$raca), data = pneu)

## Gráfico de sobrevivência segundo raça
graf3 <- ggsurvplot(km2, censor = F,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   title = "Sobrevivência segundo raça",
                   legend.labs = c("branco","pardo","negro","outros"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))


graf3 <- graf3$plot + 
  scale_x_continuous(breaks = seq(0,350,50)) +
  scale_y_continuous(breaks = seq(0,1,0.20)) +
  scale_color_manual(values = c("branco" = "#020305", 
                                "pardo" = "#ec225e",
                                "negro" = "#269199",
                                "outros" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf3

## Curva de risco acumulado segundo raça 
graf4 <- ggsurvplot(aalen2,data = pneu, fun = "cumhaz", censor = F,
                    xlab = "Tempo (dias)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    title = "Risco acumulado segundo raça",
                    legend.labs = c("branco","pardo","negro","outros"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))


graf4 <- graf4$plot + 
  scale_x_continuous(breaks = seq(0,350,50)) +
  scale_y_continuous(breaks = seq(0,8,1),
                     limits = c(0,8)) +
  scale_color_manual(values = c("branco" = "#020305", 
                                "pardo" = "#ec225e",
                                "negro" = "#269199",
                                "outros" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf4

## Teste de log-rank para raça
logrank2 <- survdiff(y~pneu$raca, data = pneu)
logrank2

## Terminação
