# Carregamento dos pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, readxl, survival,
               ggfortify, survminer, ggsurvfit)



# Leitura do banco
## Créditos da base: diabetic (do pacote survival)
## ?survival::diabetic

dados <- readxl::read_excel("Análise sobrevivência.xlsx")

glimpse(dados)

dados$Tempo <- as.numeric(difftime(dados$`Data Final`, dados$`Data Inicial`,
                                   units = "days"))

dados[,c(2,4,5)] <- lapply(dados[,c(2,4,5)], factor)


######################## Análise de sobrevivência #############################


# Curva de sobrevivência para toda a amostra

## Estimador Kaplan-Meier

ekm <- survival::survfit(Surv(Tempo, Status) ~ 1, data = dados)

ekm
summary(ekm)


## Curva - opção 1 (ggfortify)

autoplot(ekm, censor = T, surv.colour = "cadetblue",
         censor.colour = "cadetblue") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 2250, by = 250)) +
  labs(y = "Probabilidade de sobrevivência", x = "Tempo (dias)") +
  theme_bw()



## Curva - opção 2 (survminer)
## https://rpkgs.datanovia.com/survminer/

ggsurvplot(ekm,
           conf.int = T,
           break.time.by = 250,
           xlab = "Tempo (dias)",
           ylab = "Probabilidade de sobrevivência",
           legend.title = "",
           legend.labs = "Total",
           surv.median.line = "hv",
           risk.table = "abs_pct",
           risk.table.title = "Quantidade em risco - n (%)",
           risk.table.fontsize = 3,
           risk.table.y.text = F,
           ggtheme = theme_bw())



## Curva - opção 3 (ggsurvfit)
## https://www.danieldsjoberg.com/ggsurvfit/articles/gallery.html

ggsurvfit(ekm, color = "cadetblue") +
  add_censor_mark(color = "cadetblue") +
  add_confidence_interval(fill = "cadetblue") +
  add_quantile(y_value = 0.5, linetype = "dashed", color = "grey30") + 
  labs(x = "Tempo (dias)", y = "Probabilidade de sobrevivência") +
  scale_ggsurvfit(x_scales = list(breaks = seq(0, 2250, by = 250))) +
  add_risktable(stats_label = c("Em risco", "Eventos")) +
  theme(axis.title = element_text(size = 11),
        axis.title.x = element_text(margin = margin(5,5,15,5)))



# Curva de acordo com uma variável + comparação entre os grupos por log-rank

ekm_trt <- survival::survfit(Surv(Tempo, Status) ~ Tratamento,
                             data = dados)

ekm_trt
summary(ekm_trt)


summary(ekm_trt, times = c(500, 1000))


survdiff(Surv(Tempo, Status) ~ Tratamento, data = dados, rho = 0)
# rho = 0 -> equivale ao teste log-rank (= teste Mantel-Haenszel)


## Curva - opção 1 (ggfortify)

autoplot(ekm_trt, censor = T, surv.colour = "strata",
         censor.colour = "strata") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 2250, by = 500)) +
  labs(y = "Probabilidade de sobrevivência", x = "Tempo (dias)",
       color = "Tratamento", fill = "Tratamento") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))



## Curva - opção 2 (survminer)
## https://rpkgs.datanovia.com/survminer/

ggsurvplot(ekm_trt,
           conf.int = T,
           pval = T,
           break.time.by = 250,
           xlab = "Tempo (dias)",
           ylab = "Probabilidade de sobrevivência",
           legend.title = "",
           surv.median.line = "hv",
           risk.table = "abs_pct",
           risk.table.title = "Quantidade em risco - n (%)",
           risk.table.fontsize = 3,
           risk.table.y.text = F,
           ggtheme = theme_bw())


## Curva - opção 3 (ggsurvfit)
## https://www.danieldsjoberg.com/ggsurvfit/articles/gallery.html

ggsurvfit(ekm_trt) +
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile(y_value = 0.5, linetype = "dashed", color = "grey30") + 
  labs(x = "Tempo (dias)", y = "Probabilidade de sobrevivência") +
  scale_ggsurvfit(x_scales = list(breaks = seq(0, 2250, by = 250))) +
  add_risktable(stats_label = c("Em risco", "Eventos")) +
  theme(axis.title = element_text(size = 11),
        axis.title.x = element_text(margin = margin(5,5,15,5)))


