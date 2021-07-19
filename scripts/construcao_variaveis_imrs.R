# Informação Geral --------------------------------------------------------

# Script de atualização automática das informações do IMRS da área de Assistência Social
# Alexandre Freitas
# Email: alexandre.freitas@fjp.mg.gov.br
# Data de construção: Julho e agosto de 2021.

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(readxl)

# Ler base de dados -------------------------------------------------------
rm(list=ls())

## CRAS
cras_rh <- read.xlsx("data/CRAS/Censo_SUAS_2019_CRAS_RH_divulgacao.xlsx") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

cras_geral <- read_xls("data/CRAS/Censo_SUAS_2019_dados_gerais_RH_CRAS_divulgacao.xls") %>%
  left_join(unique(cras_rh[, c("IBGE","IBGE7")]), by = "IBGE") %>%
  filter(!is.na(IBGE7))

### Dicionario das variaveis dos df do cras geral e RH
dicionario_cras_geral <- read.xlsx("data/CRAS/Dicionario_Censo_Suas_2019_CRAS.xlsx", sheet = 1)
dicionario_cras_rh <- read.xlsx("data/CRAS/Dicionario_Censo_Suas_2019_CRAS.xlsx", sheet = 2)

## CREAS
creas_geral <-  read_csv2("data/CREAS/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

creas_rh <- read.xlsx("data/CREAS/Censo_SUAS_2019_RH_CREAS_divulgacao.xlsx") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")


# Construcao das variaveis do IMRS ----------------------------------------

imrs_cras_geral <- cras_geral %>%
  group_by(IBGE7) %>%
  summarise(B_NCRAS = n())

imrs_cras_rh <- cras_rh %>%
  group_by(IBGE7) %>%
  summarise(B_RHTOTALA = n(),
            B_RHTOTALB = sum(q56_12 != "Estagiário(a)"),
            B_RHAS = sum(q56_10 == "Assistente Social"),
            B_RHASPS = sum(q56_10 == "Assistente Social") / sum(d_56_9 == "Nível Superior") * 100,
            B_RHPSI = sum(q56_10 == "Psicóloga(o)"),
            B_RHPSIPS = sum(q56_10 == "Psicóloga(o)") / sum(d_56_9 == "Nível Superior") * 100,
            B_RHENME = sum(d_56_9 == "Nível Médio"),
            B_RHCURSU = sum(d_56_9 == "Nível Superior"),
            B_RHPOSGRA = sum(q56_9 %in% c("Especialização", "Mestrado", "Doutorado")),
            B_RHTOEST = sum(q56_11 == "Servidor Estatutário"),
            B_RHTOCEL = sum(q56_11 == "Empregado Público Celetista (CLT)"),
            B_RHVPOAAS = sum(q56_11 %in% c("Servidor Estatutário", "Empregado Público Celetista (CLT)")) / n() * 100,
            B_RHVINEST = sum(q56_11 == "Servidor Estatutário") / n() * 100
            )

count(cras_rh, q56_11)
# OK Número de funcionários da Assistência Social (com estagiário)
# OK Número de funcionários da Assistência Social (sem estagiário)
# OK Número de assistentes sociais atuando na Assistência Social 
# OK Percentual de assistentes sociais atuando na Assistência Social em relação ao total de pessoal de nível superior 
# OK Número de psicólogos atuando na Assistência Social 
# OK Percentual de psicólogos atuando na Assistência Social em relação ao total de pessoal de nível superior 
# OK Número de funcionários com ensino médio ocupados na Assistência Social 
# OK Número de funcionários com curso superior ocupados na Assistência Social
# OK Número de funcionários com pós-graduação ocupados na Assistência Social
# OK Número de funcionários estatutários ocupados na Assistência Social 
# OK Número de funcionários celetistas ocupados na assistência Social 
# Grau de instrução do pessoal ocupado na área de Assistência Social
# OK Percentual do pessoal estatutário e celestita ocupado na área de Assistência Social
# OK Percentual do pessoal estatutário ocupado na área de Assistência Social

# B_RHTOTALA
# B_RHTOTALB
# B_RHAS
# B_RHASPS
# B_RHPSI
# B_RHPSIPS
# B_RHENME
# B_RHCURSU
# B_RHPOSGRA
# B_RHTOEST
# B_RHTOCEL
# B_RHGIPOAS
# B_RHVPOAAS
# B_RHVINEST
