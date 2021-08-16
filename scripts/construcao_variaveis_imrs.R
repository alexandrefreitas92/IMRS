# Informação Geral --------------------------------------------------------

# Script de atualização automática das informações do IMRS da área de Assistência Social
# Alexandre Freitas
# Email: alexandre.freitas@fjp.mg.gov.br
# Data de construção: Julho e agosto de 2021.

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(readxl)

# CRAS, CREAS, CONSELHO E GESTÃO

# Ler base de dados -------------------------------------------------------
rm(list=ls())

## CRAS
cras_rh <- read.xlsx("data/CRAS/Censo_SUAS_2019_CRAS_RH_divulgacao.xlsx") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

cras_geral <- read_xls("data/CRAS/Censo_SUAS_2019_dados_gerais_RH_CRAS_divulgacao.xls") %>%
  left_join(unique(cras_rh[, c("IBGE","IBGE7")]), by = "IBGE") %>%
  filter(!is.na(IBGE7))

## CREAS
creas_geral <-  read_csv2("data/CREAS/Censo_SUAS_2019_CREAS_Dados_Gerais_divulgacao.csv") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

creas_rh <- read.xlsx("data/CREAS/Censo_SUAS_2019_RH_CREAS_divulgacao.xlsx") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

## Gestao Municipal
gestao_mun_geral <- read_csv2("data/Gestão Municipal/Censo_SUAS_2019_Gestao_Municipal_dados_gerais_divulgacao.csv") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

gestao_mun_rh <- read.xlsx("data/Gestão Municipal/Censo_SUAS_2019_Gestao_Municipal_RH_divulgacao.xlsx") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

## Conselho Municipal
conselho_mun_geral <- read.csv("data/Conselho Municipal/Censo_SUAS_2019_Dados_gerais_RH_Conselho_Municipal_divulgacao.csv", 
                               sep = ";", header = TRUE,
                               encoding = "latin1") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")

conselho_mun_rh <- read.csv("data/Conselho Municipal/Censo_Suas_2019_RH_Conselho_Municipal_divulgacao.csv", 
                            sep = ";", header = TRUE,
                            encoding = "latin1") %>%
  filter(str_sub(IBGE7, 1, 2) == "31")


# Construcao das variaveis do IMRS ----------------------------------------


# * Selecao variaveis -------------------------------------------------------

# CRAS
cras_var_rh <- cras_rh %>%
  select(IBGE7, q56_11, q56_12, q56_10, d_56_9, q56_9) %>%
  rename("vinculo" = q56_11,
         "funcao" = q56_12,
         "profissao" = q56_10,
         "niveis_escolaridade" = d_56_9,
         "escolaridade" = q56_9) %>%
  mutate(base = "CRAS")

# CREAS
creas_var_rh <- creas_rh %>% 
  select(IBGE7, q58_10, q58_11, q58_9, d_58_8, q58_8) %>%
  rename('vinculo' = q58_10,
         "funcao" = q58_11,
         "profissao" = q58_9,
         "niveis_escolaridade" = d_58_8,
         "escolaridade" = q58_8) %>%
  mutate(base = "CREAS")

# Gestao Municipal
gestao_mun_var_rh <- gestao_mun_rh %>%
  select(IBGE7, q65_11, q65_12, q65_10, d_65_9, q65_9) %>%
  rename('vinculo' = q65_11,
         "funcao" = q65_12,
         "profissao" = q65_10,
         "niveis_escolaridade" = d_65_9,
         "escolaridade" = q65_9) %>%
  mutate(base = "Gestão Municipal")

# Conselho
conselho_var_rh

# Juntar bases
trabalhadores <- bind_rows(cras_var_rh, creas_var_rh, gestao_mun_var_rh)

trabalhadores <- trabalhadores %>%
  mutate_if(is.character, str_trim) %>%
  mutate_if(is.character, str_to_upper)

# Testar consistência das variaveis
trabalhadores %>%
  count(vinculo)

trabalhadores %>%
  count(funcao)

trabalhadores_profissao <- trabalhadores %>%
  count(profissao)

trabalhadores %>%
  count(niveis_escolaridade)

trabalhadores %>%
  count(escolaridade)

#######################################################
################################
#
# Parei na parte que corrige as variáveis para juntar!
#
###############################
#######################################################

# * Resumo RH -------------------------------------------------------------

rh_sintese <- trabalhadores %>%
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



creas_rh %>%
  count(q58_8)

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
