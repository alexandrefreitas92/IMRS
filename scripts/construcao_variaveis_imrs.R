# Informação Geral --------------------------------------------------------

# Script de atualização automática das informações do IMRS da área de Assistência Social
# Alexandre Freitas
# Email: alexandre.freitas@fjp.mg.gov.br
# Data de construção: Julho e agosto de 2021.

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(readxl)
library(abjutils)

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

# Juntar bases
trabalhadores <- bind_rows(cras_var_rh, creas_var_rh, gestao_mun_var_rh)

trabalhadores <- trabalhadores %>%
  mutate_if(is.character, str_trim) %>%
  mutate_if(is.character, str_to_upper) %>%
  mutate_if(is.character, rm_accent)

# Testar consistência das variaveis
vinculo <- trabalhadores %>%
  count(vinculo)

funcao <- trabalhadores %>%
  count(funcao)

profissao <- trabalhadores %>%
  count(profissao)

nivel_escolaridade <- trabalhadores %>%
  count(niveis_escolaridade)

escolaridade <- trabalhadores %>%
  count(escolaridade)

# Corrigir inconsistencias
trabalhadores <- trabalhadores %>%
  mutate(vinculo = case_when(vinculo %in% c("COMISSIONADA(O)", "COMISSIONADO") ~ "COMISSIONADA(O)",
                             vinculo %in% c("EMPREGADA(O) PUBLICA(O) (CLT)", "EMPREGADO PUBLICO CELETISTA (CLT)") ~ "EMPREGADA(O) PUBLICA(O) (CLT)",
                             vinculo %in% c("SERVIDOR TEMPORARIO", "SERVIDOR(A) TEMPORARIA(O)") ~ "SERVIDOR(A) TEMPORARIA(O)",
                             vinculo %in% c("SERVIDOR ESTATUTARIO", "SERVIDOR(A)/ESTATUTARIA(O)") ~ "SERVIDOR(A)/ESTATUTARIO(A)",
                             vinculo %in% c("TERCEIRIZADA(O)", "TERCEIRIZADO") ~ "TERCEIRIZADA(O)",
                             vinculo %in% c("TRABALHADOR DE EMPRESA/COOPERATIVA/ENTIDADE PRESTADORA DE SERVICOS", "TRABALHADOR(A) DE EMPRESA, COOPERATIVA OU ENTIDADE PRESTADORA DE SERVICOS",
                                            "TRABALHADOR(A) DE EMPRESA/ COOPERATIVA/ ENTIDADE PRESTADORA DE SERVICOS") ~ "TRABALHADOR(A) DE EMPRESA, COOPERATIVA OU ENTIDADE PRESTADORA DE SERVICOS",
                             vinculo %in% c("VOLUNTARIA(O)", "VOLUNTARIO") ~ "VOLUNTARIA(O)",
                             TRUE ~ vinculo
                             ),
         funcao = case_when(funcao %in% c("ESTAGIARIA(O)", "ESTAGIARIO(A)") ~ "ESTAGIARIA(O)",
                            funcao %in% c("COORDENADOR(A)" , "COORDENADOR(A)/DIRIGENTE") ~ "COORDENADOR(A)/DIRIGENTE",
                            funcao %in% c("EDUCADOR(A) SOCIAL", "EDUCADOR(A)/ORIENTADOR(A) SOCIAL") ~ "EDUCADOR(A)/ORIENTADOR(A) SOCIAL",
                            str_detect(funcao, "SERVICOS GERAIS") ~ "SERVIÇOS GERAIS",
                            str_detect(funcao, "NIVEL MEDIO") ~ "TECNICA(O) DE NIVEL MEDIO",
                            str_detect(funcao, "NIVEL SUPERIOR") ~ "TECNICA(O) DE NIVEL SUPERIOR",
                            TRUE ~ funcao
                            ),
         escolaridade = str_remove_all(escolaridade, "ENSINO ")
         )

rm(vinculo, funcao, profissao, nivel_escolaridade, escolaridade)

# * Resumo RH -------------------------------------------------------------

trabalhadores %>%
  filter(profissao == "ASSISTENTE SOCIAL") %>%
  count(niveis)

rh_sintese <- trabalhadores %>%
  group_by(IBGE7) %>%
  summarise(B_RHTOTALA = n(), # Número de funcionários da Assistência Social (com estagiário)
            B_RHTOTALB = sum(funcao != "ESTAGIARIA(O)"), # Número de funcionários da Assistência Social (sem estagiário)
            B_RHAS = sum(profissao == "ASSISTENTE SOCIAL"), # Número de assistentes sociais atuando na Assistência Social 
            B_RHASPS = sum(profissao == "ASSISTENTE SOCIAL") / sum(niveis_escolaridade == "NIVEL SUPERIOR") * 100, # Percentual de assistentes sociais atuando na Assistência Social em relação ao total de pessoal de nível superior 
            B_RHPSI = sum(profissao == "PSICOLOGA(O)"), # Número de psicólogos atuando na Assistência Social 
            B_RHPSIPS = sum(profissao == "PSICOLOGA(O)") / sum(niveis_escolaridade == "NIVEL SUPERIOR") * 100, # Percentual de psicólogos atuando na Assistência Social em relação ao total de pessoal de nível superior 
            B_RHENME = sum(niveis_escolaridade == "NIVEL MEDIO"), # Número de funcionários com ensino médio ocupados na Assistência Social 
            B_RHCURSU = sum(niveis_escolaridade == "NIVEL SUPERIOR"), # Número de funcionários com curso superior ocupados na Assistência Social
            B_RHPOSGRA = sum(escolaridade %in% c("ESPECIALIZACAO", "MESTRADO", "DOUTORADO")), # Número de funcionários com pós-graduação ocupados na Assistência Social
            B_RHTOEST = sum(vinculo == "SERVIDOR(A)/ESTATUTARIO(A)"), # Número de funcionários  estatutários ocupados na Assistência Social 
            B_RHTOCEL = sum(vinculo == "EMPREGADA(O) PUBLICA(O) (CLT)"), # Número de funcionários celetistas ocupados na assistência Social 
            B_RHVPOAAS = sum(vinculo %in% c("SERVIDOR(A)/ESTATUTARIO(A)", "EMPREGADA(O) PUBLICA(O) (CLT)")) / n() * 100, # Percentual do pessoal estatutário e celestita ocupado na área de Assistência Social
            B_RHVINEST = sum(vinculo == "SERVIDOR(A)/ESTATUTARIO(A)") / n() * 100, # Percentual do pessoal estatutário ocupado na área de Assistência Social
     #       B_RHTPOASPH = # Proporção de pessoal ocupado na área de Assistência Social por 10 mil habitantes
            )
#
#
#
#creas_rh %>%
#  count(q58_8)

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
