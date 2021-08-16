﻿
* Encoding: UTF-8.

**Criação de Derivadas Gestão Municipal - RH - CENSO 2019**



****criar variavel data Censo

STRING  DataCenso (A10). 
COMPUTE DataCenso="31.08.2019". 
VARIABLE LABELS  DataCenso 'COMPUTE DataCenso="31.08.2019"'. 
EXECUTE.


***transformar variavel data string em date

* Date and Time Wizard: DataCenso_2019.
COMPUTE DataCenso_2019=number(DataCenso, EDATE10).
VARIABLE LABELS DataCenso_2019 "DataCenso recodificada".
VARIABLE LEVEL  DataCenso_2019 (SCALE).
FORMATS DataCenso_2019 (EDATE10).
VARIABLE WIDTH  DataCenso_2019(10).
EXECUTE.

******transfromar data nascimento string em date

COMPUTE q65_2_rec=number(q65_2, EDATE10).
VARIABLE LABELS q65_2_rec  "q65_2_data nascimento recodificada".
VARIABLE LEVEL  q65_2_rec (SCALE).
FORMATS q65_2_rec (EDATE10).
VARIABLE WIDTH  q65_2_rec(10).
EXECUTE.


**transformar em data**

COMPUTE  Idade=DATEDIF(DataCenso_2019, q65_2_rec, "years"). 
VARIABLE LABELS  Idade "Idade". 
VARIABLE LEVEL  Idade (SCALE). 
FORMATS  Idade (F5.0). 
VARIABLE WIDTH  Idade(5). 
EXECUTE.  
FREQUENCIES VARIABLES=Idade 
  /STATISTICS=RANGE MINIMUM MAXIMUM STDDEV MEAN MEDIAN 
  /FORMAT=LIMIT(50) 
  /ORDER=ANALYSIS.



RECODE Idade (16 thru 17=1) (18 thru 29=2) (30 thru 40=3) (41 thru 50=4) (51 thru 60=5) (61 thru 100=6) INTO 
    Faixas_etarias.
VARIABLE LABELS  Faixas_etarias 'Faixas etárias dos trabalhadores Gestão Municipal'.
EXECUTE.

VALUE LABELS
/Faixas_etarias 
1 '16 a 17 anos'
2 '18 a 29 anos'
3 '30 a 40 anos'
4 '41 a 50 anos'
5 '51 a 60 anos' 
6 'Mais de 60 anos'
.



****variáveis binárias escolaridade***

RECODE q65_9 (251 thru 254=0) (255 thru 256=1) (257 thru 261=2) INTO d_65_9.

VARIABLE LABELS d_65_9  'd_65_9_Níveis de escolaridade'.
EXECUTE.

VALUE LABELS d_65_9
0 'Nível fundamental'
1 'Nível Médio'
2 'Nível Superior'.
execute.

RECODE d_65_9 (0=1) INTO d_65_9bin1.

VARIABLE LABELS  d_65_9bin1  'd_65_9bin1_Nível fundamental - Trabalhadores da gestão que possuem até o nível médio (inclui aqueles sem escolaridade)'.
EXECUTE.

VALUE LABELS d_65_9bin1
1 'Nível fundamental'.
EXECUTE.

RECODE d_65_9  (1=1) INTO d_65_9bin2.
VARIABLE LABELS d_65_9bin2  'd_65_9bin2_Nível médio - Trabalhadores da gestão com ensino superior incompleto e ensino médio completo'.
EXECUTE.

VALUE LABELS d_65_9bin2
1 'Nível Médio'.
EXECUTE.

RECODE d_65_9 (2=1) INTO d_65_9bin3.
VARIABLE LABELS  d_65_9bin3  'd_65_9bin3_Nível superior - inclui Trabalhadores da gestão graduados, com especialização, mestrado ou doutorado'.
EXECUTE.

VALUE LABELS  d_65_9bin3
1 'Nível Superior'.
EXECUTE.


****variáveis binárias profissão****

RECODE q65_10  (275=1) (273=2) (234=3) (232=4)(274=5) (238=6) (238=7)(else=8) INTO d_65_10.
VARIABLE LABELS   d_65_10  'd_65_10_Profissão'.

VALUE LABELS d_65_10
1 'Pedagogo'
2 'Assistente Social'
3 'Antropólogo'
4 'Advogado'
5 'Psicólogo'
6 'Profissional de nível médio'
7 'Sem formação profissional'
8 'Outro profissional de nivel superior'.
EXECUTE.




RECODE q65_10 (275=1) INTO d_65_10bin1.
VARIABLE LABELS d_65_10bin1   'd_65_10bin1_Pedagogo'.
EXECUTE.

VALUE LABELS d_65_10bin1
1 'Pedagogo'.
EXECUTE.

RECODE q65_10  (273=1) INTO d_65_10bin2.
VARIABLE LABELS  d_65_10bin2  'd_65_10bin2_Assistente Social'.
EXECUTE.

VALUE LABELS  d_65_10bin2
1 'Assistente Social'.
EXECUTE.

RECODE q65_10   (234=1) INTO d_65_10bin3.
VARIABLE LABELS  d_65_10bin3  'd_65_10bin3_Antropólogo'.
EXECUTE.

VALUE LABELS  d_65_10bin3
1 'Antropólogo'.
EXECUTE.

RECODE q65_10   (232=1) INTO d_65_10bin4.
VARIABLE LABELS  d_65_10bin4    'd_65_10bin4_Advogado'.
EXECUTE.

VALUE LABELS  d_65_10bin4
1 'Advogado'.
EXECUTE.

RECODE q65_10  (274=1) INTO d_65_10bin5.
VARIABLE LABELS  d_65_10bin5   'd_65_10bin5_Psicologo'.
EXECUTE.

VALUE LABELS  d_65_10bin5
1 'Psicólogo'.
EXECUTE.



**** variáveis binárias - tipo de vínculo****


RECODE q65_11  (138=1) (139=2)(292=3) (else=4) INTO d_65_11.
VARIABLE LABELS   d_65_11  'd_65_11_Tipo de vínculo'.

VALUE LABELS d_65_11
1 'Servidor Estatutário'
2 'Empregado Público (CLT)'
3 'Comissionado'
4 'Outros vínculos não permanentes'.
EXECUTE.




RECODE  d_65_11  (1=1) INTO d_65_11bin1.
VARIABLE LABELS   d_65_11bin1  'd_65_11bin1_Servidor Estatutário'.

VALUE LABELS d_65_11bin1
1 'Servidor Estatutário'.
EXECUTE.

RECODE d_65_11   (2=1) INTO d_65_11bin2.
VARIABLE LABELS  d_65_11bin2  ' d_65_11bin2_Empregado Público (CLT)'.

VALUE LABELS  d_65_11bin2
1 'Empregado Público (CLT)'.
EXECUTE.

RECODE d_65_11 (3=1) INTO d_65_11bin3.
VARIABLE LABELS d_65_11bin3    'd_65_11bin3_Comissionado'.

VALUE LABELS  d_65_11bin3
1 'Comissionado'.
EXECUTE.

RECODE d_65_11 (4=1) INTO d_65_11bin4.
VARIABLE LABELS d_65_11bin4   'd_65_11bin4_Outros vínculos não permanentes - inclui Trabalhador de Empresas/Cooperativa/Entidade Prestadora de Serviços; Voluntários; Servidor Temporário; Sem vínculo; Terceirizado e Outro vínculo não permanente'.

VALUE LABELS  d_65_11bin4
1 'Outros vínculos não permanentes'.
EXECUTE.


*******************AGGREGATE VARIÁVEIS BINÁRIAS PARA MERGE*************************



AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d_65_9bin1_sum=SUM(d_65_9bin1) 
  /d_65_9bin2_sum=SUM(d_65_9bin2) 
  /d_65_9bin3_sum=SUM(d_65_9bin3) 
  /d_65_10bin1_sum=SUM(d_65_10bin1) 
  /d_65_10bin2_sum=SUM(d_65_10bin2) 
  /d_65_10bin3_sum=SUM(d_65_10bin3) 
  /d_65_10bin4_sum=SUM(d_65_10bin4) 
  /d_65_10bin5_sum=SUM(d_65_10bin5) 
  /d_65_11bin1_sum=SUM(d_65_11bin1) 
  /d_65_11bin2_sum=SUM(d_65_11bin2) 
  /d_65_11bin3_sum=SUM(d_65_11bin3) 
  /d_65_11bin4_sum=SUM(d_65_11bin4)
  /N_BREAK=N.


VARIABLE LABELS
 d_65_9bin1_sum 'Número de trabalhadores de Nível fundamental (inclui trabalhadores sem escolaridade, fundamental incompleto e ensino médio incompleto)'
 d_65_9bin2_sum 'Número de trabalhadores de Nível médio (inclui trabalhadores com ensino superior incompleto)'
 d_65_9bin3_sum 'Número de trabalhadores de Nível superior (inclui trabalhadores com especialização, mestrado e doutorado)'
 d_65_10bin1_sum 'Número de Pedagogos'
 d_65_10bin2_sum 'Número de Assistentes Sociais'
 d_65_10bin3_sum 'Número de Antropólogos'
 d_65_10bin4_sum 'Número de Advogados'
 d_65_10bin5_sum 'Número de Psicólogos'
 d_65_11bin1_sum 'Número de Servidores Estatutários'
 d_65_11bin2_sum 'Número de Empregados Públicos(CLT)'
 d_65_11bin3_sum 'Número de Comissionados'
 d_65_11bin4_sum 'Número de profissionais com outros vínculos não permanentes'
 N_BREAK 'Número de trabalhadores na GESTAO'
.
EXECUTE.



*** RECODE MISSING TO ZERO ****

DATASET ACTIVATE agrr.


RECODE d_65_9bin1_sum  d_65_9bin2_sum d_65_9bin3_sum d_65_10bin1_sum d_65_10bin2_sum d_65_10bin3_sum  d_65_10bin4_sum  d_65_10bin5_sum  d_65_11bin1_sum  d_65_11bin2_sum  d_65_11bin3_sum  d_65_11bin4_sum   nu_trabalhador (SYSMIS=0).
EXECUTE.

VARIABLE LABELS
 d_65_9bin1_sum  'd_65_9bin1_Número de trabalhadores de Nível fundamental (inclui trabalhadores sem escolaridade, fundamental incompleto e ensino médio incompleto)'
 d_65_9bin2_sum 'd_65_9bin2_Número de trabalhadores de Nível médio (inclui trabalhadores com ensino superior incompleto)'
 d_65_9bin3_sum 'd_65_9bin3_Número de trabalhadores de Nível superior (inclui trabalhadores com especialização, mestrado e doutorado)'.
EXECUTE.

VARIABLE LABELS
d_65_10bin1_sum 'd_65_10bin1_Número de Pedagogos'
 d_65_10bin2_sum 'd_65_10bin2_Número de Assistentes Sociais'
d_65_10bin3_sum 'd_65_10bin3_Número de Antropólogos'
 d_65_10bin4_sum 'd_65_10bin4_Número de Advogados'
 d_65_10bin5_sum 'd_65_10bin5_Número de Psicólogos'.
EXECUTE.

 VARIABLE LABELS
d_65_11bin1_sum 'd_65_11bin1_Número de Servidores Estatutários'
d_65_11bin2_sum 'd_65_11bin2_Número de Empregados Públicos(CLT)'
 d_65_11bin3_sum 'd_65_11bin3_Número de Comissionados'
 d_65_11bin4_sum  'd_65_11bin4_Número de profissionais com outros vínculos não permanentes'.
EXECUTE.



** variáveis binárias - Serviços****

**** Principal serviço/atividade  1   


RECODE q65_15 (1=1) INTO d_65_15_1bin_Gestão.
VARIABLE LABELS  d_65_15_1bin_Gestão  'd_65_15_1bin_Gestão - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Gestão
1 'Gestão'.
EXECUTE.

RECODE  q65_15  (2=1) INTO d_65_15_1bin_Gestão_do_SUAS.
VARIABLE LABELS   d_65_15_1bin_Gestão_do_SUAS  'd_65_15_1bin_Gestão do SUAS - Principal serviço/atividade'.

VALUE LABELS d_65_15_1bin_Gestão_do_SUAS
1 'Gestão do SUAS'.
EXECUTE.

RECODE   q65_15 (3=1) INTO d_65_15_1bin_Vigilância_Socioassistencial.
VARIABLE LABELS   d_65_15_1bin_Vigilância_Socioassistencial  'd_65_15_1bin_Vigilância Socioassistencial - Principal serviço/atividade'.

VALUE LABELS d_65_15_1bin_Vigilância_Socioassistencial
1 'Vigilância Socioassistencial'.
EXECUTE.

RECODE q65_15  (4=1) INTO d_65_15_1bin_Gestão_do_Trabalho.
VARIABLE LABELS   d_65_15_1bin_Gestão_do_Trabalho ' d_65_15_1bin_Gestão do Trabalho- Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Gestão_do_Trabalho
1 'Gestão do Trabalho'.
EXECUTE.

RECODE  q65_15  (5=1) INTO d_65_15_1bin_Regulação_do_SUAS.
VARIABLE LABELS  d_65_15_1bin_Regulação_do_SUAS  'd_65_15_1bin_Regulação do SUAS - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Regulação_do_SUAS
1 'Regulação do SUAS'.
EXECUTE.

RECODE  q65_15  (6=1) INTO d_65_15_1bin_Gestão_Financeira_Orçamentária.
VARIABLE LABELS d_65_15_1bin_Gestão_Financeira_Orçamentária   'd_65_15_1bin_Gestão Financeira e Orçamentária - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Gestão_Financeira_Orçamentária
1 'Gestão Financeira e Orçamentária'.
EXECUTE.

RECODE  q65_15  (7=1) INTO d_65_15_1bin_PSB.
VARIABLE LABELS  d_65_15_1bin_PSB   'd_65_15_1bin_PSB - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_PSB
1 'Proteção Social Básica'.
EXECUTE.

RECODE  q65_15  (8=1) INTO d_65_15_1bin_PSE_Média_Complexidade.
VARIABLE LABELS  d_65_15_1bin_PSE_Média_Complexidade   'd_65_15_1bin_PSE Média Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_PSE_Média_Complexidade
1 'PSE Média Complexidade'.
EXECUTE.

RECODE  q65_15  (9=1) INTO d_65_15_1bin_PSE_Alta_Complexidade.
VARIABLE LABELS  d_65_15_1bin_PSE_Alta_Complexidade   'd_65_15_1bin_PSE Alta Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_PSE_Alta_Complexidade
1 'PSE Alta Complexidade'.
EXECUTE.

RECODE  q65_15  (10=1) INTO d_65_15_1bin_Gestão_CadÚnico_PBF.
VARIABLE LABELS  d_65_15_1bin_Gestão_CadÚnico_PBF   'd_65_15_1bin_Gestão do Cadastro Único e Programa Bolsa Família - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Gestão_CadÚnico_PBF
1 'Gestão do Cadastro Único e Programa Bolsa Família'.
EXECUTE.

RECODE  q65_15  (11=1) INTO d_65_15_1bin_Gestão_Benefícios_Assistenciais.
VARIABLE LABELS  d_65_15_1bin_Gestão_Benefícios_Assistenciais   'd_65_15_1bin_Gestão de Benefícios Assistenciais - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Gestão_Benefícios_Assistenciais
1 'Gestão de Benefícios Assistenciais (BPC, benefícios eventuais)'.
EXECUTE.

RECODE  q65_15  (12=1) INTO d_65_15_1bin_Outros.
VARIABLE LABELS  d_65_15_1bin_Outros  'd_65_15_1bin_Outros - Principal serviço/atividade'.

VALUE LABELS  d_65_15_1bin_Outros
1 'Outros'.
EXECUTE.

**** Segundo principal serviço/atividade  2

RECODE q65_16 (1=1) INTO d_65_16_2bin_Gestão.
VARIABLE LABELS  d_65_16_2bin_Gestão  'd_65_16_2bin_Gestão - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Gestão
1 'Gestão'.
EXECUTE.

RECODE  q65_16  (2=1) INTO d_65_16_2bin_Gestão_do_SUAS.
VARIABLE LABELS   d_65_16_2bin_Gestão_do_SUAS  'd_65_16_2bin_Gestão do SUAS - Principal serviço/atividade'.

VALUE LABELS d_65_16_2bin_Gestão_do_SUAS
1 'Gestão do SUAS'.
EXECUTE.

RECODE   q65_16 (3=1) INTO d_65_16_2bin_Vigilância_Socioassistencial.
VARIABLE LABELS   d_65_16_2bin_Vigilância_Socioassistencial  'd_65_16_2bin_Vigilância Socioassistencial - Principal serviço/atividade'.

VALUE LABELS d_65_16_2bin_Vigilância_Socioassistencial
1 'Vigilância Socioassistencial'.
EXECUTE.

RECODE q65_15  (4=1) INTO d_65_16_2bin_Gestão_do_Trabalho.
VARIABLE LABELS   d_65_16_2bin_Gestão_do_Trabalho ' d_65_16_2bin_Gestão do Trabalho- Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Gestão_do_Trabalho
1 'Gestão do Trabalho'.
EXECUTE.

RECODE  q65_15  (5=1) INTO d_65_16_2bin_Regulação_do_SUAS.
VARIABLE LABELS  d_65_16_2bin_Regulação_do_SUAS  'd_65_16_2bin_Regulação do SUAS - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Regulação_do_SUAS
1 'Regulação do SUAS'.
EXECUTE.

RECODE  q65_15  (6=1) INTO d_65_16_2bin_Gestão_Financeira_Orçamentária.
VARIABLE LABELS d_65_16_2bin_Gestão_Financeira_Orçamentária   'd_65_16_2bin_Gestão Financeira e Orçamentária - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Gestão_Financeira_Orçamentária
1 'Gestão Financeira e Orçamentária'.
EXECUTE.

RECODE  q65_16  (7=1) INTO d_65_16_2bin_PSB.
VARIABLE LABELS  d_65_16_2bin_PSB   'd_65_16_2bin_PSB - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_PSB
1 'Proteção Social Básica'.
EXECUTE.

RECODE  q65_16  (8=1) INTO d_65_16_2bin_PSE_Média_Complexidade.
VARIABLE LABELS  d_65_16_2bin_PSE_Média_Complexidade   'd_65_16_2bin_PSE Média Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_PSE_Média_Complexidade
1 'PSE Média Complexidade'.
EXECUTE.

RECODE  q65_16  (9=1) INTO d_65_16_2bin_PSE_Alta_Complexidade.
VARIABLE LABELS  d_65_16_2bin_PSE_Alta_Complexidade   'd_65_16_2bin_PSE Alta Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_PSE_Alta_Complexidade
1 'PSE Alta Complexidade'.
EXECUTE.

RECODE  q65_16  (10=1) INTO d_65_16_2bin_Gestão_CadÚnico_PBF.
VARIABLE LABELS  d_65_16_2bin_Gestão_CadÚnico_PBF   'd_65_16_2bin_Gestão do Cadastro Único e Programa Bolsa Família - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Gestão_CadÚnico_PBF
1 'Gestão do Cadastro Único e Programa Bolsa Família'.
EXECUTE.

RECODE  q65_16  (11=1) INTO d_65_16_2bin_Gestão_Benefícios_Assistenciais.
VARIABLE LABELS  d_65_16_2bin_Gestão_Benefícios_Assistenciais   'd_65_16_2bin_Gestão de Benefícios Assistenciais - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Gestão_Benefícios_Assistenciais
1 'Gestão de Benefícios Assistenciais (BPC, benefícios eventuais)'.
EXECUTE.

RECODE  q65_16  (12=1) INTO d_65_16_2bin_Outros.
VARIABLE LABELS  d_65_16_2bin_Outros  'd_65_16_2bin_Outros - Principal serviço/atividade'.

VALUE LABELS  d_65_16_2bin_Outros
1 'Outros'.
EXECUTE.


***** Terceiro principal serviço/atividade  3

RECODE q65_17 (1=1) INTO d_65_17_3bin_Gestão.
VARIABLE LABELS  d_65_17_3bin_Gestão  'd_65_17_3bin_Gestão - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Gestão
1 'Gestão'.
EXECUTE.

RECODE  q65_17  (2=1) INTO d_65_17_3bin_Gestão_do_SUAS.
VARIABLE LABELS   d_65_17_3bin_Gestão_do_SUAS  'd_65_17_3bin_Gestão do SUAS - Principal serviço/atividade'.

VALUE LABELS d_65_17_3bin_Gestão_do_SUAS
1 'Gestão do SUAS'.
EXECUTE.

RECODE   q65_17 (3=1) INTO d_65_17_3bin_Vigilância_Socioassistencial.
VARIABLE LABELS   d_65_17_3bin_Vigilância_Socioassistencial  'd_65_17_3bin_Vigilância Socioassistencial - Principal serviço/atividade'.

VALUE LABELS d_65_17_3bin_Vigilância_Socioassistencial
1 'Vigilância Socioassistencial'.
EXECUTE.

RECODE q65_17  (4=1) INTO d_65_17_3bin_Gestão_do_Trabalho.
VARIABLE LABELS   d_65_17_3bin_Gestão_do_Trabalho ' d_65_17_3bin_Gestão do Trabalho- Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Gestão_do_Trabalho
1 'Gestão do Trabalho'.
EXECUTE.

RECODE  q65_17  (5=1) INTO d_65_17_3bin_Regulação_do_SUAS.
VARIABLE LABELS  d_65_17_3bin_Regulação_do_SUAS  'd_65_17_3bin_Regulação do SUAS - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Regulação_do_SUAS
1 'Regulação do SUAS'.
EXECUTE.

RECODE  q65_17  (6=1) INTO d_65_17_3bin_Gestão_Financeira_Orçamentária.
VARIABLE LABELS d_65_17_3bin_Gestão_Financeira_Orçamentária   'd_65_17_3bin_Gestão Financeira e Orçamentária - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Gestão_Financeira_Orçamentária
1 'Gestão Financeira e Orçamentária'.
EXECUTE.

RECODE  q65_17  (7=1) INTO d_65_17_3bin_PSB.
VARIABLE LABELS  d_65_17_3bin_PSB   'd_65_17_3bin_PSB - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_PSB
1 'Proteção Social Básica'.
EXECUTE.

RECODE  q65_17  (8=1) INTO d_65_17_3bin_PSE_Média_Complexidade.
VARIABLE LABELS  d_65_17_3bin_PSE_Média_Complexidade   'd_65_17_3bin_PSE Média Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_PSE_Média_Complexidade
1 'PSE Média Complexidade'.
EXECUTE.

RECODE  q65_17  (9=1) INTO d_65_17_3bin_PSE_Alta_Complexidade.
VARIABLE LABELS  d_65_17_3bin_PSE_Alta_Complexidade   'd_65_17_3bin_PSE Alta Complexidade - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_PSE_Alta_Complexidade
1 'PSE Alta Complexidade'.
EXECUTE.

RECODE  q65_17  (10=1) INTO d_65_17_3bin_Gestão_CadÚnico_PBF.
VARIABLE LABELS  d_65_17_3bin_Gestão_CadÚnico_PBF   'd_65_17_3bin_Gestão do Cadastro Único e Programa Bolsa Família - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Gestão_CadÚnico_PBF
1 'Gestão do Cadastro Único e Programa Bolsa Família'.
EXECUTE.

RECODE  q65_17  (11=1) INTO d_65_17_3bin_Gestão_Benefícios_Assistenciais.
VARIABLE LABELS  d_65_17_3bin_Gestão_Benefícios_Assistenciais   'd_65_17_3bin_Gestão de Benefícios Assistenciais - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Gestão_Benefícios_Assistenciais
1 'Gestão de Benefícios Assistenciais (BPC, benefícios eventuais)'.
EXECUTE.

RECODE  q65_17  (12=1) INTO d_65_17_3bin_Outros.
VARIABLE LABELS  d_65_17_3bin_Outros  'd_65_17_3bin_Outros - Principal serviço/atividade'.

VALUE LABELS  d_65_17_3bin_Outros
1 'Outros'.
EXECUTE.



**** Variável Gestão  - mencionada, no minimo, 1 vez 

IF  (q65_15 = 1 | q65_16 =1 | q65_17=1) d_65_Gestão=1.
EXECUTE.


IF  (q65_15 = 2 | q65_16 =2 | q65_17=2) d_65_Gestão_do_SUAS=1.
EXECUTE .


IF  (q65_15 = 3 | q65_16 =3 | q65_17=3) d_65_Vigilância_Socioassistencial=1.
EXECUTE .


IF  ( q65_15 = 4 | q65_16 =4 | q65_17=4) d_65_Gestão_do_Trabalho=1.
EXECUTE .


IF  (q65_15= 5 | q65_16=5 | q65_17=5) d_65_Regulação_do_SUAS=1.
EXECUTE.

IF  (q65_15= 6 | q65_16=6 | q65_17=6) d_65_Gestão_Financeira_Orçamentária=1.
EXECUTE.

IF  (q65_15= 7 | q65_16=7 | q65_17=7) d_65_PSB=1.
EXECUTE.

IF  (q65_15= 8 | q65_16=8 | q65_17=8) d_65_PSE_Média_Complexidade=1.
EXECUTE.

IF  (q65_15= 9 | q65_16=9 | q65_17=9) d_65_PSE_Alta_Complexidade=1.
EXECUTE.

IF  (q65_15= 10 | q65_16=10 | q65_17=10) d_65_Gestão_CadÚnico_PBF=1.
EXECUTE.

IF  (q65_15= 11 | q65_16=11 | q65_17=11) d_65_Gestão_Benefícios_Assistenciais=1.
EXECUTE.

IF  (q65_15 = 12 | q65_16=12 | q65_17=12) d_65_Outros=1.
EXECUTE.

Variable labels
d_65_Gestão 'Atividades de Gestão - mencionada, no minimo, 1 vez'
d_65_Gestão_do_SUAS  'Gestão do SUAS - mencionada, no mínimo, 1 vez'
d_65_Vigilância_Socioassistencial 'Vigilância Socioassistencial - mencionada, no mínimo, 1 vez'
d_65_Gestão_do_Trabalho 'Gestão do Trabalho - mencionada, no mínimo, 1 vez'
d_65_Regulação_do_SUAS 'Regulação do SUAS -  mencionada, no mínimo, 1 vez'
d_65_Gestão_Financeira_Orçamentária 'Gestão Financeira e Orçamentária - mencionada, no mínimo, 1 vez'
d_65_PSB 'Proteção Social Básica - mencionada, no mínimo, 1 vez'
d_65_PSE_Média_Complexidade 'PSE_Média_Complexidade - mencionada, no mínimo, 1 vez'
d_65_PSE_Alta_Complexidade 'PSE_Alta_Complexidade - mencionada, no mínimo, 1 vez'
d_65_Gestão_CadÚnico_PBF 'Gestão do Cadastro Único e Programa Bolsa Família - mencionado, no mínimo, 1 vez'
d_65_Gestão_Benefícios_Assistenciais 'Gestão de Benefícios Assistenciais (BPC, benefícios eventuais) - mencionado, no mínimo, 1 vez'
d_65_Outros 'Outras atividades - mencionadas, no mínimo, 1 vez'.



********************AGGREGATE VARIÁVEIS BINÁRIAS NA BASE RH**************************
********************AGGREGATE VARIÁVEIS BINÁRIAS PARA MERGE**************************




AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d_65_15_1bin_Gestão_do_SUAS_sum=SUM(d_65_15_1bin_Gestão_do_SUAS)
  /d_65_15_1bin_Vigilância_Socioassistencial_sum=SUM(d_65_15_1bin_Vigilância_Socioassistencial)
  /d_65_15_1bin_Gestão_do_Trabalho_sum=SUM(d_65_15_1bin_Gestão_do_Trabalho)
  /d_65_15_1bin_Regulação_do_SUAS_sum=SUM(d_65_15_1bin_Regulação_do_SUAS)
  /d_65_15_1bin_Gestão_Financeira_Orçamentária_sum=SUM(d_65_15_1bin_Gestão_Financeira_Orçamentária)
  /d_65_15_1bin_PSB_sum=SUM(d_65_15_1bin_PSB)
  /d_65_15_1bin_PSE_Média_Complexidade_sum=SUM(d_65_15_1bin_PSE_Média_Complexidade)
  /d_65_15_1bin_PSE_Alta_Complexidade_sum=SUM(d_65_15_1bin_PSE_Alta_Complexidade)
  /d_65_15_1bin_Gestão_CadÚnico_PBF_sum=SUM(d_65_15_1bin_Gestão_CadÚnico_PBF)
  /d_65_15_1bin_Gestão_Benefícios_Assistenciais_sum=SUM(d_65_15_1bin_Gestão_Benefícios_Assistenciais)
  /d_65_15_1bin_Outros_sum=SUM(d_65_15_1bin_Outros)
  /d_65_16_2bin_Gestão_sum=SUM(d_65_16_2bin_Gestão)
  /d_65_16_2bin_Gestão_do_SUAS_sum=SUM(d_65_16_2bin_Gestão_do_SUAS)
  /d_65_16_2bin_Vigilância_Socioassistencial_sum=SUM(d_65_16_2bin_Vigilância_Socioassistencial)
  /d_65_16_2bin_Gestão_do_Trabalho_sum=SUM(d_65_16_2bin_Gestão_do_Trabalho)
  /d_65_16_2bin_Regulação_do_SUAS_sum=SUM(d_65_16_2bin_Regulação_do_SUAS)
  /d_65_16_2bin_Gestão_Financeira_Orçamentária_sum=SUM(d_65_16_2bin_Gestão_Financeira_Orçamentária)
  /d_65_16_2bin_PSB_sum=SUM(d_65_16_2bin_PSB)
  /d_65_16_2bin_PSE_Média_Complexidade_sum=SUM(d_65_16_2bin_PSE_Média_Complexidade)
  /d_65_16_2bin_PSE_Alta_Complexidade_sum=SUM(d_65_16_2bin_PSE_Alta_Complexidade)
  /d_65_16_2bin_Gestão_CadÚnico_PBF_sum=SUM(d_65_16_2bin_Gestão_CadÚnico_PBF)
  /d_65_16_2bin_Gestão_Benefícios_Assistenciais_sum=SUM(d_65_16_2bin_Gestão_Benefícios_Assistenciais)
  /d_65_16_2bin_Outros_sum=SUM(d_65_16_2bin_Outros)
  /d_65_17_3bin_Gestão_sum=SUM(d_65_17_3bin_Gestão)
  /d_65_17_3bin_Gestão_do_SUAS_sum=SUM(d_65_17_3bin_Gestão_do_SUAS)
  /d_65_17_3bin_Vigilância_Socioassistencial_sum=SUM(d_65_17_3bin_Vigilância_Socioassistencial)
  /d_65_17_3bin_Gestão_do_Trabalho_sum=SUM(d_65_17_3bin_Gestão_do_Trabalho)
  /d_65_17_3bin_Regulação_do_SUAS_sum=SUM(d_65_17_3bin_Regulação_do_SUAS)
  /d_65_17_3bin_Gestão_Financeira_Orçamentária_sum=SUM(d_65_17_3bin_Gestão_Financeira_Orçamentária)
  /d_65_17_3bin_PSB_sum=SUM(d_65_17_3bin_PSB)
  /d_65_17_3bin_PSE_Média_Complexidade_sum=SUM(d_65_17_3bin_PSE_Média_Complexidade)
  /d_65_17_3bin_PSE_Alta_Complexidade_sum=SUM(d_65_17_3bin_PSE_Alta_Complexidade)
  /d_65_17_3bin_Gestão_CadÚnico_PBF_sum=SUM(d_65_17_3bin_Gestão_CadÚnico_PBF)
  /d_65_17_3bin_Gestão_Benefícios_Assistenciais_sum=SUM(d_65_17_3bin_Gestão_Benefícios_Assistenciais)
  /d_65_17_3bin_Outros_sum=SUM(d_65_17_3bin_Outros)
  /N_BREAK_1=N.



VARIABLE LABELS
d_65_15_1bin_Gestão_do_SUAS_sum 'd_65_15_1bin_Gestão_do_SUAS'
d_65_15_1bin_Vigilância_Socioassistencial_sum 'd_65_15_1bin_Vigilância_Socioassistencial'
d_65_15_1bin_Gestão_do_Trabalho_sum 'd_65_15_1bin_Gestão_do_Trabalho'
d_65_15_1bin_Regulação_do_SUAS_sum 'd_65_15_1bin_Regulação_do_SUAS'
d_65_15_1bin_Gestão_Financeira_Orçamentária_sum 'd_65_15_1bin_Gestão_Financeira_Orçamentária'
d_65_15_1bin_PSB_sum 'd_65_15_1bin_PSB'
d_65_15_1bin_PSE_Média_Complexidade_sum 'd_65_15_1bin_PSE_Média_Complexidade'
d_65_15_1bin_PSE_Alta_Complexidade_sum 'd_65_15_1bin_PSE_Alta_Complexidade'
d_65_15_1bin_Gestão_CadÚnico_PBF_sum 'd_65_15_1bin_Gestão_CadÚnico_PBF'
d_65_15_1bin_Gestão_Benefícios_Assistenciais_sum 'd_65_15_1bin_Gestão_Benefícios_Assistenciais'
d_65_15_1bin_Outros_sum 'd_65_15_1bin_Outros'
d_65_16_2bin_Gestão_sum 'd_65_16_2bin_Gestão'
d_65_16_2bin_Gestão_do_SUAS_sum 'd_65_16_2bin_Gestão_do_SUAS'
d_65_16_2bin_Vigilância_Socioassistencial_sum 'd_65_16_2bin_Vigilância_Socioassistencial'
d_65_16_2bin_Gestão_do_Trabalho_sum 'd_65_16_2bin_Gestão_do_Trabalho'
d_65_16_2bin_Regulação_do_SUAS_sum 'd_65_16_2bin_Regulação_do_SUAS'
d_65_16_2bin_Gestão_Financeira_Orçamentária_sum 'd_65_16_2bin_Gestão_Financeira_Orçamentária'
d_65_16_2bin_PSB_sum 'd_65_16_2bin_PSB'
d_65_16_2bin_PSE_Média_Complexidade_sum 'd_65_16_2bin_PSE_Média_Complexidade'
d_65_16_2bin_PSE_Alta_Complexidade_sum 'd_65_16_2bin_PSE_Alta_Complexidade'
d_65_16_2bin_Gestão_CadÚnico_PBF_sum 'd_65_16_2bin_Gestão_CadÚnico_PBF'
d_65_16_2bin_Gestão_Benefícios_Assistenciais_sum 'd_65_16_2bin_Gestão_Benefícios_Assistenciais'
d_65_16_2bin_Outros_sum 'd_65_16_2bin_Outros'
d_65_17_3bin_Gestão_sum 'd_65_17_3bin_Gestão'
d_65_17_3bin_Gestão_do_SUAS_sum 'd_65_17_3bin_Gestão_do_SUAS'
d_65_17_3bin_Vigilância_Socioassistencial_sum 'd_65_17_3bin_Vigilância_Socioassistencial'
d_65_17_3bin_Gestão_do_Trabalho_sum 'd_65_17_3bin_Gestão_do_Trabalho'
d_65_17_3bin_Regulação_do_SUAS_sum 'd_65_17_3bin_Regulação_do_SUAS'
d_65_17_3bin_Gestão_Financeira_Orçamentária_sum 'd_65_17_3bin_Gestão_Financeira_Orçamentária'
d_65_17_3bin_PSB_sum 'd_65_17_3bin_PSB'
d_65_17_3bin_PSE_Média_Complexidade_sum 'd_65_17_3bin_PSE_Média_Complexidade'
d_65_17_3bin_PSE_Alta_Complexidade_sum 'd_65_17_3bin_PSE_Alta_Complexidade'
d_65_17_3bin_Gestão_CadÚnico_PBF_sum 'd_65_17_3bin_Gestão_CadÚnico_PBF'
d_65_17_3bin_Gestão_Benefícios_Assistenciais_sum 'd_65_17_3bin_Gestão_Benefícios_Assistenciais'
d_65_17_3bin_Outros_sum 'd_65_17_3bin_Outros'.
 EXECUTE.












