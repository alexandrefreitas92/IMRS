﻿* Encoding: UTF-8.

**Criação de Derivadas Conselheiros - RH Conselhos - CENSO 2019**



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

COMPUTE q71_2_rec=number(q71_2, EDATE10).
VARIABLE LABELS q71_2_rec "q71_2_data nascimento recodificada".
VARIABLE LEVEL  q71_2_rec (SCALE).
FORMATS q71_2_rec (EDATE10).
VARIABLE WIDTH  q71_2_rec(10).
EXECUTE.


**transformar em data**

COMPUTE  Idade=DATEDIF(DataCenso_2019, q71_2_rec, "years"). 
VARIABLE LABELS  Idade "Idade". 
VARIABLE LEVEL  Idade (SCALE). 
FORMATS  Idade (F5.0). 
VARIABLE WIDTH  Idade(5). 
EXECUTE.  
FREQUENCIES VARIABLES=Idade 
  /STATISTICS=RANGE MINIMUM MAXIMUM STDDEV MEAN MEDIAN 
  /FORMAT=LIMIT(50) 
  /ORDER=ANALYSIS.



RECODE Idade (18 thru 29=1) (30 thru 40=2) (41 thru 50=3) (51 thru 60=4) (61 thru 100=5) INTO 
    Faixas_etarias.
VARIABLE LABELS  Faixas_etarias 'Faixas etárias dos Conselheiros'.
EXECUTE.

VALUE LABELS
/Faixas_etarias 
1'18 a 29 anos'
2'30 a 40 anos'
3'41 a 50 anos'
4'51 a 60 anos' 
5'Mais de 60 anos'
.


*******deletar casos em que não  foi possivel calcular idade ou a mesma estava fora do padrão estabelecido

*FILTER OFF.
*USE ALL.
*SELECT IF (Faixas_etarias >= 1).
*EXECUTE.

*FREQUENCIES VARIABLES=Faixas_etarias
  /ORDER=ANALYSIS.






********************************************************************************
*****************************************************************************************
**Tempo de mandato dos Conselheiros**
* Date and Time Wizard: q71_8_Data de Início do mandato.

COMPUTE q71_8_rec=number(q71_8, EDATE10).
VARIABLE LABELS q71_8_rec "q71_8_Data de Início do mandato recodificada".
VARIABLE LEVEL  q71_8_rec (SCALE).
FORMATS q71_8_rec (EDATE10).
VARIABLE WIDTH  q71_8_rec(10).
EXECUTE.



****** Date and Time Wizard: q71_9_Data de término do mandato

COMPUTE q71_9_rec=number(q71_9, EDATE10).
VARIABLE LABELS q71_9_rec "q71_9_Data de término do mandato recodificada".
VARIABLE LEVEL  q71_9_rec (SCALE).
FORMATS q71_9_rec (EDATE10).
VARIABLE WIDTH q71_9_rec(10).
EXECUTE.



COMPUTE  TempoMandato=RND((q71_9_rec - q71_8_rec) / (365.25 * time.days(1))).
VARIABLE LABELS  TempoMandato "Tempo de mandato dos Conselheiros".
VARIABLE LEVEL  TempoMandato (SCALE).
FORMATS  TempoMandato (F5.0).
VARIABLE WIDTH  TempoMandato(5).
EXECUTE.





****variáveis binárias escolaridade***

RECODE q71_5 (251 thru 253=0) (254 thru 256=1) (257 thru 261=2) INTO d71_5.

VARIABLE LABELS d71_5 'd71_5_Níveis de escolaridade'.
EXECUTE.

VALUE LABELS d71_5
0 'Nível fundamental'
1 'Nível Médio'
2 'Nível Superior'.
execute.

RECODE d71_5 (0=1) INTO d71_5bin1.

VARIABLE LABELS  d71_5bin1  'd71_5bin1_Nível fundamental - Conselheiros que possuem até o nível médio (inclui aqueles sem escolaridade)'.
EXECUTE.

VALUE LABELS d71_5bin1
1 'Nível fundamental'.
EXECUTE.

RECODE d71_5  (1=1) INTO d71_5bin2.
VARIABLE LABELS d71_5bin2  'd71_5 bin2_Nível médio - Conselheiros com ensino superior incompleto e ensino médio completo'.
EXECUTE.

VALUE LABELS d71_5bin2
1 'Nível Médio'.
EXECUTE.

RECODE d71_5 (2=1) INTO d71_5bin3.
VARIABLE LABELS  d71_5bin3  'd71_5bin3_Nível superior - inclui Conselheiros graduados, com especialização, mestrado ou doutorado'.
EXECUTE.

VALUE LABELS  d71_5bin3
1 'Nível Superior'.
EXECUTE.



****variáveis binárias Representação***
*
"GA"  'Governamental - Assistência Social'
"GE"  'Governamental – Educação'
"GS"  'Governamental – Saúde'
"GO"  'Governamental – Outras áreas'
"E"  'Sociedade Civil - das entidade e Organizações de Ass. Social'
"U"  'Sociedade Civil - dos usuários'
"T"  'Sociedade Civil - organizações dos trabalhadores'
"O"  'Sociedade Civil - organizações dos usuários'
*


RECODE q71_6 ("U"=1) ("O"=1)  INTO d71_6bin1.
VARIABLE LABELS  d71_6bin1  'd71_6bin1_Representante de usuários e organização de usuários '.
EXECUTE.


RECODE q71_6 ("E" =1) INTO d71_6bin2.
VARIABLE LABELS d71_6bin2 'd71_6bin2_.Representante de Entidades de Assistência Social'.
EXECUTE.

RECODE q71_6 ("T"=1) INTO d71_6bin3.
VARIABLE LABELS  d71_6bin3 'd71_6bin3_Representante de organização dos trabalhadores'.
EXECUTE.

RECODE q71_6 ("GA","GE","GS" ,"GO"=1) INTO d71_6bin4.
VARIABLE LABELS  d71_6bin4  'd71_6bin4_Representante do Governo'.
EXECUTE.


RECODE q71_6 ("U", "O", "E","T" =1) INTO d71_6bin5.
VARIABLE LABELS  d71_6bin5  'd71_6bin5_Representante da Sociedade Civil'.
EXECUTE.

VALUE LABELS  d71_6bin1
1 'Representante de usuários e organização de usuários'.
EXECUTE.


VALUE LABELS   d71_6bin2
1 'Representante de Entidades de Assistência Social'.
EXECUTE.

VALUE LABELS d71_6bin3
1 'Representante de organização dos trabalhadores'.
EXECUTE.

VALUE LABELS  d71_6bin4
1 'Representante do Governo'.
EXECUTE.

VALUE LABELS  d71_6bin5
1 'Representante da Sociedade Civil'.
EXECUTE.

**variáveis binárias de representação para titulares e suplentes**
20: Conselheiro(a) Presidente
308: Vice-Presidente
105: Conselheiro Titular
106: Conselheiro Suplente
21: Coordenador(a)
251: Apoio Administrativo
248: Educador Social
253: Estagiário(a)
79: Secretário Executivo
252: Serviços Gerais
250: Técnico(a) Nível Médio
249: Técnico(a) Nível Superior
254: Outros


*Duvida: na sintaxe de 2016 estava assim: 

**
DO IF  (q53.6 >= 20 & (q53.6 =251 | q53.6 =20 | q53.6 =105 | q53.6 =106))
RECODE D53.5bin1 (1=1) INTO D53.5bin1.TIT
END IF
VARIABLE LABELS  D53.5bin1.TIT 'D53.5bin1.TIT.Representante Titular de usuários e organização de usuários'
EXECUTE
está certo colocar o "106"? Que se refere a conselheiros suplentes?**
*Fiz diferente de 2016 (Debora)*


DO IF  (q71_7 = 20 | q71_7 = 308 | q71_7 = 105).
RECODE d71_6bin1 (1=1) INTO d71_6bin1_TIT.
END IF.

VARIABLE LABELS d71_6bin1_TIT 'd71_6bin1_TIT_Representante Titular de usuários e organização de usuários'.
EXECUTE.



DO IF  (q71_7 = 106).
RECODE d71_6bin1 (1=1) INTO d71_6bin1_SUPL.
END IF.

VARIABLE LABELS d71_6bin1_SUPL 'd71_6bin1_SUPL_Representante Suplente de usuários e organização de usuários'.
EXECUTE.




DO IF  (q71_7 = 20 | q71_7 = 308 | q71_7 = 105).
RECODE d71_6bin2 (1=1) INTO d71_6bin2_TIT.
END IF.

VARIABLE LABELS  d71_6bin2_TIT  'd71_6bin2_TIT_Representante Titular de Entidades de Assistência Social'.
EXECUTE.


DO IF  (q71_7 = 106).
RECODE d71_6bin2 (1=1) INTO d71_6bin2_SUPL.
END IF.

VARIABLE LABELS  d71_6bin2_SUPL  'd71_6bin2_SUPL_Representante Suplente de Entidades de Assistência Social'.
EXECUTE.


DO IF  (q71_7 = 20 | q71_7 = 308 | q71_7 = 105).
RECODE d71_6bin3 (1=1) INTO d71_6bin3_TIT.
END IF.

VARIABLE LABELS   d71_6bin3_TIT  ' d71_6bin3_TIT_Representante Titular de organização dos trabalhadores'.
EXECUTE.


DO IF  (q71_7 = 106).
RECODE d71_6bin3 (1=1) INTO d71_6bin3_SUPL.
END IF.

VARIABLE LABELS d71_6bin3_SUPL  'd71_6bin3_SUPL_Representante Suplente de organização dos trabalhadores'.
EXECUTE.


DO IF  (q71_7 = 20 | q71_7 = 308 | q71_7 = 105).
RECODE d71_6bin4 (1=1) INTO d71_6bin4_TIT.
END IF.

VARIABLE LABELS  d71_6bin4_TIT  'd71_6bin4_TIT_Representante Titular do Governo'.
EXECUTE.


DO IF  (q71_7 = 106).
RECODE d71_6bin4 (1=1) INTO d71_6bin4_SUPL.
END IF.

VARIABLE LABELS  d71_6bin4_SUPL 'd71_6bin4_SUPL_Representante Suplente do Governo'.
EXECUTE.



DO IF  (q71_7 = 20 | q71_7 = 308 | q71_7 = 105).
RECODE d71_6bin5 (1=1) INTO d71_6bin5_TIT.
END IF.

VARIABLE LABELS  d71_6bin5_TIT  'd71_6bin5_TIT_Representante Titular da Sociedade Civil'.
EXECUTE.


DO IF  (q71_7 = 106).
RECODE d71_6bin5 (1=1) INTO d71_6bin5_SUPL.
END IF.

VARIABLE LABELS  d71_6bin5_SUPL 'd71_6bin5_SUPL_Representante Suplente da Sociedade Civil'.
EXECUTE.



AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin1_sum=SUM(d71_6bin1) 
  /d71_6bin2_sum=SUM(d71_6bin2) 
  /d71_6bin3_sum=SUM(d71_6bin3) 
  /d71_6bin4_sum=SUM(d71_6bin4) 
  /d71_6bin5_sum=SUM(d71_6bin5).



AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin1_TIT_sum=SUM(d71_6bin1_TIT)
  /d71_6bin1_SUPL_sum=SUM(d71_6bin1_SUPL)
  /d71_6bin2_TIT_sum=SUM(d71_6bin2_TIT)
  /d71_6bin2_SUPL_sum=SUM(d71_6bin2_SUPL).
EXECUTE.


AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin3_TIT_sum=SUM(d71_6bin3_TIT)
  /d71_6bin3_SUPL_sum=SUM(d71_6bin3_SUPL).
EXECUTE.


AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin4_TIT_sum=SUM(d71_6bin4_TIT)
  /d71_6bin4_SUPL_sum=SUM(d71_6bin4_SUPL).
EXECUTE.

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin5_TIT_sum=SUM(d71_6bin5_TIT)
  /d71_6bin5_SUPL_sum=SUM(d71_6bin5_SUPL).
EXECUTE.

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /nu_conselheiros=N.
EXECUTE.


RECODE d71_6bin1_sum  d71_6bin2_sum  d71_6bin3_sum  d71_6bin4_sum   d71_6bin5_sum  nu_conselheiros 
    (SYSMIS=0).
EXECUTE.


RECODE d71_6bin1_TIT_sum  d71_6bin1_SUPL_sum  d71_6bin2_TIT_sum  d71_6bin2_SUPL_sum  d71_6bin3_TIT_sum  d71_6bin3_SUPL_sum  d71_6bin4_TIT_sum  d71_6bin4_SUPL_sum
 d71_6bin5_TIT_sum  d71_6bin5_SUPL_sum   (SYSMIS=0).
EXECUTE.




VARIABLE LABELS
d71_6bin1_sum	'd71_6bin1_sum_Representante de usuários e organização de usuários'
  d71_6bin2_sum	'd71_6bin2_sum_Representante de Entidades de Assistência Social'
  d71_6bin3_sum	'd71_6bin3_sum_Representante de organização dos trabalhadores'
  d71_6bin4_sum	'd71_6bin4_sum_Representante do Governo'
  d71_6bin5_sum	'd71_6bin5_sum_Representante da Sociedade Civil'
  d71_6bin1_TIT_sum    'd71_6bin1_TIT_sum.Representante Titular de usuários e organização de usuários'
  d71_6bin1_SUPL_sum    'd71_6bin1_SUPL_sum.Representante Suplente de usuários e organização de usuários'
  d71_6bin2_TIT_sum    'd71_6bin2_TIT_sum.Representante Titular de Entidades de Assistência Social'
  d71_6bin2_SUPL_sum    'd71_6bin2_SUPL_sum.Representante Suplente de Entidades de Assistência Social'
  d71_6bin3_TIT_sum    'd71_6bin3_TIT_sum.Representante Titular de organização dos trabalhadores'
  d71_6bin3_SUPL_sum    'd71_6bin3_SUPL_sum.Representante Suplente de organização dos trabalhadores'
  d71_6bin4_TIT_sum    'd71_6bin4_TIT_sum.Representante Titular do Governo'
  d71_6bin4_SUPL_sum    'd71_6bin4_SUPL_sum.Representante Suplente do Governo'
  d71_6bin5_TIT_sum    'd71_6bin5_TIT_sum.Representante Titular da Sociedade Civil'
  d71_6bin5_SUPL_sum    'd71_6bin5_SUPL_sum.Representante Suplente da Sociedade Civil'
nu_conselheiros	'Quantidade de conselheiros'
.
EXECUTE.



*****agrr base separada para  merge com Dados Gerais*****

dataset declare aggr.RH.conselho2019.
AGGREGATE
/outfile='aggr.RH.conselho2019'
  /BREAK=IBGE
  /d71_6bin1_sum=SUM(d71_6bin1)
  /d71_6bin2_sum=SUM(d71_6bin2)
  /d71_6bin3_sum=SUM(d71_6bin3)
  /d71_6bin4_sum=SUM(d71_6bin4)
  /d71_6bin1_TIT_sum=SUM(d71_6bin1_TIT)
  /d71_6bin1_SUPL_sum=SUM(d71_6bin1_SUPL)
  /d71_6bin2_TIT_sum=SUM(d71_6bin2_TIT)
  /d71_6bin2_SUPL_sum=SUM(d71_6bin2_SUPL)
  /d71_6bin3_TIT_sum=SUM(d71_6bin3_TIT)
  /d71_6bin3_SUPL_sum=SUM(d71_6bin3_SUPL)
  /d71_6bin4_TIT_sum=SUM(d71_6bin4_TIT)
  /d71_6bin4_SUPL_sum=SUM(d71_6bin4_SUPL)
  /d71_6bin5_TIT_sum=SUM(d71_6bin5_TIT)
  /d71_6bin5_SUPL_sum=SUM(d71_6bin5_SUPL)
  /nu_conselheiros=N
.
EXECUTE.




RECODE  d71_6bin1_sum  d71_6bin2_sum  d71_6bin3_sum  d71_6bin4_sum  d71_6bin5_sum  nu_conselheiros 
    (SYSMIS=0).
EXECUTE.


RECODE d71_6bin1_TIT_sum  d71_6bin1_SUPL_sum  d71_6bin2_TIT_sum d71_6bin2_SUPL_sum  d71_6bin3_TIT_sum  d71_6bin3_SUPL_sum  d71_6bin4_TIT_sum  d71_6bin4_SUPL_sum
d71_6bin5_TIT_sum  d71_6bin5_SUPL_sum    (SYSMIS=0).
EXECUTE.






* Date and Time Wizard: Idade55.








DATASET ACTIVATE DataSet2.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=
  /d71_6bin1_sum=SUM(d71_6bin1) 
  /d71_6bin2_sum=SUM(d71_6bin2) 
  /d71_6bin3_sum=SUM(d71_6bin3) 
  /d71_6bin4_sum=SUM(d71_6bin4) 
  /d71_6bin5_sum=SUM(d71_6bin5).

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=IBGE
  /d71_6bin1_sum=SUM(d71_6bin1) 
  /d71_6bin2_sum=SUM(d71_6bin2) 
  /d71_6bin3_sum=SUM(d71_6bin3) 
  /d71_6bin4_sum=SUM(d71_6bin4) 
  /d71_6bin5_sum=SUM(d71_6bin5).
