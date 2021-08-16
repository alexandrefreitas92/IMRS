* Encoding: UTF-8.
****************************************************************************************************
SINTAXE VARI�VEIS DERIVADAS GEST�O MUNICIPAL - CENSO SUAS 2019
****************************************************************************************************

********************************************** quest�o 5

RECODE q5_1 (1=0) INTO d_5.
RECODE q5   (2019=1)  (2018=2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 = 6) (2006 THRU 2010 = 7) (LOWEST THRU 2005 = 8) INTO d_5.
VARIABLE LABELS d_5 'd_5. Ano da Lei Municipal de regulamenta��o do SUAS'.
VALUE LABELS d_5
0 'N�o possui Lei'
1 '2019'
2 '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.


*******************************quest�o 12

RECODE q12 (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_12.
VARIABLE LABELS d_12 'd_12.Quantidade de Pessoas em Situa��o de Rua'.
VALUE LABELS d_12
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.

************************************quest�o 17

RECODE q17_98 (1=0) INTO d_17.
RECODE q17 (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru 50=4) (51 thru 200=5)
(201 thru 500=6)  (501 thru 1000=7) (1001 thru Highest=8) (ELSE=Copy) INTO d_17.
VARIABLE LABELS d_17 'd_17. Quantas pessoas foram atendidas pelo Servi�o de Prote��o Social B�sica no Domic�lio para Pessoas com defici�ncia e idosas no munic�pio, em agosto/2018'.
VALUE LABELS d_17
0 'N�o soube informar'
1 'Nenhum' 
2 'At� 5'  
3 '6 a 10'  
4 '11 a 50'  
5 '51 a 200'
6 '201 a 500' 
7 '501 a 1.000' 
8 'mais de 1.000'. 
EXECUTE.


************************************quest�o 19

RECODE q19_5_99  (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_19_5.
VARIABLE LABELS d_19_5 'd_19_5.MSE executada em unidade no municipio'.
VALUE LABELS d_19_5
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.

RECODE q19_6_99  (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_19_6.
VARIABLE LABELS d_19_6 'd_19_6.MSE executada em outra unidade  publica (exceto CREAS)'.
VALUE LABELS d_19_6
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.


************************************quest�o 22

RECODE q22_4_99   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_22_4.
VARIABLE LABELS d_22_4 'd_22_4.Abordagem executada em outra  unidade publica'.
VALUE LABELS d_22_4
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.


RECODE q22_5_99   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_22_5.
VARIABLE LABELS d_22_5 'd_22_5.Abordagem xecutada em Entidade ou ONG'.
VALUE LABELS d_22_5
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.


***************************************quest�o 27

RECODE q27 (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 15=3) (16 thru 20=4) (21 thru HIGHEST=5) into d_27.
VARIABLE LABELS  d_27 'd_27. Atualmente, quantas crian�as e/ou adolescente est�o acolhidos em fam�lias guardi�s na fam�lia extensa inseridas no Programa'.
VALUE LABELS d_27
0 'Nenhuma'
1 'De 1 a 5 crian�as e/ou adolescente'
2 'De 6 a 10 crian�as e/ou adolescente'
3 'De 11 a 15 crian�as e/ou adolescente' 
4 'De 16 a 20 crian�as e/ou adolescente' 
5 'Mais de 20 crian�as e/ou adolescente'. 
EXECUTE.

**************************************quest�o 28

RECODE q28 (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 25=3) (26 thru 50=4) (51 thru HIGHEST=5) into d_28.
VARIABLE LABELS  d_28 'd_28. Atualmente quantas fam�lias que recebem recursos pelo Programa de Guarda Subsidiada'.
VALUE LABELS d_28
0 'Nenhuma'
1 'De 1 a 5 fam�lias'
2 'De 6 a 10 fam�lias'
3 'De 11 a 25 fam�lias' 
4 'De 26 a 50 fam�lias' 
5 'Mais de 50 fam�lias'. 
EXECUTE.



*********************************************q29**************************************************

RECODE q29 (0=0) (1 thru 476=1) (477 thru 500=2) (500 thru 950=3) (951 thru 999=4) (1000 thru HIGHEST=5) into d_29.
VARIABLE LABELS  d_29 'd_29 Qual � o valor m�dio de repasse a estas fam�lias?'.
VALUE LABELS d_29
0 'Zero'
1 'At� meio sal�rio m�nimo'
2 'Meio sal�rio m�nimo'
3 'Mais de meio e menos de um sal�rio m�nimo' 
4 'Um sal�rio M�nimo' 
5 'Mais de um sal�rio m�nimo'. 
EXECUTE.






****q33********************************************************************************


RECODE q33   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_33.
VARIABLE LABELS d_33  'd_33.quantidade de pessoas desabrigadas que foram acolhidas em alojamentos provis�rios'.
VALUE LABELS d_33
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.


**q35********************************************************************************************************************************



RECODE q35   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 200=4)
(201 thru 500=5)  (501 thru 1000=6) (1001 thru Highest=7)  INTO d_35.
VARIABLE LABELS d_35  'd_35.Para quantos imigrantes/refugiados a assistencia social do munic�pio ofertou programa(s)/servi�o(s)'.
VALUE LABELS d_35
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 200'
5 '201 a 500' 
6 '501 a 1.000' 
7 'mais de 1.000'. 
EXECUTE.


****q41*****************************************************************

RECODE q41_1_3 (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d41_1_3.
VARIABLE LABELS  d41_1_3   'd41_1_3_Ano da �ltima atualiza��o da regulamenta��o: Benef�cio Eventual por Situa��o de morte'.
FORMATS d41_1_3  (f1.0).
EXECUTE.

VALUE LABELS  d41_1_3
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.




RECODE q41_2_3  (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d41_2_3.
VARIABLE LABELS  d41_2_3   'd41_2_3_Ano da �ltima atualiza��o da regulamenta��o: Benef�cio Eventual por Situa��o de Natalidade'.
FORMATS d41_2_3  (f1.0).
EXECUTE.

VALUE LABELS  d41_2_3
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.


RECODE q41_3_3  (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d41_3_3.
VARIABLE LABELS  d41_3_3   'd41_3_3_Ano da �ltima atualiza��o da regulamenta��o: Benef�cio Eventual para situa��o de calamidade p�blica'.
FORMATS d41_3_3  (f1.0).
EXECUTE.

VALUE LABELS  d41_3_3
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.


RECODE q41_4_3  (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d41_4_3.
VARIABLE LABELS  d41_4_3   'd41_4_3_Ano da �ltima atualiza��o da regulamenta��o: Outros benef�cios eventuais para fam�lias em situa��o de vulnerabilidade tempor�ria'.
FORMATS d41_4_3  (f1.0).
EXECUTE.

VALUE LABELS  d41_4_3
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.


***q43***********************************************************

RECODE q43   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 100=4)
(101 thru Highest=5)  INTO d_43.
VARIABLE LABELS d_43  'd_43.Quantos locais (postos/unidades) realizam atendimento do Cadastro �nico em seu munic�pio'.
VALUE LABELS d_43
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 100'
5 'mais de 100'. 
EXECUTE.



***q44***********************************************************

RECODE q44   (0=0) (1 thru 5=1) (6 thru 10=2) (11 thru 50=3) (51 thru 100=4)
(101 thru Highest=5)  INTO d_44.
VARIABLE LABELS d_44  'd_44.Do total de locais (postos/unidades) citados na quest�o anterior, quantos s�o EXCLUSIVOS para atendimento do Cadastro �nico'.
VALUE LABELS d_44
0 'Nenhum' 
1 'At� 5'  
2 '6 a 10'  
3 '11 a 50'  
4 '51 a 100'
5 'mais de 100'. 
EXECUTE.

*************************************quest�o 51


RECODE q51_1_2  (0=1) (1 thru 20=2) (21 thru 50=3) (51 thru 100=4) (101 thru HIGHEST=5) into d_51_1_2.
VARIABLE LABELS d_51_1_2 'd_51_1_2. Quantidade de vagas para n�vel superior '.
VALUE LABELS d_51_1_2
1 'Nenhuma'
2 'De 1 a 20 vagas'
3 'De 21 a 50 vagas'
4 'De 51 a 100 vagas' 
5 'Mais de 100 vagas'. 
EXECUTE.

RECODE q51_1_4 (0=1) (1 thru 20=2) (21 thru 50=3) (51 thru 100=4) (101 thru HIGHEST=5) into d_51_1_4.
VARIABLE LABELS  d_51_1_4  'd_51_1_4 . Quantidade de trabalhadores de n�vel superior que tomaram posse'.
VALUE LABELS d_51_1_4
1 'Nenhum'
2 'De 1 a 20 trabalhadores'
3 'De 21 a 50 trabalhadores'
4 'De 51 a 100 trabalhadores'
5 'Mais de 100 trabalhadores'. 
EXECUTE.

RECODE q51_2_2  (0=1) (1 thru 20=2) (21 thru 50=3) (51 thru 100=4) (101 thru HIGHEST=5) into d_51_2_2.
VARIABLE LABELS  d_51_2_2 'd_51_2_2. Quantidade de vagas para n�vel medio'.
VALUE LABELS d_51_2_2
1 'Nenhuma'
2 'De 1 a 20 vagas'
3 'De 21 a 50 vagas'
4 'De 51 a 100 vagas' 
5 'Mais de 100 vagas'. 
EXECUTE.

RECODE q51_2_4  (0=1) (1 thru 20=2) (21 thru 50=3) (51 thru 100=4) (101 thru HIGHEST=5) into d_51_2_4.
VARIABLE LABELS  d_51_2_4  'd_51_2_4. Quantidade de trabalhadores de n�vel medio que tomaram posse'.
VALUE LABELS d_51_2_4
1 'Nenhum'
2 'De 1 a 20 trabalhadores'
3 'De 21 a 50 trabalhadores'
4 'De 51 a 100 trabalhadores'
5 'Mais de 100 trabalhadores'. 
EXECUTE.


*******************************q53


RECODE q53 (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d_53.
VARIABLE LABELS d_53  'd_53. Ano da �ltima atualiza��o do Plano de Capacita��o e Educa��o Permanente'.
VALUE LABELS d_53
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.
















****************************************quest�o 54

RECODE q54_98 (1=0) INTO d_54.
RECODE q54 (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru 500=4) (501 thru 1000=5) (1001 thru HIGHEST=6) into d_54.
VARIABLE LABELS  d_54 'd_54. Para o ano de 2019, qual foi a previs�o de quantidade de trabalhadores capacitados no plano'.
VALUE LABELS d_54
0 'N�o h� previs�o'
1 'Nenhuma'
2 'De 1 a 5 pessoas'
3 'De 6 a 10 pessoas'
4 'De 10 a 500 pessoas' 
5 'De 501 a 1000 pessoas' 
6 'Mais de 1000 pessoas'. 
EXECUTE.

****************************************quest�o 56

RECODE q56_1_98 (1=0) INTO d_56_1.
RECODE q56_1 (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru 500=4) (501 thru 1000=5) (1001 thru HIGHEST=6) into d_56_1.
VARIABLE LABELS  d_56_1  'd_56_1. Informe o total de profissionais capacitadas(os) '.
VALUE LABELS d_56_1
0 'N�o sabe'
1 'Nenhuma'
2 'De 1 a 5 profissionais'
3 'De 6 a 10 profissionais'
4 'De 10 a 500 profissionais' 
5 'De 501 a 1000 profissionais ' 
6 'Mais de 1000 profissionais'. 
EXECUTE.

RECODE q56_2_98 (1=0) INTO d_56_2.
RECODE q56_2  (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru 500=4) (501 thru 1000=5) (1001 thru HIGHEST=6) into d_56_2.
VARIABLE LABELS  d_56_2 'd_56_2. Informe o total de conselheiras(os) capacitadas(os)'.
VALUE LABELS d_56_2
0 'N�o sabe'
1 'Nenhuma'
2 'De 1 a 5 conselheiras(os)'
3 'De 6 a 10 conselheiras(os)'
4 'De 10 a 500 conselheiras(os)' 
5 'De 501 a 1000 conselheiras(os)' 
6 'Mais de 1000 conselheiras(os)'. 
EXECUTE.


***************************************quest�o 58 

RECODE q58_98  (1=0) INTO d_58.
RECODE q58 (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru 100=4) (101 thru HIGHEST=5) into d_58.
VARIABLE LABELS  d_58  'd_58  Qual a quantidade de profissionais do munic�pio que participaram de cursos do CAPACITASUAS em 2019'.
VALUE LABELS d_58 
0 'N�o sabe'
1 'Nenhuma'
2 'De 1 a 5 profissionais'
3 'De 6 a 10 profissionais'
4 'De 10 a 100 profissionais' 
5 'Mais de 100 profissionais'.
EXECUTE.


****************************************quest�o 62

RECODE q62 (2019 =1)  (2018 =2) (2017=3) (2016=4) (2015=5) (2011 THRU 2014 =6) (2006 THRU 2010=7) (LOWEST THRU 2005=8) INTO d_62.
VARIABLE LABELS d_62  'd_62. Ano da �ltima atualiza��o do PCCS'.
VALUE LABELS d_62
1 '2019'
2  '2018'
3 '2017'
4 '2016'
5 '2015'
6 'De 2011 a 2014'
7 'De 2006 a 2010'
8 'At� 2005'.
EXECUTE.









