
************************************************************************************************************************
Syntax Limpeza de outliers valores num�ricos  q8- Question�rio Fundo Municipal de Assist�ncia Social
************************************************************************************************************************





RECODE q_8_1_1_1 (ELSE=Copy) INTO  q_8_1_1_1_velha.
RECODE q_8_1_2_1 (ELSE=Copy) INTO  q_8_1_2_1_velha.
RECODE q_8_2_1_1 (ELSE=Copy) INTO  q_8_2_1_1_velha.
RECODE q_8_2_2_1 (ELSE=Copy) INTO  q_8_2_2_1_velha.
RECODE q_8_1_1_2 (ELSE=Copy) INTO  q_8_1_1_2_velha.
RECODE q_8_1_2_2 (ELSE=Copy) INTO  q_8_1_2_2_velha.
RECODE q_8_2_1_2 (ELSE=Copy) INTO  q_8_2_1_2_velha.
RECODE q_8_2_2_2 (ELSE=Copy) INTO  q_8_2_2_2_velha.
RECODE q_8_1_1_3 (ELSE=Copy) INTO  q_8_1_1_3_velha.
RECODE q_8_1_2_3 (ELSE=Copy) INTO  q_8_1_2_3_velha.
RECODE q_8_2_1_3 (ELSE=Copy) INTO  q_8_2_1_3_velha.
RECODE q_8_2_2_3 (ELSE=Copy) INTO  q_8_2_2_3_velha.
RECODE q_8_1_1_4 (ELSE=Copy) INTO  q_8_1_1_4_velha.
RECODE q_8_1_2_4 (ELSE=Copy) INTO  q_8_1_2_4_velha.
RECODE q_8_2_1_4 (ELSE=Copy) INTO  q_8_2_1_4_velha.
RECODE q_8_2_2_4 (ELSE=Copy) INTO  q_8_2_2_4_velha.
RECODE q_8_1_1_5 (ELSE=Copy) INTO  q_8_1_1_5_velha.
RECODE q_8_1_2_5 (ELSE=Copy) INTO  q_8_1_2_5_velha.
RECODE q_8_2_1_5 (ELSE=Copy) INTO  q_8_2_1_5_velha.
RECODE q_8_2_2_5 (ELSE=Copy) INTO  q_8_2_2_5_velha.
RECODE q_8_1_1_6 (ELSE=Copy) INTO  q_8_1_1_6_velha.
RECODE q_8_1_2_6 (ELSE=Copy) INTO  q_8_1_2_6_velha.
RECODE q_8_2_1_6 (ELSE=Copy) INTO  q_8_2_1_6_velha.
RECODE q_8_2_2_6 (ELSE=Copy) INTO  q_8_2_2_6_velha.
EXECUTE.




VARIABLE LABELS
q_8_1_1_1_velha  'q_8_1_1_1_v13303_Prote��o Social B�sica - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_1_2_velha  'q_8_1_1_2_v14544_Prote��o Social Especial - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_1_3_velha  'q_8_1_1_3_v14548_Benef�cios - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_1_4_velha  'q_8_1_1_4_v14552_Gest�o - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_1_5_velha  'q_8_1_1_5_v14556_Outros - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_1_6_velha  'q_8_1_1_6_v14560_Total - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio'
q_8_1_2_1_velha  'q_8_1_2_1_v14542_Prote��o Social B�sica - RECURSOS FINANCEIROS -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_1_2_2_velha  'q_8_1_2_2_v14545_Prote��o Social Especial - RECURSOS FINANCEIROS- Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_1_2_3_velha  'q_8_1_2_3_v14549_Benef�cios - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_1_2_4_velha  'q_8_1_2_4_v14553_Gest�o - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_1_2_5_velha  'q_8_1_2_5_v14557_Outros - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_1_2_6_velha  'q_8_1_2_6_v14561_Total - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
q_8_2_1_1_velha  'q_8_2_1_1_v14470_Prote��o Social B�sica - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_1_2_velha  'q_8_2_1_2_v14546_Prote��o Social Especial - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_1_3_velha  'q_8_2_1_3_v14550_Benef�cios - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_1_4_velha  'q_8_2_1_4_v14554_Gest�o - DESPESAS PAGAS- Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_1_5_velha  'q_8_2_1_5_v14558_Outros - DESPESAS PAGAS- Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_1_6_velha  'q_8_2_1_6_v14562_Total - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio'
q_8_2_2_1_velha  'q_8_2_2_1_v14543_Prote��o Social B�sica - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual'
q_8_2_2_2_velha  'q_8_2_2_2_v14547_Prote��o Social Especial - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual'
q_8_2_2_3_velha  'q_8_2_2_3_v14551_Benef�cios - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual'
q_8_2_2_4_velha  'q_8_2_2_4_v14555_Gest�o - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual'
q_8_2_2_5_velha  'q_8_2_2_5_v14559_Outros - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual'
q_8_2_2_6_velha  'q_8_2_2_6_v14563_Total - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual'
.






***************************************************************************************************************************************
*Passo 1 - Dividir todas as vari�veis de recurso financeiro pela popula��o, para criar um valor per capita.
*Dessa forma, � poss�vel comparar a distribui��o para todos os munic�pios.
***************************************************************************************************************************************

COMPUTE	q_8_1_1_1_POP= q_8_1_1_1/Pop_total_2010.
COMPUTE	q_8_1_2_1_POP= q_8_1_2_1/Pop_total_2010.
COMPUTE	q_8_2_1_1_POP= q_8_2_1_1/Pop_total_2010.
COMPUTE	q_8_2_2_1_POP= q_8_2_2_1/Pop_total_2010.
COMPUTE	q_8_1_1_2_POP= q_8_1_1_2/Pop_total_2010.
COMPUTE	q_8_1_2_2_POP= q_8_1_2_2/Pop_total_2010.
COMPUTE	q_8_2_1_2_POP= q_8_2_1_2/Pop_total_2010.
COMPUTE	q_8_2_2_2_POP= q_8_2_2_2/Pop_total_2010.
COMPUTE	q_8_1_1_3_POP= q_8_1_1_3/Pop_total_2010.
COMPUTE	q_8_1_2_3_POP= q_8_1_2_3/Pop_total_2010.
COMPUTE	q_8_2_1_3_POP= q_8_2_1_3/Pop_total_2010.
COMPUTE	q_8_2_2_3_POP= q_8_2_2_3/Pop_total_2010.
COMPUTE	q_8_1_1_4_POP= q_8_1_1_4/Pop_total_2010.
COMPUTE	q_8_1_2_4_POP= q_8_1_2_4/Pop_total_2010.
COMPUTE	q_8_2_1_4_POP= q_8_2_1_4/Pop_total_2010.
COMPUTE	q_8_2_2_4_POP= q_8_2_2_4/Pop_total_2010.
COMPUTE	q_8_1_1_5_POP= q_8_1_1_5/Pop_total_2010.
COMPUTE	q_8_1_2_5_POP= q_8_1_2_5/Pop_total_2010.
COMPUTE	q_8_2_1_5_POP= q_8_2_1_5/Pop_total_2010.
COMPUTE	q_8_2_2_5_POP= q_8_2_2_5/Pop_total_2010.
COMPUTE	q_8_1_1_6_POP= q_8_1_1_6/Pop_total_2010.
COMPUTE	q_8_1_2_6_POP= q_8_1_2_6/Pop_total_2010.
COMPUTE	q_8_2_1_6_POP= q_8_2_1_6/Pop_total_2010.
COMPUTE	q_8_2_2_6_POP= q_8_2_2_6/Pop_total_2010.
EXECUTE.




VARIABLE LABELS
q_8_1_1_1_POP  'q_8_1_1_1_v13303_Prote��o Social B�sica - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_1_2_POP  'q_8_1_1_2_v14544_Prote��o Social Especial - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_1_3_POP  'q_8_1_1_3_v14548_Benef�cios - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_1_4_POP  'q_8_1_1_4_v14552_Gest�o - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_1_5_POP  'q_8_1_1_5_v14556_Outros - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_1_6_POP  'q_8_1_1_6_v14560_Total - RECURSOS FINANCEIROS - Valor alocado referente aos recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_1_2_1_POP  'q_8_1_2_1_v14542_Prote��o Social B�sica - RECURSOS FINANCEIROS -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios) em rela��o a sua popula��o total'
q_8_1_2_2_POP  'q_8_1_2_2_v14545_Prote��o Social Especial - RECURSOS FINANCEIROS- Valor recebido do governo estadual (Fundo a Fundo e Conv�nios) em rela��o a sua popula��o total'
q_8_1_2_3_POP  'q_8_1_2_3_v14549_Benef�cios - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios em rela��o a sua popula��o total)'
q_8_1_2_4_POP  'q_8_1_2_4_v14553_Gest�o - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios) em rela��o a sua popula��o total'
q_8_1_2_5_POP  'q_8_1_2_5_v14557_Outros - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios) em rela��o a sua popula��o total'
q_8_1_2_6_POP  'q_8_1_2_6_v14561_Total - RECURSOS FINANCEIROS - Valor recebido do governo estadual (Fundo a Fundo e Conv�nios) em rela��o a sua popula��o total'
q_8_2_1_1_POP  'q_8_2_1_1_v14470_Prote��o Social B�sica - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_1_2_POP  'q_8_2_1_2_v14546_Prote��o Social Especial - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_1_3_POP  'q_8_2_1_3_v14550_Benef�cios - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_1_4_POP  'q_8_2_1_4_v14554_Gest�o - DESPESAS PAGAS- Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_1_5_POP  'q_8_2_1_5_v14558_Outros - DESPESAS PAGAS- Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_1_6_POP  'q_8_2_1_6_v14562_Total - DESPESAS PAGAS - Valor da despesa realizada com recursos pr�prios do munic�pio em rela��o a sua popula��o total'
q_8_2_2_1_POP  'q_8_2_2_1_v14543_Prote��o Social B�sica - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
q_8_2_2_2_POP  'q_8_2_2_2_v14547_Prote��o Social Especial - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
q_8_2_2_3_POP  'q_8_2_2_3_v14551_Benef�cios - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
q_8_2_2_4_POP  'q_8_2_2_4_v14555_Gest�o - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
q_8_2_2_5_POP  'q_8_2_2_5_v14559_Outros - DESPESAS PAGAS - Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
q_8_2_2_6_POP  'q_8_2_2_6_v14563_Total - DESPESAS PAGAS- Valor da despesa realizada com recursos repassados pelo governo estadual em rela��o a sua popula��o total'
.

***************************************************************************************************************************************
*Passo 2 - Analisar a distribui��o
***************************************************************************************************************************************


FREQUENCIES VARIABLES= 
q_8_1_1_1_POP
q_8_1_2_1_POP
q_8_2_1_1_POP
q_8_2_2_1_POP
q_8_1_1_2_POP
q_8_1_2_2_POP
q_8_2_1_2_POP
q_8_2_2_2_POP
q_8_1_1_3_POP
q_8_1_2_3_POP
q_8_2_1_3_POP
q_8_2_2_3_POP
q_8_1_1_4_POP
q_8_1_2_4_POP
q_8_2_1_4_POP
q_8_2_2_4_POP
q_8_1_1_5_POP
q_8_1_2_5_POP
q_8_2_1_5_POP
q_8_2_2_5_POP
q_8_1_1_6_POP
q_8_1_2_6_POP
q_8_2_1_6_POP
q_8_2_2_6_POP
  /FORMAT=NOTABLE
  /NTILES=4
  /NTILES=10
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS.






*********************************************************************************************************************************************************************************************************************************************
*Passo 3 - Cria��o de vari�vel de Execu��o relacionando despesa e or�amento:

* As vari�veis q_8_1_1_X (onde X varia de 1 a 6) se referem ao valor dos recursos pr�prios alocados pelo munic�pio no or�amento.
* As vari�veis q_8_2_1_X (onde X varia de 1 a 6) se referem ao valor da despesa realizada desses recursos. Assim, se dividirmos a despesa pelo or�amento, temos a execu��o daquele or�amento.
*Da mesma forma, as vari�veis q_8_1_2_X representam o valor repassado pelo Estado e as vari�veis q_8_2_2_X o valor despendido.
*Assim, posteriormente ser� poss�vel aplicar crit�rio de limpeza que elimine valores inconsistentes, como os munic�pios que executam um valor 10 vezes maior do que o valor or�ado, por exemplo.
*********************************************************************************************************************************************************************************************************************************************





COMPUTE	EXECUCAO_1MUN = q_8_2_1_1/q_8_1_1_1.
COMPUTE	EXECUCAO_1EST = q_8_2_2_1/q_8_1_2_1.
COMPUTE	EXECUCAO_2MUN = q_8_2_1_2/q_8_1_1_2.
COMPUTE	EXECUCAO_2EST = q_8_2_2_2/q_8_1_2_2.
COMPUTE	EXECUCAO_3MUN = q_8_2_1_3/q_8_1_1_3.
COMPUTE	EXECUCAO_3EST = q_8_2_2_3/q_8_1_2_3.
COMPUTE	EXECUCAO_4MUN = q_8_2_1_4/q_8_1_1_4.
COMPUTE	EXECUCAO_4EST = q_8_2_2_4/q_8_1_2_4.
COMPUTE	EXECUCAO_5MUN = q_8_2_1_5/q_8_1_1_5.
COMPUTE	EXECUCAO_5EST = q_8_2_2_5/q_8_1_2_5.
COMPUTE	EXECUCAO_6MUN = q_8_2_1_6/q_8_1_1_6.
COMPUTE	EXECUCAO_6EST = q_8_2_2_6/q_8_1_2_6.
EXECUTE.




VARIABLE LABELS
EXECUCAO_1MUN  'Prote��o Social B�sica -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_1EST 'Prote��o Social B�sica -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
EXECUCAO_2MUN 'Prote��o Social Especial -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_2EST 'Prote��o Social Especial  -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
EXECUCAO_3MUN 'Benef�cios -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_3EST 'Benef�cios -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
EXECUCAO_4MUN 'Gest�o -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_4EST 'Gest�o -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
EXECUCAO_5MUN 'Outros -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_5EST 'Outros -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
EXECUCAO_6MUN 'Total -  Execu��o relacionando despesa e or�amento -  valor alocado referente aos recursos pr�prios do munic�pio '
EXECUCAO_6EST 'Total -  Execu��o relacionando despesa e or�amento -  Valor recebido do governo estadual (Fundo a Fundo e Conv�nios)'
.



IF(		EXECUCAO_1MUN	>=	10	|
		EXECUCAO_1EST	>=	10	|
		EXECUCAO_2MUN	>=	10	|
		EXECUCAO_2EST	>=	10	|
		EXECUCAO_3MUN	>=	10	|
		EXECUCAO_3EST	>=	10	|
		EXECUCAO_4MUN	>=	10	|
		EXECUCAO_4EST	>=	10	|
		EXECUCAO_5MUN	>=	10	|
		EXECUCAO_5EST	>=	10	|
		EXECUCAO_6MUN	>=	10	|
		EXECUCAO_6EST	>=	10)	
LIMP_EXECUCAO=1.				
EXECUTE.				




VARIABLE LABELS
LIMP_EXECUCAO  'Limpeza outlier_ para munic�pios que executam um valor 10 vezes maior do que o valor or�ado '.


***************************************************************************************************************************************
*Passo 4 - Identificar as vari�veis que superam a linha de 400 reais per capita
***************************************************************************************************************************************



IF(	q_8_1_1_1_POP		>=	400	|
	q_8_1_2_1_POP		>=	400	|
	q_8_2_1_1_POP		>=	400	|
	q_8_2_2_1_POP		>=	400	|
	q_8_1_1_2_POP		>=	400	|
	q_8_1_2_2_POP		>=	400	|
	q_8_2_1_2_POP		>=	400	|
	q_8_2_2_2_POP		>=	400	|
	q_8_1_1_3_POP		>=	400	|
	q_8_1_2_3_POP		>=	400	|
	q_8_2_1_3_POP		>=	400	|
	q_8_2_2_3_POP		>=	400	|
	q_8_1_1_4_POP		>=	400	|
	q_8_1_2_4_POP		>=	400	|
	q_8_2_1_4_POP		>=	400	|
	q_8_2_2_4_POP		>=	400	|
	q_8_1_1_5_POP		>=	400	|
	q_8_1_2_5_POP		>=	400	|
	q_8_2_1_5_POP		>=	400	|
	q_8_2_2_5_POP		>=	400	|
	q_8_1_1_6_POP		>=	400	|
	q_8_1_2_6_POP		>=	400	|
	q_8_2_1_6_POP		>=	400	|
	q_8_2_2_6_POP		>=	400)	
LIMP_OUT=1.				
	EXECUTE.				



VARIABLE LABELS
LIMP_OUT  'Limpeza outlier  para vari�veis que superam a linha de 400 reais per capita '.



***************************************************************************************************************************************
*Passo 5 - Executar a limpeza dos valores inconsistentes
***************************************************************************************************************************************

IF EXECUCAO_1MUN > 10 q_8_2_1_1=0.777.
IF EXECUCAO_1EST > 10 q_8_2_2_1=0.777.
IF EXECUCAO_2MUN > 10 q_8_2_1_2=0.777.
IF EXECUCAO_2EST > 10 q_8_2_2_2=0.777.
IF EXECUCAO_3MUN > 10 q_8_2_1_3=0.777.
IF EXECUCAO_3EST > 10 q_8_2_2_3=0.777.
IF EXECUCAO_4MUN > 10 q_8_2_1_4=0.777.
IF EXECUCAO_4EST > 10 q_8_2_2_4=0.777.
IF EXECUCAO_5MUN > 10 q_8_2_1_5=0.777.
IF EXECUCAO_5EST > 10 q_8_2_2_5=0.777.
IF EXECUCAO_6MUN > 10 q_8_2_1_6=0.777.
IF EXECUCAO_6EST >10 q_8_2_2_6=0.777.
IF EXECUCAO_1MUN > 10 q_8_1_1_1=0.777.
IF EXECUCAO_1EST > 10 q_8_1_2_1=0.777.
IF EXECUCAO_2MUN > 10 q_8_1_1_2=0.777.
IF EXECUCAO_2EST > 10 q_8_1_2_2=0.777.
IF EXECUCAO_3MUN > 10 q_8_1_1_3=0.777.
IF EXECUCAO_3EST > 10 q_8_1_2_3=0.777.
IF EXECUCAO_4MUN > 10 q_8_1_1_4=0.777.
IF EXECUCAO_4EST > 10 q_8_1_2_4=0.777.
IF EXECUCAO_5MUN > 10 q_8_1_1_5=0.777.
IF EXECUCAO_5EST > 10 q_8_1_2_5=0.777.
IF EXECUCAO_6MUN > 10 q_8_1_1_6=0.777.
IF EXECUCAO_6EST >10q_8_1_2_6=0.777.
execute.

*Alterar o r�tulo dos valores para 0.777 = valores inconsistentes

IF	q_8_1_1_1_POP >= 400 	q_8_1_1_1	=	0.777	.
IF	q_8_1_2_1_POP >= 400 	q_8_1_2_1	=	0.777	.
IF	q_8_2_1_1_POP >= 400 	q_8_2_1_1	=	0.777	.
IF	q_8_2_2_1_POP >= 400 	q_8_2_2_1	=	0.777	.
IF	q_8_1_1_2_POP >= 400 	q_8_1_1_2	=	0.777	.
IF	q_8_1_2_2_POP >= 400 	q_8_1_2_2	=	0.777	.
IF	q_8_2_1_2_POP >= 400 	q_8_2_1_2	=	0.777	.
IF	q_8_2_2_2_POP >= 400 	q_8_2_2_2	=	0.777	.
IF	q_8_1_1_3_POP >= 400 	q_8_1_1_3	=	0.777	.
IF	q_8_1_2_3_POP >= 400 	q_8_1_2_3	=	0.777	.
IF	q_8_2_1_3_POP >= 400 	q_8_2_1_3	=	0.777	.
IF	q_8_2_2_3_POP >= 400 	q_8_2_2_3	=	0.777	.
IF	q_8_1_1_4_POP >= 400 	q_8_1_1_4	=	0.777	.
IF	q_8_1_2_4_POP >= 400 	q_8_1_2_4	=	0.777	.
IF	q_8_2_1_4_POP >= 400 	q_8_2_1_4	=	0.777	.
IF	q_8_2_2_4_POP >= 400 	q_8_2_2_4	=	0.777	.
IF	q_8_1_1_5_POP >= 400 	q_8_1_1_5	=	0.777	.
IF	q_8_1_2_5_POP >= 400 	q_8_1_2_5	=	0.777	.
IF	q_8_2_1_5_POP >= 400 	q_8_2_1_5	=	0.777	.
IF	q_8_2_2_5_POP >= 400 	q_8_2_2_5	=	0.777	.
IF	q_8_1_1_6_POP >= 400 	q_8_1_1_6	=	0.777	.
IF	q_8_1_2_6_POP >= 400 	q_8_1_2_6	=	0.777	.
IF	q_8_2_1_6_POP >= 400 	q_8_2_1_6	=	0.777	.
IF	q_8_2_2_6_POP>=400	q_8_2_2_6	=	0.777	.

