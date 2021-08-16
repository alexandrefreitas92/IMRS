* Encoding: UTF-8.
****************************************************************************************************
SINTAXE VARI�VEIS DERIVADAS GEST�O MUNICIPAL - CENSO SUAS 2019
****************************************************************************************************
****q1**********************************************************************************************

COMPUTE q2_1_velha=q2_1.
COMPUTE q2_2_velha=q2_2.
COMPUTE q2_3_velha=q2_3.
COMPUTE q2_4_velha=q2_4.
COMPUTE q2_5_velha=q2_5.
COMPUTE q2_6_velha=q2_6.
COMPUTE q2_7_velha=q2_7.
COMPUTE q2_99_velha=q2_99.
EXECUTE.

DO IF (q1=1 | q1=3 | q1=4).
RECODE q2_1 (ELSE=SYSMIS).
RECODE q2_2 (ELSE=SYSMIS).
RECODE q2_3 (ELSE=SYSMIS).
RECODE q2_4 (ELSE=SYSMIS).
RECODE q2_5 (ELSE=SYSMIS).
RECODE q2_6 (ELSE=SYSMIS).
RECODE q2_7 (ELSE=SYSMIS).
RECODE q2_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.

****q3**********************************************************************************************

COMPUTE q4_velha=q4.
EXECUTE.

DO IF (q3_7=0).
RECODE q4 (ELSE=SYSMIS).
END IF. 
EXECUTE.

****q5**********************************************************************************************

COMPUTE q5_velha=q5.
EXECUTE.


DO IF (q5>0).
RECODE q5_1 (ELSE=SYSMIS).
END IF. 
EXECUTE.



COMPUTE q6_1_velha=q6_1.
COMPUTE q6_2_velha=q6_2.
COMPUTE q6_3_velha=q6_3.
COMPUTE q6_4_velha=q6_4.
COMPUTE q6_5_velha=q6_5.
COMPUTE q6_6_velha=q6_6.
COMPUTE q6_7_velha=q6_7.
COMPUTE q6_8_velha=q6_8.
COMPUTE q6_9_velha=q6_9.
COMPUTE q6_10_velha=q6_10.
COMPUTE q6_11_velha=q6_11.
COMPUTE q6_12_velha=q6_12.
COMPUTE q6_13_velha=q6_13.
COMPUTE q6_14_velha=q6_14.
COMPUTE q6_15_velha=q6_15.
COMPUTE q6_16_velha=q6_16.
COMPUTE q6_17_velha=q6_17.
COMPUTE q6_18_velha=q6_18.
COMPUTE q6_19_velha=q6_19.
COMPUTE q6_20_velha=q6_20.
COMPUTE q6_21_velha=q6_21.
COMPUTE q6_22_velha=q6_22.
COMPUTE q6_99_velha=q6_99.
EXECUTE.

DO IF (q5_1=1).
RECODE q6_1 (ELSE=SYSMIS).
RECODE q6_2 (ELSE=SYSMIS).
RECODE q6_3 (ELSE=SYSMIS).
RECODE q6_4 (ELSE=SYSMIS).
RECODE q6_5 (ELSE=SYSMIS).
RECODE q6_6 (ELSE=SYSMIS).
RECODE q6_7 (ELSE=SYSMIS).
RECODE q6_8 (ELSE=SYSMIS).
RECODE q6_9 (ELSE=SYSMIS).
RECODE q6_10 (ELSE=SYSMIS).
RECODE q6_11 (ELSE=SYSMIS).
RECODE q6_12 (ELSE=SYSMIS).
RECODE q6_13 (ELSE=SYSMIS).
RECODE q6_14  (ELSE=SYSMIS).
RECODE q6_15 (ELSE=SYSMIS).
RECODE q6_16 (ELSE=SYSMIS).
RECODE q6_17  (ELSE=SYSMIS).
RECODE q6_18 (ELSE=SYSMIS).
RECODE q6_19 (ELSE=SYSMIS).
RECODE q6_20 (ELSE=SYSMIS).
RECODE q6_21 (ELSE=SYSMIS).
RECODE q6_22 (ELSE=SYSMIS).
RECODE q6_99(ELSE=SYSMIS).
END IF. 
EXECUTE.



****q11**********************************************************************************************

COMPUTE q12_velha=q12.
COMPUTE q13_velha=q13.
COMPUTE q14_velha=q14.


DO IF (q11=0).
RECODE q12 (ELSE=SYSMIS).
RECODE q13 (ELSE=SYSMIS).
RECODE q14 (ELSE=SYSMIS).
END IF. 
EXECUTE.


****q15**********************************************************************************************

COMPUTE q16_velha=q16.
COMPUTE q17_velha=q17.


DO IF (q15=0).
RECODE q16 (ELSE=SYSMIS).
RECODE q17 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q18*******************************************************************




COMPUTE q18_1_velha=q18_1.
COMPUTE q18_2_velha=q18_2.
COMPUTE q18_3_velha=q18_3.
COMPUTE q18_4_velha=q18_4.
COMPUTE q18_5_velha=q18_5.
COMPUTE q18_6_velha=q18_6.
COMPUTE q18_99_velha=q18_99.
COMPUTE q18_0_velha=q18_0.
EXECUTE.

DO IF (q18_0=1 | q18_1=1).
RECODE q18_1 (ELSE=SYSMIS).
RECODE q18_2 (ELSE=SYSMIS).
RECODE q18_3 (ELSE=SYSMIS).
RECODE q18_4 (ELSE=SYSMIS).
RECODE q18_5 (ELSE=SYSMIS).
RECODE q18_6 (ELSE=SYSMIS).
RECODE q18_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q19*******************************************************************

COMPUTE q19_1_velha=q19_1.
COMPUTE q19_2_velha=q19_2.
COMPUTE q19_3_velha=q19_3.
COMPUTE q19_4_velha=q19_4.
COMPUTE q19_5_velha=q19_5.
COMPUTE q19_6_velha=q19_6.
COMPUTE q19_7_velha=q19_7.
COMPUTE q19_99_velha=q19_99.
COMPUTE q19_0_velha=q19_0.
EXECUTE.

DO IF (q19_0=1).
RECODE q19_1 (ELSE=SYSMIS).
RECODE q19_2 (ELSE=SYSMIS).
RECODE q19_3 (ELSE=SYSMIS).
RECODE q19_4 (ELSE=SYSMIS).
RECODE q19_5 (ELSE=SYSMIS).
RECODE q19_6 (ELSE=SYSMIS).
RECODE q19_7 (ELSE=SYSMIS).
RECODE q19_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.


**q20******************************************************************

COMPUTE q20_1_velha=q20_1.
COMPUTE q20_2_velha=q20_2.
COMPUTE q20_3_velha=q20_3.
COMPUTE q20_4_velha=q20_4.
COMPUTE q20_5_velha=q20_5.
COMPUTE q20_6_velha=q20_6.
COMPUTE q20_7_velha=q20_7.
COMPUTE q20_8_velha=q20_8.
COMPUTE q20_0_velha=q20_0.
EXECUTE.

DO IF (q20_0=1).
RECODE q20_1 (ELSE=SYSMIS).
RECODE q20_2 (ELSE=SYSMIS).
RECODE q20_3 (ELSE=SYSMIS).
RECODE q20_4 (ELSE=SYSMIS).
RECODE q20_5 (ELSE=SYSMIS).
RECODE q20_6 (ELSE=SYSMIS).
RECODE q20_7 (ELSE=SYSMIS).
RECODE q20_8 (ELSE=SYSMIS).
END IF. 
EXECUTE.


**q21******************************************************************

COMPUTE q22_1_velha=q22_1.
COMPUTE q22_2_velha=q22_2.
COMPUTE q22_3_velha=q22_3.
COMPUTE q22_4_velha=q22_4.
COMPUTE q22_5_velha=q22_5.
EXECUTE.

DO IF (q21=0).
RECODE q22_1 (ELSE=SYSMIS).
RECODE q22_2 (ELSE=SYSMIS).
RECODE q22_3 (ELSE=SYSMIS).
RECODE q22_4 (ELSE=SYSMIS).
RECODE q22_5 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q24******************************************************************

COMPUTE q25_velha=q25.
COMPUTE q26_velha=q26.
COMPUTE q27_velha=q27.
COMPUTE q28_velha=q28.
COMPUTE q29_velha=q29.
COMPUTE q30_1_velha=q30_1.
COMPUTE q30_2_velha=q30_2.
COMPUTE q30_3_velha=q30_3.
COMPUTE q30_4_velha=q30_4.
COMPUTE q30_5_velha=q30_5.
COMPUTE q30_6_velha=q30_6.
COMPUTE q30_0_velha=q30_0.
EXECUTE.

DO IF (q24=0).
RECODE q25 (ELSE=SYSMIS).
RECODE q26 (ELSE=SYSMIS).
RECODE q27 (ELSE=SYSMIS).
RECODE q28 (ELSE=SYSMIS).
RECODE q29 (ELSE=SYSMIS).
RECODE q30_1 (ELSE=SYSMIS).
RECODE q30_2 (ELSE=SYSMIS).
RECODE q30_3 (ELSE=SYSMIS).
RECODE q30_4 (ELSE=SYSMIS).
RECODE q30_5 (ELSE=SYSMIS).
RECODE q30_6 (ELSE=SYSMIS).
RECODE q30_0 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q30******************************************************************

DO IF (q30_0=1).
RECODE q30_1 (ELSE=SYSMIS).
RECODE q30_2 (ELSE=SYSMIS).
RECODE q30_3 (ELSE=SYSMIS).
RECODE q30_4 (ELSE=SYSMIS).
RECODE q30_5 (ELSE=SYSMIS).
RECODE q30_6 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q31******************************************************************

COMPUTE q31_1_velha=q31_1.
COMPUTE q31_2_velha=q31_2.
COMPUTE q31_3_velha=q31_3.
COMPUTE q31_4_velha=q31_4.
COMPUTE q31_5_velha=q31_5.
COMPUTE q31_6_velha=q31_6.
COMPUTE q31_7_velha=q31_7.
COMPUTE q31_8_velha=q31_8.
COMPUTE q31_9_velha=q31_9.
COMPUTE q31_0_velha=q31_0.
EXECUTE.

DO IF (q31_0=1).
RECODE q31_1 (ELSE=SYSMIS).
RECODE q31_2 (ELSE=SYSMIS).
RECODE q31_3 (ELSE=SYSMIS).
RECODE q31_4 (ELSE=SYSMIS).
RECODE q31_5 (ELSE=SYSMIS).
RECODE q31_6 (ELSE=SYSMIS).
RECODE q31_7 (ELSE=SYSMIS).
RECODE q31_8 (ELSE=SYSMIS).
RECODE q31_9 (ELSE=SYSMIS).
RECODE q31_0 (ELSE=SYSMIS).
END IF. 
EXECUTE.




**q33******************************************************************

COMPUTE q33_velha=q33.
EXECUTE.



**q37******************************************************************

COMPUTE q38_1_velha=q38_1.
COMPUTE q38_2_velha=q38_2.
COMPUTE q38_3_velha=q38_3.
COMPUTE q38_4_velha=q38_4.
COMPUTE q38_5_velha=q38_5.
COMPUTE q38_6_velha=q38_6.
COMPUTE q38_7_velha=q38_7.
COMPUTE q38_8_velha=q38_8.
COMPUTE q38_99_velha=q38_99.
COMPUTE q39_1_velha=q39_1.
COMPUTE q39_2_velha=q39_2.
COMPUTE q39_3_velha=q39_3.
COMPUTE q39_4_velha=q39_4.
COMPUTE q39_5_velha=q39_5.
COMPUTE q39_6_velha=q39_6.
COMPUTE q39_7_velha=q39_7.
COMPUTE q39_8_velha=q39_8.
COMPUTE q39_9_velha=q39_9.
COMPUTE q39_10_velha=q39_10.
COMPUTE q39_11_velha=q39_11.
COMPUTE q39_12_velha=q39_12.
COMPUTE q39_99_velha=q39_99.
EXECUTE.

DO IF (q37=0).
RECODE q38_1 (ELSE=SYSMIS).
RECODE q38_2 (ELSE=SYSMIS).
RECODE q38_3 (ELSE=SYSMIS).
RECODE q38_4 (ELSE=SYSMIS).
RECODE q38_5 (ELSE=SYSMIS).
RECODE q38_6 (ELSE=SYSMIS).
RECODE q38_7 (ELSE=SYSMIS).
RECODE q38_8 (ELSE=SYSMIS).
RECODE q38_99 (ELSE=SYSMIS).
RECODE q39_0 (ELSE=SYSMIS).
RECODE q39_1 (ELSE=SYSMIS).
RECODE q39_2 (ELSE=SYSMIS).
RECODE q39_3 (ELSE=SYSMIS).
RECODE q39_4 (ELSE=SYSMIS).
RECODE q39_5 (ELSE=SYSMIS).
RECODE q39_6 (ELSE=SYSMIS).
RECODE q39_7 (ELSE=SYSMIS).
RECODE q39_8 (ELSE=SYSMIS).
RECODE q39_9 (ELSE=SYSMIS).
RECODE q39_10 (ELSE=SYSMIS).
RECODE q39_11 (ELSE=SYSMIS).
RECODE q39_12 (ELSE=SYSMIS).
RECODE q39_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q39***********************************************************************************************************

DO IF (q39_0=1).
RECODE q39_1 (ELSE=SYSMIS).
RECODE q39_2 (ELSE=SYSMIS).
RECODE q39_3 (ELSE=SYSMIS).
RECODE q39_4 (ELSE=SYSMIS).
RECODE q39_5 (ELSE=SYSMIS).
RECODE q39_6 (ELSE=SYSMIS).
RECODE q39_7 (ELSE=SYSMIS).
RECODE q39_8 (ELSE=SYSMIS).
RECODE q39_9 (ELSE=SYSMIS).
RECODE q39_10 (ELSE=SYSMIS).
RECODE q39_11 (ELSE=SYSMIS).
RECODE q39_12 (ELSE=SYSMIS).
RECODE q39_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.


**q40***********************************************************************************************************


COMPUTE q40_1_velha=q40_1.
COMPUTE q40_2_velha=q40_2.
COMPUTE q40_3_velha=q40_3.
COMPUTE q40_4_velha=q40_4.
COMPUTE q40_5_velha=q40_5.
COMPUTE q40_6_velha=q40_6.
COMPUTE q40_7_velha=q40_7.
COMPUTE q40_8_velha=q40_8.
COMPUTE q40_9_velha=q40_9.
COMPUTE q40_99_velha=q40_99.
COMPUTE q40_0_velha=q40_0.
EXECUTE.

DO IF (q40_0=1).
RECODE q40_1 (ELSE=SYSMIS).
RECODE q40_2 (ELSE=SYSMIS).
RECODE q40_3 (ELSE=SYSMIS).
RECODE q40_4 (ELSE=SYSMIS).
RECODE q40_5 (ELSE=SYSMIS).
RECODE q40_6 (ELSE=SYSMIS).
RECODE q40_7 (ELSE=SYSMIS).
RECODE q40_8 (ELSE=SYSMIS).
RECODE q40_9 (ELSE=SYSMIS).
RECODE q40_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.





*********************************q43 e q44



COMPUTE q43_velha=q43.
COMPUTE q44_velha=q44.
execute.




**q45***********************************************************************************************************


COMPUTE q45_1_1_velha=q45_1_1.
COMPUTE q45_1_2_velha=q45_1_2.
COMPUTE q45_1_3_velha=q45_1_3.
COMPUTE q45_1_4_velha=q45_1_4.
COMPUTE q45_1_0_velha=q45_1_0.
COMPUTE q45_2_1_velha=q45_2_1.
COMPUTE q45_2_2_velha=q45_2_2.
COMPUTE q45_2_3_velha=q45_2_3.
COMPUTE q45_2_4_velha=q45_2_4.
COMPUTE q45_2_0_velha=q45_2_0.
COMPUTE q45_3_1_velha=q45_3_1.
COMPUTE q45_3_2_velha=q45_3_2.
COMPUTE q45_3_3_velha=q45_3_3.
COMPUTE q45_3_4_velha=q45_3_4.
COMPUTE q45_3_0_velha=q45_3_0.
COMPUTE q45_4_1_velha=q45_4_1.
COMPUTE q45_4_2_velha=q45_4_2.
COMPUTE q45_4_3_velha=q45_4_3.
COMPUTE q45_4_4_velha=q45_4_4.
COMPUTE q45_4_0_velha=q45_4_0.
COMPUTE q45_5_1_velha=q45_5_1.
COMPUTE q45_5_2_velha=q45_5_2.
COMPUTE q45_5_3_velha=q45_5_3.
COMPUTE q45_5_4_velha=q45_5_4.
COMPUTE q45_5_0_velha=q45_5_0.
COMPUTE q45_6_1_velha=q45_6_1.
COMPUTE q45_6_2_velha=q45_6_2.
COMPUTE q45_6_3_velha=q45_6_3.
COMPUTE q45_6_4_velha=q45_6_4.
COMPUTE q45_6_0_velha=q45_6_0.
COMPUTE q45_7_1_velha=q45_7_1.
COMPUTE q45_7_2_velha=q45_7_2.
COMPUTE q45_7_3_velha=q45_7_3.
COMPUTE q45_7_4_velha=q45_7_4.
COMPUTE q45_7_0_velha=q45_7_0.
COMPUTE q45_8_1_velha=q45_8_1.
COMPUTE q45_8_2_velha=q45_8_2.
COMPUTE q45_8_3_velha=q45_8_3.
COMPUTE q45_8_4_velha=q45_8_4.
COMPUTE q45_8_0_velha=q45_8_0.
EXECUTE.

DO IF (q45_1_0=1).
RECODE q45_1_1 (ELSE=SYSMIS).
RECODE q45_1_2 (ELSE=SYSMIS).
RECODE q45_1_3 (ELSE=SYSMIS).
RECODE q45_1_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.

DO IF (q45_2_0=1).
RECODE q45_2_1 (ELSE=SYSMIS).
RECODE q45_2_2 (ELSE=SYSMIS).
RECODE q45_2_3 (ELSE=SYSMIS).
RECODE q45_2_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.


DO IF (q45_3_0=1).
RECODE q45_3_1 (ELSE=SYSMIS).
RECODE q45_3_2 (ELSE=SYSMIS).
RECODE q45_3_3 (ELSE=SYSMIS).
RECODE q45_3_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.

DO IF (q45_4_0=1).
RECODE q45_4_1 (ELSE=SYSMIS).
RECODE q45_4_2 (ELSE=SYSMIS).
RECODE q45_4_3 (ELSE=SYSMIS).
RECODE q45_4_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.


DO IF (q45_5_0=1).
RECODE q45_5_1 (ELSE=SYSMIS).
RECODE q45_5_2 (ELSE=SYSMIS).
RECODE q45_5_3 (ELSE=SYSMIS).
RECODE q45_5_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.

DO IF (q45_6_0=1).
RECODE q45_6_1 (ELSE=SYSMIS).
RECODE q45_6_2 (ELSE=SYSMIS).
RECODE q45_6_3 (ELSE=SYSMIS).
RECODE q45_6_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.


DO IF (q45_7_0=1).
RECODE q45_7_1 (ELSE=SYSMIS).
RECODE q45_7_2 (ELSE=SYSMIS).
RECODE q45_7_3 (ELSE=SYSMIS).
RECODE q45_7_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.


DO IF (q45_8_0=1).
RECODE q45_8_1 (ELSE=SYSMIS).
RECODE q45_8_2 (ELSE=SYSMIS).
RECODE q45_8_3 (ELSE=SYSMIS).
RECODE q45_8_4 (ELSE=SYSMIS).
END IF. 
EXECUTE.


**q46***********************************************************************************************************

COMPUTE q47_velha=q47.

DO IF (q46=1).
RECODE q47 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q49***********************************************************************************************************


COMPUTE q50_1_velha=q50_1.
COMPUTE q50_2_velha=q50_2.
COMPUTE q50_3_velha=q50_3.
COMPUTE q50_4_velha=q50_4.
COMPUTE q50_5_velha=q50_5.
COMPUTE q50_6_velha=q50_6.
COMPUTE q50_7_velha=q50_7.
COMPUTE q50_8_velha=q50_8.
COMPUTE q50_9_velha=q50_9.
COMPUTE q50_10_velha=q50_10.
COMPUTE q50_11_velha=q50_11.
COMPUTE q50_12_velha=q50_12.
COMPUTE q50_13_velha=q50_13.
COMPUTE q50_14_velha=q50_14.
COMPUTE q50_15_velha=q50_15.
COMPUTE q50_99_velha=q50_99.
EXECUTE.


DO IF (q49=0).
RECODE q50_1 (ELSE=SYSMIS).
RECODE q50_2 (ELSE=SYSMIS).
RECODE q50_3 (ELSE=SYSMIS).
RECODE q50_4 (ELSE=SYSMIS).
RECODE q50_5 (ELSE=SYSMIS).
RECODE q50_6 (ELSE=SYSMIS).
RECODE q50_7 (ELSE=SYSMIS).
RECODE q50_8 (ELSE=SYSMIS).
RECODE q50_9 (ELSE=SYSMIS).
RECODE q50_10 (ELSE=SYSMIS).
RECODE q50_11 (ELSE=SYSMIS).
RECODE q50_12 (ELSE=SYSMIS).
RECODE q50_13 (ELSE=SYSMIS).
RECODE q50_14 (ELSE=SYSMIS).
RECODE q50_15 (ELSE=SYSMIS).
RECODE q50_99 (ELSE=SYSMIS).
END IF. 
EXECUTE.



COMPUTE q51_1_4_velha=q51_1_4.
execute.



**q52***********************************************************************************************************

COMPUTE q53_velha=q53.

COMPUTE q54_velha=q54.


COMPUTE q53_98velha=q53_98.

COMPUTE q54_98velha=q54_98.

EXECUTE.


DO IF (q52=0).
RECODE q53 (ELSE=SYSMIS).
RECODE q54 (ELSE=SYSMIS).
RECODE q53_98 (ELSE=SYSMIS).
RECODE q54_98 (ELSE=SYSMIS).
END IF. 
EXECUTE.


**q55***********************************************************************************************************

COMPUTE q56_1_velha=q56_1.
COMPUTE q56_2_velha=q56_2.
EXECUTE.


DO IF (q55=0).
RECODE q56_1 (ELSE=SYSMIS).
RECODE q56_2  (ELSE=SYSMIS).
END IF. 
EXECUTE.



**q57***********************************************************************************************************

COMPUTE q58_velha=q58.
EXECUTE.


DO IF (q57=0).
RECODE q59 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q59***********************************************************************************************************

COMPUTE q60_velha=q60.
EXECUTE.


DO IF (q59=0).
RECODE q60 (ELSE=SYSMIS).
END IF. 
EXECUTE.

**q61***********************************************************************************************************

COMPUTE q62_velha=q62.
COMPUTE q63_velha=q63.
COMPUTE q64_velha=q64.
EXECUTE.


DO IF (q61=0).
RECODE q62 (ELSE=SYSMIS).
RECODE q63 (ELSE=SYSMIS).
RECODE q64 (ELSE=SYSMIS).
END IF. 
EXECUTE.


DO IF (q63=0).
RECODE q64 (ELSE=SYSMIS).
END IF. 
EXECUTE.



VARIABLE LABELS
q2_1_velha'q2_1_v11144_1_2. A qual secretaria a assist�ncia social est� associada?1: Educa��o'
q2_2_velha'q2_2_v11144_2_2. A qual secretaria a assist�ncia social est� associada?2: Planejamento'
q2_3_velha'q2_3_v11144_3_2. A qual secretaria a assist�ncia social est� associada?3: Sa�de'
q2_4_velha'q2_4_v11144_4_2. A qual secretaria a assist�ncia social est� associada?4: Habita��o'
q2_5_velha'q2_5_v11144_5_2. A qual secretaria a assist�ncia social est� associada?5: Seguran�a Alimentar'
q2_6_velha'q2_6_v11144_6_2. A qual secretaria a assist�ncia social est� associada?6: Trabalho e/ou Emprego'
q2_7_velha'q2_7_v11144_7_2. A qual secretaria a assist�ncia social est� associada?7: Direitos Humanos'
q2_99_velha'q2_99_v11144_99_2. A qual secretaria a assist�ncia social est� associada?99: Outra'
q4_velha'q4_v13077_4. No caso de haver subdivis�o administrativa de Prote��o Social Especial, ela � desmembrada entre Alta e M�dia Complexidade?'
q6_1_velha'q6_1_v13079_1_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:1: Conceitua��o da assist�ncia social'
q6_2_velha'q6_2_v13079_2_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:2: Fun��es da assist�ncia social (Vigil�ncia Socioassistencial, Prote��o Social, Defesa de Direitos)'
q6_3_velha'q6_3_v13079_3_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:3: Princ�pios pelos quais a Assist�ncia Social � regida'
q6_4_velha'q6_4_v13079_4_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:4: Diretrizes para organiza��o da Assist�ncia Social'
q6_5_velha'q6_5_v13079_5_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:5: Organiza��o da assist�ncia social por n�veis de Prote��o Social'
q6_6_velha'q6_6_v13079_6_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:6: Conceitua��o e Objetivos do SUAS'
q6_7_velha'q6_7_v13079_7_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:7: P�blico destinat�rio do SUAS'
q6_8_velha'q6_8_v13079_8_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:8: Previs�o do financiamento dos servi�os'
q6_9_velha'q6_9_v13079_9_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:9: Previs�o do financiamento dos programas e projetos'
q6_10_velha'q6_10_v13079_10_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:10: Previs�o do financiamento dos benef�cios socioassistenciais'
q6_11_velha'q6_11_v13079_11_6. Conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:11: Especifica��o do �rg�o da Administra��o P�blica respons�vel pela coordena��o da Pol�tica de Assist�ncia Social'
q6_12_velha'q6_12_v13079_12_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:12: Previs�o de prazo para regulamenta��o do funcionamento do Fundo de Assist�ncia Social'
q6_13_velha'q6_13_v13079_13_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:13: Especifica��o e/ou conceitua��o dos equipamentos p�blicos estatais CRAS e CREAS'
q6_14_velha'q6_14_v13079_14_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:14: Especifica��o e/ou conceitua��o dos servi�os socioassistenciais'
q6_15_velha'q6_15_v13079_15_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:15: Especifica��o, conceitua��o e/ou formas de concess�o dos benef�cios eventuais'
q6_16_velha'q6_16_v13079_16_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:16: Cria��o e/ou compet�ncias do Conselho Municipal de Assist�ncia Social'
q6_17_velha'q6_17_v13079_17_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:17: Cria��o do Fundo Municipal de Assist�ncia Social'
q6_18_velha'q6_18_v13079_18_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:18: Institui��o do Plano Municipal de Assist�ncia Social'
q6_19_velha'q6_19_v13079_19_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:19: Organiza��o das Confer�ncias Municipais de Assist�ncia Social'
q6_20_velha'q6_20_v13079_20_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:20: Inst�ncias de pactua��o'
q6_21_velha'q6_21_v13079_21_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:21: Rela��o com as Organiza��es da Sociedade Civil'
q6_22_velha'q6_22_v13079_22_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:22: Organiza��o dos Recursos Humanos (cargos e sal�rios, gratifica��es, educa��o permanente, entre outros)'
q6_99_velha'q6_99_v13079_99_6. Informe o conte�do previsto na Lei Municipal de regulamenta��o do Sistema �nico de Assist�ncia Social � SUAS:99: Outros'
q12_velha'q12_v11340_12. Caso sim, quantas pessoas?'
q13_velha'q13_v13084_13. Este quantitativo considera:'
q14_velha'q14_v11342_14. Como foi realizado este levantamento ou pesquisa?'
q16_velha'q16_v13200_16. Em qual(is) unidade(s) e com que equipe o Servi�o � ofertado?'
q17_velha'q17_v13201_17. Quantas pessoas foram atendidas pelo Servi�o de Prote��o Social B�sica no Domic�lio para Pessoas com defici�ncia e idosas no munic�pio, em Agosto/2019?'
q18_1_velha'q18_1_v11196_1_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?1: Atende no CREAS do munic�pio'
q18_2_velha'q18_2_v11196_2_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?2: Encaminha para o CREAS de outro munic�pio'
q18_3_velha'q18_3_v11196_3_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?3: Encaminha para o CREAS Regional ao qual est� vinculado'
q18_4_velha'q18_4_v11196_4_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?4: Atende no CRAS'
q18_5_velha'q18_5_v11196_5_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?5: Atende em entidade ou organiza��o da sociedade civil no munic�pio'
q18_6_velha'q18_6_v11196_6_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?6: � atendido pela equipe de refer�ncia (ou t�cnica(o) do �rg�o gestor'
q18_99_velha'q18_99_v11196_99_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?99: Outro'
q18_0_velha'q18_0_v11196_0_18. Como a Assist�ncia Social atende quest�es de viola��es de direitos no munic�pio?0: N�o � atendido pela Assist�ncia Social'
q19_1_velha'q19_1_v14658_1_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?1: Atende no CREAS do munic�pio'
q19_2_velha'q19_2_v14658_2_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?2: Encaminha para o CREAS de outro munic�pio'
q19_3_velha'q19_3_v14658_3_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?3: Encaminha para o CREAS Regional ao qual est� vinculado'
q19_4_velha'q19_4_v14658_4_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?4: Executa no CRAS'
q19_5_velha'q19_5_v14658_5_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?5: Executa em entidade no munic�pio'
q19_6_velha'q19_6_v14658_6_19. Como o munic�pio atende adolescentes em cumprimento de MSE-  LA e PSC?6: Executa em outra unidade p�blica (exceto CREAS) de acompanhamento de adolescentes em cumprimento de Medida Socioeducativa (LA ou PSC) do munic�pio'
q19_7_velha'q19_7_v14658_7_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?7: � atendido pela equipe de refer�ncia (ou t�cnico) do �rg�o gestor (responda a quest�o 20)'
q19_99_velha'q19_99_v14658_99_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?99: Outro'
q19_0_velha'q19_0_v14658_0_19. Como o munic�pio atende adolescentes em cumprimento de Medida Socioeducativa de LA e PSC?0: N�o atende'
q20_1_velha'q20_1_v14662_1_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?1: Atendimento/acompanhamento direto �s fam�lias e indiv�duos'
q20_2_velha'q20_2_v14662_2_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?2: Elabora��o dos Planos de Acompanhamento Familiar ou Individual'
q20_3_velha'q20_3_v14662_3_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?3: Elabora��o de relat�rios t�cnicos sobre as fam�lias/indiv�duos em acompanhamento'
q20_4_velha'q20_4_v14662_4_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?4: Registro do atendimento/acompanhamento em prontu�rio'
q20_5_velha'q20_5_v14662_5_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?5: Encaminhamento dos usu�rios para a rede de servi�os local'
q20_6_velha'q20_6_v14662_6_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?6: Articula��o da rede de pol�ticas setoriais no munic�pio'
q20_7_velha'q20_7_v14662_7_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?7: Articula��o com os �rg�os do sistema de justi�a do munic�pio'
q20_8_velha'q20_8_v14662_8_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?8: Busca ativa'
q20_0_velha'q20_0_v14662_0_20. Quais atividades a(o) t�cnica(o) ou a equipe de refer�ncia do �rg�o gestor realiza?0: Nenhuma das anteriores'
q22_1_velha'q22_1_v14664_1_22. Em qual(is) unidade(s) o servi�o � ofertado?1: No(s) CREAS'
q22_2_velha'q22_2_v14664_2_22. Em qual(is) unidade(s) o servi�o � ofertado?2: No(s) Centro POP'
q22_3_velha'q22_3_v14664_3_22. Em qual(is) unidade(s) o servi�o � ofertado?3: Na sede/�rg�o gestor do munic�pio (considerar nesta op��o estruturas administrativas descentralizadas, como regionais ou similares)'
q22_4_velha'q22_4_v14664_4_22. Em qual(is) unidade(s) o servi�o � ofertado?4: Em outra unidade p�blica. Se sim, quantas unidades?'
q22_5_velha'q22_5_v14664_5_22. Em qual(is) unidade(s) o servi�o � ofertado?5: Em entidade ou Organiza��o da Sociedade Civil Se sim, quantas unidades?'
q25_velha'q25_v14669_25. Caso sim, o Programa de apoio � fam�lia guardi� na fam�lia extensa (Guarda Subsidiada) � regulamentado por lei municipal?'
q26_velha'q26_v14670_26. O Programa de apoio � fam�lia guardi� na fam�lia extensa (Guarda Subsidiada) � ofertado por unidade governamental ou por Organiza��o da Sociedade Civil (OSC)?'
q27_velha'q27_v14671_27. Atualmente, quantas crian�as e/ou adolescente est�o acolhidos em fam�lias guardi�s na fam�lia extensa inseridas no Programa?'
q28_velha'q28_v14672_28. Atualmente, quantas fam�lias recebem recursos pelo Programa? (Se n�o tiver, preencher �0�)'
q29_velha'q29_v14674_29. Qual � o valor m�dio de repasse a estas fam�lias?'
q30_1_velha'q30_1_v13140_1_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 1: Visitas domiciliares da equipe t�cnica do Programa � fam�lia'
q30_2_velha'q30_2_v13140_2_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 2: Reuni�es com grupos de fam�lias'
q30_3_velha'q30_3_v13140_3_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 3: Atendimento psicossocial individualizado'
q30_4_velha'q30_4_v13140_4_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 4: Palestras / oficinas'
q30_5_velha'q30_5_v13140_5_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 5: Elabora��o de relat�rios t�cnicos sobre casos em acompanhamento'
q30_6_velha'q30_6_v13140_6_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 6: Envio de relat�rios para o Judici�rio'
q30_0_velha'q30_0_v13140_0_30. Quais atividades s�o realizadas pelo Programa de Guarda Subsidiada do munic�pio? 0: N�o realiza nenhuma das atividades acima'
q31_1_velha'q31_1_v14675_1_31. Acolhimento de crian�as/adolescentes,gest�o e monitoramento?1: Controla o acesso �s vagas (controle da porta de entrada), sendo respons�vel por indicar a Unidade de Acolhimento que receber� a crian�a/adolescente'
q31_2_velha'q31_2_v14675_2_31. Acolhimento de crian�as/adolescentes, atividades de gest�o e monitoramento realizadas pelo �rg�o gestor da Assist�ncia Social?2: Acompanha/monitora o fluxo de entradas e sa�das de crian�as e adolescentes nas Unidades'
q31_3_velha'q31_3_v14675_3_31. Acolhimento de crian�as/adolescentes, atividades de gest�o e monitoramento realizadas pelo �rg�o gestor da Assist�ncia Social?3: Centraliza as informa��es das medidas de acolhimento determinadas pelo poder Judici�rio'
q31_4_velha'q31_4_v14675_4_31. Acolhimento de crian�as/adolescentes, atividades de gest�o e monitoramento realizadas pelo �rg�o gestor da Assist�ncia Social?4: Centraliza as informa��es dos acolhimentos emergenciais realizados pelo Conselho Tutelar'
q31_5_velha'q31_5_v14675_5_31. Acolhimento de crian�as/adolescentes, atividades de gest�o e monitoramento realizada pelo �rg�o gestor da AS?5: Promove a articula��o dos servi�os de acolhimento com os demais servi�os da rede socioassistencial'
q31_6_velha'q31_6_v14675_6_31. Acolhimento de crian�as/adolescentes, gest�o e monitoramento realizada pelo �rg�o gestor da AS?6: Promove a articula��o dos servi�os de acolhimento com as demais pol�ticas p�blicas e �rg�os de defesa de direitos'
q31_7_velha'q31_7_v14675_7_31. Nos casos de acolhimento de crian�as e adolescentes, quais atividades de gest�o e monitoramento s�o realizadas pelo �rg�o gestor da Assist�ncia Social?7: Realiza supervis�o e suporte t�cnico aos servi�os de acolhimento'
q31_8_velha'q31_8_v14675_8_31. Acolhimento de crian�as/adolescentes, quais atividades de gest�o e monitoramento realizada pelo �rg�o gestor da AS?8: Monitora o tempo de perman�ncia das crian�as e adolescentes nos servi�os de acolhimento'
q31_9_velha'q31_9_v14675_9_31. Nos casos de acolhimento de crian�as e adolescentes, quais atividades de gest�o e monitoramento s�o realizadas pelo �rg�o gestor da Assist�ncia Social?9: Fiscaliza a qualidade dos servi�os'
q31_0_velha'q31_0_v14675_0_31. Nos casos de acolhimento de crian�as e adolescentes, quais atividades de gest�o e monitoramento s�o realizadas pelo �rg�o gestor da Assist�ncia Social?0: N�o realiza nenhuma das atividades listadas acima'
q38_1_velha'q38_1_v13166_1_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?1: Servi�o Nacional do Emprego � SINE'
q38_2_velha'q38_2_v13166_2_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?2: Cursos do PRONATEC'
q38_3_velha'q38_3_v13166_3_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?3: Cursos t�cnicos de institui��es municipais/estaduais'
q38_4_velha'q38_4_v13166_4_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?4: Cooperativas'
q38_5_velha'q38_5_v13166_5_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?5: Microcr�dito'
q38_6_velha'q38_6_v13166_6_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?6: Micro empreendedorismo individual'
q38_7_velha'q38_7_v13166_7_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?7: Sistema S (SESC, SENAC, SESI, SENAR, etc)'
q38_8_velha'q38_8_v13166_8_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?8: Organiza��es de trabalhadores (sindicatos, conselhos, etc)'
q38_99_velha'q38_99_v13166_99_38. Para quais oportunidades do mundo do trabalho o munic�pio encaminha, de forma rotineira, as(os) usu�rias(os) da assist�ncia social?99: Outros'
q39_1_velha'q39_1_v13186_1_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?1: Pessoas inscritas no CAD�NICO;'
q39_2_velha'q39_2_v13186_2_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?2: Benefici�rias(os) do Programa Bolsa Fam�lia;'
q39_3_velha'q39_3_v13186_3_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?3: Pessoas com defici�ncia;'
q39_4_velha'q39_4_v13186_4_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?4: Pessoas do Servi�o de Conviv�ncia e Fortalecimento de V�nculos;'
q39_5_velha'q39_5_v13186_5_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?5: Adolescentes e jovens no sistema socioeducativo e egressas(os);'
q39_6_velha'q39_6_v13186_6_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?6: Adultas(os) em Fam�lias com presen�a de trabalho infantil;'
q39_7_velha'q39_7_v13186_7_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?7: Fam�lias com pessoas em situa��o de priva��o de liberdade;'
q39_8_velha'q39_8_v13186_8_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?8: Indiv�duos egressos do sistema penal;'
q39_9_velha'q39_9_v13186_9_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?9: Fam�lias com crian�as em situa��o de acolhimento provis�rio;'
q39_10_velha'q39_10_v13186_10_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?10: Adolescentes e jovens no servi�o de acolhimento e egressas(os);'
q39_11_velha'q39_11_v13186_11_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?11: Popula��o em Situa��o de Rua;'
q39_12_velha'q39_12_v13186_12_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?12: Povos e comunidades tradicionais;'
q39_99_velha'q39_99_v13186_99_39. Existe delimita��o de usu�rias(os) priorit�rias(os) para o atendimento do Acessuas Trabalho?99: Outros'
q40_1_velha'q40_1_v13187_1_40. Especifique a��es relativas ao BPC s�o desenvolvidas pela Assist�ncia Social no munic�pio:1: Realizar estudos sobre as condi��es de vulnerabilidade das fam�lias com pessoas idosas e com defici�ncia'
q40_2_velha'q40_2_v13187_2_40. Especifique a��es relativas ao Benef�cio de Presta��o Continuada (BPC) s�o desenvolvidas pela Assist�ncia Social no munic�pio:2: Orientar a popula��o usu�ria sobre seus direitos e procedimentos para acesso ao BPC'
q40_3_velha'q40_3_v13187_3_40. Especifique quais as a��es relativas ao Benef�cio de Presta��o Continuada (BPC) s�o desenvolvidas pela Assist�ncia Social no munic�pio:3: Apoiar e acompanhar o processo de concess�o do BPC'
q40_4_velha'q40_4_v13187_4_40. Especifique a��es relativas ao BPC s�o desenvolvidas pela Assist�ncia Social no munic�pio:4: Distribuir aos CRAS e CREAS do munic�pio listas territorializadas das fam�lias com benefici�rias(os) do BPC'
q40_5_velha'q40_5_v13187_5_40. Especifique a��es relativas ao BPC s�o desenvolvidas pela Assist�ncia Social no munic�pio:5: Articular com outros setores a inser��o das(os) benefici�rias(os) nas diversas pol�ticas sociais'
q40_6_velha'q40_6_v13187_6_40. Especifique  a��es relativas ao BPC s�o desenvolvidas pela Assist�ncia Social no munic�pio:6: Realizar e acompanhar a inser��o e atualiza��o das(os) benefici�rias(os) do BPC no Cadastro �nico'
q40_7_velha'q40_7_v13187_7_40. Especifique quais as a��es relativas ao Benef�cio de Presta��o Continuada (BPC) s�o desenvolvidas pela Assist�ncia Social no munic�pio:7: Articular a��es com INSS acerca do requerimento e manuten��o do BPC'
q40_8_velha'q40_8_v13187_8_40. A��es relativas ao BPC desenvolvidas pela AS no munic�pio:8: Realizar e acompanhar a inser��o de crian�as e adolescentes com defici�ncia benefici�rias do BPC na escola, em articula��o com o Grupo Gestor Local'
q40_9_velha'q40_9_v13187_9_40. Especifique  a��es relativas ao (BPC s�o desenvolvidas pela Assist�ncia Social no munic�pio:9: Acompanhar a inser��o de jovens e adultas(os) com defici�ncia benefici�rias(os) do BPC no mundo do trabalho'
q40_99_velha'q40_99_v13187_99_40. Especifique quais as a��es relativas ao Benef�cio de Presta��o Continuada (BPC) s�o desenvolvidas pela Assist�ncia Social no munic�pio:99: Outros'
q40_0_velha'q40_0_v13187_0_40. Especifique quais as a��es relativas ao Benef�cio de Presta��o Continuada (BPC) s�o desenvolvidas pela Assist�ncia Social no munic�pio:0: N�o realiza'
q45_1_1_velha'q45_1_1_v13190_1_45.1. Na sede do �rg�o gestor/Secretaria de Assist�ncia1: Entrevista para Inclus�o Cadastral'
q45_1_2_velha'q45_1_2_v13190_2_45.1. Na sede do �rg�o gestor/Secretaria de Assist�ncia2: Entrevista para Atualiza��o Cadastral'
q45_1_3_velha'q45_1_3_v13190_3_45.1. Na sede do �rg�o gestor/Secretaria de Assist�ncia3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_1_4_velha'q45_1_4_v13190_4_45.1. Na sede do �rg�o gestor/Secretaria de Assist�ncia4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_1_0_velha'q45_1_0_v13190_0_45.1. Na sede do �rg�o gestor/Secretaria de Assist�ncia0: N�o realiza'
q45_2_1_velha'q45_2_1_v14693_1_45.2. Nos CRAS1: Entrevista para Inclus�o Cadastral'
q45_2_2_velha'q45_2_2_v14693_2_45.2. Nos CRAS2: Entrevista para Atualiza��o Cadastral'
q45_2_3_velha'q45_2_3_v14693_3_45.2. Nos CRAS3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_2_4_velha'q45_2_4_v14693_4_45.2. Nos CRAS4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_2_0_velha'q45_2_0_v14693_0_45.2. Nos CRAS0: N�o realiza'
q45_3_1_velha'q45_3_1_v14694_1_45.3. Em unidades/postos fixos exclusivos para cadastramento1: Entrevista para Inclus�o Cadastral'
q45_3_2_velha'q45_3_2_v14694_2_45.3. Em unidades/postos fixos exclusivos para cadastramento2: Entrevista para Atualiza��o Cadastral'
q45_3_3_velha'q45_3_3_v14694_3_45.3. Em unidades/postos fixos exclusivos para cadastramento3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_3_4_velha'q45_3_4_v14694_4_45.3. Em unidades/postos fixos exclusivos para cadastramento4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_3_0_velha'q45_3_0_v14694_0_45.3. Em unidades/postos fixos exclusivos para cadastramento0: N�o realiza'
q45_4_1_velha'q45_4_1_v14695_1_45.4. Em unidades m�veis ou postos tempor�rios1: Entrevista para Inclus�o Cadastral'
q45_4_2_velha'q45_4_2_v14695_2_45.4. Em unidades m�veis ou postos tempor�rios2: Entrevista para Atualiza��o Cadastral'
q45_4_3_velha'q45_4_3_v14695_3_45.4. Em unidades m�veis ou postos tempor�rios3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_4_4_velha'q45_4_4_v14695_4_45.4. Em unidades m�veis ou postos tempor�rios4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_4_0_velha'q45_4_5_v14695_0_45.4. Em unidades m�veis ou postos tempor�rios0: N�o realiza'
q45_5_1_velha'q45_5_1_v14696_1_45.5. Em outras unidades da Assist�ncia Social1: Entrevista para Inclus�o Cadastral'
q45_5_2_velha'q45_5_2_v14696_2_45.5. Em outras unidades da Assist�ncia Social2: Entrevista para Atualiza��o Cadastral'
q45_5_3_velha'q45_5_3_v14696_3_45.5. Em outras unidades da Assist�ncia Social3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_5_4_velha'q45_5_4_v14696_4_45.5. Em outras unidades da Assist�ncia Social4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_5_0_velha'q45_5_0_v14696_0_45.5. Em outras unidades da Assist�ncia Social0: N�o realiza'
q45_6_1_velha'q45_6_1_v14697_1_45.6. Em unidades de outras pol�ticas p�blicas1: Entrevista para Inclus�o Cadastral'
q45_6_2_velha'q45_6_2_v14697_2_45.6. Em unidades de outras pol�ticas p�blicas2: Entrevista para Atualiza��o Cadastral'
q45_6_3_velha'q45_6_3_v14697_3_45.6. Em unidades de outras pol�ticas p�blicas3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_6_4_velha'q45_6_4_v14697_4_45.6. Em unidades de outras pol�ticas p�blicas4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_6_0_velha'q45_6_0_v14697_0_45.6. Em unidades de outras pol�ticas p�blicas0: N�o realiza'
q45_7_1_velha'q45_7_1_v14698_1_45.7. No domic�lio das fam�lias1: Entrevista para Inclus�o Cadastral'
q45_7_2_velha'q45_7_2_v14698_2_45.7. No domic�lio das fam�lias2: Entrevista para Atualiza��o Cadastral'
q45_7_3_velha'q45_7_3_v14698_3_45.7. No domic�lio das fam�lias3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_7_4_velha'q45_7_4_v14698_4_45.7. No domic�lio das fam�lias4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_7_0_velha'q45_7_0_v14698_0_45.7. No domic�lio das fam�lias0: N�o realiza'
q45_8_1_velha'q45_8_1_v14699_1_45.8. Outros1: Entrevista para Inclus�o Cadastral'
q45_8_2_velha'q45_8_2_v14699_2_45.8. Outros2: Entrevista para Atualiza��o Cadastral'
q45_8_3_velha'q45_8_3_v14699_3_45.8. Outros3: Digita��o dos dados das fam�lias no Sistema de Cadastro �nico'
q45_8_4_velha'q45_8_4_v14699_4_45.8. Outros4: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q45_8_0_velha'q45_8_0_v14699_5_45.8. Outros0: N�o realiza'
q47_velha'q47_v13193_47. Em m�dia, ap�s a entrevista, quanto tempo leva para que os dados coletados no formul�rio em papel sejam inseridos no sistema?'
q50_1_velha'q50_1_v14801_1_50. conte�dos/temas das a��es de comunica��o Realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?1: Informa��o sobre direitos das(os) usu�rias(os) (Direito da pessoa idosa, da Mulher, de crian�as/adolescentes, etc)'
q50_2_velha'q50_2_v14801_2_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?2: BPC'
q50_3_velha'q50_3_v14801_3_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?3: Programa Bolsa Fam�lia'
q50_4_velha'q50_4_v14801_4_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?4: Cadastro �nico'
q50_5_velha'q50_5_v14801_5_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?5: Servi�os Socioassistenciais (CRAS, Acolhimento, etc)'
q50_6_velha'q50_6_v14801_6_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?6: Popula��o de Rua'
q50_7_velha'q50_7_v14801_7_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?7: Trabalho Infantil'
q50_8_velha'q50_8_v14801_8_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?8: Viol�ncia Dom�stica'
q50_9_velha'q50_9_v14801_9_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?9: Explora��o Sexual'
q50_10_velha'q50_10_v14801_10_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?10: Desigualdade de G�nero'
q50_11_velha'q50_11_v14801_11_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?11: Direito LGBT'
q50_12_velha'q50_12_v14801_12_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?12: Divulga��o de eventos t�cnicos'
q50_13_velha'q50_13_v14801_13_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?13: Divulga��o de Orienta��es T�cnicas'
q50_14_velha'q50_14_v14801_14_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?14: Divulga��o de Normativas'
q50_15_velha'q50_15_v14801_15_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?15: Controle Social'
q50_99_velha'q50_99_v14801_99_50. Indique os conte�dos/temas das a��es de comunica��o (campanha/ mobiliza��o/sensibiliza��o) realizadas pelo �rg�o gestor de Assist�ncia Social em 2018?99: Outros'
q53_velha'q53_v14791_53. Em que ano ocorreu a �ltima atualiza��o do Plano de Capacita��o e Educa��o Permanente?'
q54_velha'q54_v14793_54. Para o ano de 2019, qual foi a previs�o de quantidade de trabalhadores capacitados no plano?'
q56_1_velha'q56_1_v14795_56.1. Profissionais'
q56_2_velha'q56_2_v14708_56.2. Conselheiras(os) (n�o contabilizar os/as tutelares)'
q58_velha'q58_v14711_58. Em 2018, qual a quantidade de profissionais do munic�pio (SEDE E UNIDADES P�BLICAS) que participaram de cursos do CAPACITASUAS?'
q60_velha'q60_v14714_60. Com qual frequ�ncia ocorrem as reuni�es ordin�rias da Mesa de Negocia��o do SUAS?'
q62_velha'q62_v14718_62. Qual o ano da �ltima atualiza��o deste PCCS?'
q63_velha'q63_v15145_63. Existe Coordenador do Programa Crian�a Feliz no Munic�pio?'
q64_velha'q64_v15146_64. Qual a escolaridade do Coordenador Municipal do Programa Crian�a Feliz no Munic�pio?'
.







