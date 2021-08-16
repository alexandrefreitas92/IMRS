
****q1***********************************************************

compute q1_velha = q1. 
compute q1_1_velha = q1_1. 
compute q2_velha = q2. 
compute q3_velha = q3. 
execute. 

DO IF (MISSING(q1)).
RECODE q1_1 (ELSE=1).
END IF.
EXECUTE.


DO IF (q1=0).
RECODE q2 (ELSE=SYSMIS).
RECODE q3 (ELSE=SYSMIS).
END IF.
EXECUTE.

*******q2***************************

DO IF (q2=0).
RECODE q3 (ELSE=SYSMIS).
END IF.
EXECUTE.

*******q4***************************

compute q5_velha = q5. 
execute. 

DO IF (q4=0).
RECODE q5 (ELSE=SYSMIS).
END IF.
EXECUTE.

*******q7***************************

compute q8_1_velha = q8_1. 
compute q8_2_velha = q8_2. 
compute q8_3_velha = q8_3. 
compute q8_4_velha = q8_4. 
compute q8_5_velha = q8_5. 
compute q8_6_velha = q8_6. 
compute q8_7_velha = q8_7. 
compute q8_8_velha = q8_8. 
compute q8_9_velha = q8_9. 
compute q8_10_velha = q8_10. 
compute q8_11_velha = q8_11. 
compute q8_99_velha = q8_99. 
execute. 

DO IF (q7=0).
RECODE q8_1 (ELSE=SYSMIS).
RECODE q8_2 (ELSE=SYSMIS).
RECODE q8_3 (ELSE=SYSMIS).
RECODE q8_4 (ELSE=SYSMIS).
RECODE q8_5 (ELSE=SYSMIS).
RECODE q8_6 (ELSE=SYSMIS).
RECODE q8_7 (ELSE=SYSMIS).
RECODE q8_8 (ELSE=SYSMIS).
RECODE q8_9 (ELSE=SYSMIS).
RECODE q8_10 (ELSE=SYSMIS).
RECODE q8_11 (ELSE=SYSMIS).
RECODE q8_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

***q12*************************************************

compute q13_velha = q13. 
compute q14_1_velha = q14_1. 
compute q14_2_velha = q14_2. 
execute. 

DO IF (q12=0).
RECODE q13 (ELSE=SYSMIS).
RECODE q14_1 (ELSE=SYSMIS).
RECODE q14_2 (ELSE=SYSMIS).
END IF.
EXECUTE.

******q22**********************************************************************

compute q22_velha = q22. 
compute q22_98_velha = q22_98. 
execute. 

DO IF (MISSING(q22)).
RECODE q22_98 (ELSE=1).
END IF.
EXECUTE.

***q28******************************************************************

compute q28_0_velha = q28_0. 
compute q28_1_velha = q28_1. 
compute q28_2_velha = q28_2. 
compute q28_3_velha = q28_3. 
compute q28_4_velha = q28_4. 
compute q28_5_velha = q28_5. 
compute q28_6_velha = q28_6. 
compute q28_7_velha = q28_7. 
execute. 

DO IF (q28_0=1).
RECODE q28_1 (ELSE=SYSMIS).
RECODE q28_2 (ELSE=SYSMIS).
RECODE q28_3 (ELSE=SYSMIS).
RECODE q28_4 (ELSE=SYSMIS).
RECODE q28_5 (ELSE=SYSMIS).
RECODE q28_6 (ELSE=SYSMIS).
RECODE q28_7 (ELSE=SYSMIS).
END IF.
EXECUTE.

*****q30*************************************************************

compute q30_1_velha = q30_1. 
compute q30_2_velha = q30_2. 
compute q30_3_velha = q30_3. 
compute q30_4_velha = q30_4. 
compute q30_5_velha = q30_5. 
compute q30_99_velha = q30_99. 
execute. 

DO IF (q30_1=1).
RECODE q30_2 (ELSE=SYSMIS).
RECODE q30_3 (ELSE=SYSMIS).
RECODE q30_4 (ELSE=SYSMIS).
RECODE q30_5 (ELSE=SYSMIS).
RECODE q30_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

****q35*************************************************

compute q36_velha = q36. 
compute q36_98_velha = q36_98. 
execute. 

DO IF (q35=0).
RECODE q36 (ELSE=SYSMIS).
RECODE q36_98 (ELSE=SYSMIS).
END IF.
EXECUTE.

Do IF  (MISSING(q36)  & q35=1).
RECODE q36_98  (ELSE=1).
END IF.
EXECUTE.

*****q42***************************************************************************

compute q43_velha = q43. 
compute q44_velha = q44. 
execute. 

DO IF (q42=0).
RECODE q43 (ELSE=SYSMIS).
RECODE q44 (ELSE=SYSMIS).
END IF.
EXECUTE.

****q45**************************************************************************************

compute q45_1_velha = q45_1. 
compute q45_1_98_velha = q45_1_98. 
compute q45_2_velha = q45_2. 
compute q45_2_98_velha = q45_2_98. 
compute q45_3_velha = q45_3. 
compute q45_3_98_velha = q45_3_98. 
execute. 

DO IF (MISSING(q45_1)).
RECODE q45_1_98 (ELSE=1).
END IF.
EXECUTE.

DO IF (MISSING(q45_2)).
RECODE q45_2_98 (ELSE=1).
END IF.
EXECUTE.

DO IF (MISSING(q45_3)).
RECODE q45_3_98 (ELSE=1).
END IF.
EXECUTE.
*****q47***************************************************************************

compute q49_1_velha = q49_1. 
compute q49_2_velha = q49_2. 
compute q49_3_velha = q49_3. 
compute q49_4_velha = q49_4. 
compute q49_5_velha = q49_5. 
compute q49_6_velha = q49_6. 
compute q49_7_velha = q49_7. 
compute q49_8_velha = q49_8. 
compute q49_9_velha = q49_9. 
compute q49_99_velha = q49_99. 
execute. 

DO IF (q47=0).
RECODE q48_1 (ELSE=SYSMIS).
RECODE q49_1 (ELSE=SYSMIS).
RECODE q49_2 (ELSE=SYSMIS).
RECODE q49_3 (ELSE=SYSMIS).
RECODE q49_4 (ELSE=SYSMIS).
RECODE q49_5 (ELSE=SYSMIS).
RECODE q49_6 (ELSE=SYSMIS).
RECODE q49_7 (ELSE=SYSMIS).
RECODE q49_8 (ELSE=SYSMIS).
RECODE q49_9 (ELSE=SYSMIS).
RECODE q49_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

***q51****************************************************************************

compute q51_velha = q51. 
compute q51_98_velha = q51_98. 
execute. 

DO IF (MISSING(q51)).
RECODE q51_98 (ELSE=1).
END IF.
EXECUTE.


***q52**************************************************************************

compute q52_1_velha = q52_1. 
compute q52_1_98_velha = q52_1_98. 
compute q52_2_velha = q52_2. 
compute q52_2_98_velha = q52_2_98. 
execute. 

DO IF (MISSING(q52_1)).
RECODE q52_1_98 (ELSE=1).
END IF.
EXECUTE.

DO IF (MISSING(q52_2)).
RECODE q52_2_98 (ELSE=1).
END IF.
EXECUTE.


****q53**********************************************************************************************

compute q53_1_velha = q53_1. 
compute q53_2_velha = q53_2. 
compute q53_3_velha = q53_3. 
compute q53_99_velha = q53_99. 
compute q53_0_velha = q53_0. 
execute. 

DO IF (q53_0=1).
RECODE q53_1 (ELSE=SYSMIS).
RECODE q53_2 (ELSE=SYSMIS).
RECODE q53_3 (ELSE=SYSMIS).
RECODE q53_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

****q54*************************************************************************************

compute q54_velha = q54. 
compute q54_98_velha = q54_98. 
execute. 

DO IF (MISSING(q54)).
RECODE q54_98 (ELSE=1).
END IF.
EXECUTE.
****q55*************************************************************************************

compute q55_velha = q55. 
compute q55_98_velha = q55_98. 
execute. 

DO IF (MISSING(q55)).
RECODE q55_98 (ELSE=1).
END IF.
EXECUTE.


****q56*************************************************************************************

compute q56_velha = q56. 
compute q56_97_velha = q56_97. 
execute. 

DO IF (MISSING(q56)).
RECODE q56_97 (ELSE=1).
END IF.
EXECUTE.

****q63*****************************************************************************************************

compute q63_1_velha = q63_1. 
compute q63_2_velha = q63_2. 
compute q63_0_velha = q63_0. 
compute q64_1_velha = q64_1. 
compute q64_2_velha = q64_2. 
compute q64_3_velha = q64_3. 
compute q64_4_velha = q64_4. 
compute q64_5_velha = q64_5. 
compute q64_6_velha = q64_6. 
compute q64_99_velha = q64_99. 
execute. 

DO IF (q63_0=1).
RECODE q63_1 (ELSE=SYSMIS).
RECODE q63_2 (ELSE=SYSMIS).
RECODE q64_1 (ELSE=SYSMIS).
RECODE q64_2 (ELSE=SYSMIS).
RECODE q64_3 (ELSE=SYSMIS).
RECODE q64_4 (ELSE=SYSMIS).
RECODE q64_5 (ELSE=SYSMIS).
RECODE q64_6 (ELSE=SYSMIS).
RECODE q64_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

****q65**************************************************************************************************

compute q65_1_velha = q65_1. 
compute q65_2_velha = q65_2. 
compute q65_3_velha = q65_3. 
compute q65_4_velha = q65_4. 
compute q65_99_velha = q65_99. 
compute q65_0_velha = q65_0. 
execute. 

DO IF (q65_0=1).
RECODE q65_1 (ELSE=SYSMIS).
RECODE q65_2 (ELSE=SYSMIS).
RECODE q65_3 (ELSE=SYSMIS).
RECODE q65_4 (ELSE=SYSMIS).
RECODE q65_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


****q66*********************************************************************************************

compute q67_1_velha = q67_1. 
compute q67_2_velha = q67_2. 
compute q67_3_velha = q67_3. 
compute q67_4_velha = q67_4. 
compute q67_5_velha = q67_5. 
compute q67_6_velha = q67_6. 
compute q67_7_velha = q67_7. 
compute q67_8_velha = q67_8. 
compute q67_99_velha = q67_99. 
execute. 

DO IF (q66=0).
RECODE q67_1 (ELSE=SYSMIS).
RECODE q67_2 (ELSE=SYSMIS).
RECODE q67_3 (ELSE=SYSMIS).
RECODE q67_4 (ELSE=SYSMIS).
RECODE q67_5  (ELSE=SYSMIS).
RECODE q67_6 (ELSE=SYSMIS).
RECODE q67_7 (ELSE=SYSMIS).
RECODE q67_8 (ELSE=SYSMIS).
RECODE q67_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


VARIABLE LABELS
q1_velha'q1_v12958_1. Informe o ano de publica��o da Lei que criou o Conselho:'
q1_1_velha'q1_1_v13022_0_1.1. N�o est� criado em Lei'
q2_velha'q2_v12959_2. Houve altera��o da Lei original por alguma Lei posteriormente aprovada?'
q3_velha'q3_v12960_3. Caso sim, informe o ano da Lei que alterou a Lei original de cria��o do Conselho:'
q5_velha'q5_v10771_5. Caso sim, informe o ano da �ltima atualiza��o do Regimento:'
q8_1_velha'q8_1_v10806_1_8. Caso sim, indique quais:1: Secretaria de Assist�ncia Social ou cong�nere'
q8_2_velha'q8_2_v10806_2_8. Caso sim, indique quais:2: Outra unidade administrativa (Sede de Prefeitura, Sede de Governo, Administra��o Regional, etc)'
q8_3_velha'q8_3_v10806_3_8. Caso sim, indique quais:3: CRAS � Centro de Refer�ncia de Assist�ncia Social'
q8_4_velha'q8_4_v10806_4_8. Caso sim, indique quais:4: CREAS � Centro de Refer�ncia Especializado de Assist�ncia Social'
q8_5_velha'q8_5_v10806_5_8. Caso sim, indique quais:5: Outra unidade p�blica de servi�os da Assist�ncia Social'
q8_6_velha'q8_6_v10806_6_8. Caso sim, indique quais:6: Unidade de Sa�de'
q8_7_velha'q8_7_v10806_7_8. Caso sim, indique quais:7: Escola'
q8_8_velha'q8_8_v10806_8_8. Caso sim, indique quais:8: Associa��o Comunit�ria'
q8_9_velha'q8_9_v10806_9_8. Caso sim, indique quais:9: Entidade ou Organiza��o da Sociedade Civil'
q8_10_velha'q8_10_v10806_10_8. Caso sim, indique quais:10: Conselho Tutelar'
q8_11_velha'q8_11_v10806_11_8. Caso sim, indique quais:11: Outros Conselhos'
q8_99_velha'q8_99_v10806_99_8. Caso sim, indique quais:99: Outros'
q13_velha'q13_v10828_13. O(a) Secret�rio(a) Executivo(a) trabalha exclusivamente no Conselho?'
q14_1_velha'q14_1_v12961_14.1. Funcion�rias(os) exclusivas(os) da Secretaria Executiva do Conselho'
q14_2_velha'q14_2_v12962_14.2. Funcion�rias(os) n�o exclusivas(os) da Secretaria Executiva do Conselho'
q22_velha'q22_v12988_22. Em 2018, quantas reuni�es da CIB o conselho de assist�ncia social acompanhou? (Caso n�o tenha, informar 0)'
q22_98_velha'q22_98_v13025_98_22. Em 2018, quantas reuni�es da CIB o conselho de assist�ncia social acompanhou? (Caso n�o tenha, informar 0)22.98. N�o sabe'
q28_0_velha'q28_0_v10838_0_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?0: N�o s�o publicizadas'
q28_1_velha'q28_1_v10838_1_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?1: Ficam no Conselho em local de livre acesso para a popula��o usu�ria'
q28_2_velha'q28_2_v10838_2_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?2: S�o enviados �s(aos) Conselheiras(os) e/ou suas entidades'
q28_3_velha'q28_3_v10838_3_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?3: S�o enviados �s entidades de Assist�ncia Social que n�o est�o representadas no conselho'
q28_4_velha'q28_4_v10838_4_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?4: S�o enviados �s(aos) representantes e organiza��es de usu�rias(os) que n�o est�o representadas no conselho'
q28_5_velha'q28_5_v10838_5_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?5: S�o enviados �s organiza��es e entidades de trabalhadoras(es) do setor que n�o est�o representadas no conselho'
q28_6_velha'q28_6_v10838_6_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?6: Disponibilizados no site do Conselho'
q28_7_velha'q28_7_v10838_7_28. De que forma as atas do Conselho s�o publicizadas (tornam-se de conhecimento para a sociedade)?7: Em Boletins produzidos pela �rea de assist�ncia social'
q30_1_velha'q30_1_v14609_1_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?1: N�o h� mecanismos de comunica��o'
q30_2_velha'q30_2_v14609_2_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?2: O Conselho disponibiliza contatos (e-mail/ telefone/whatsapp) para o p�blico'
q30_3_velha'q30_3_v14609_3_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?3: O Conselho tem p�gina nas redes sociais (Facebook, blog etc)'
q30_4_velha'q30_4_v14609_4_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?4: O Conselho tem canal de recebimento de den�ncias'
q30_5_velha'q30_5_v14609_5_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?5: O Conselho convida usu�rias(os), al�m das(os) conselheiras(os), para a participa��o nas reuni�es plen�rias'
q30_99_velha'q30_99_v14609_99_30. O Conselho possui alguma estrat�gia de comunica��o com as(os) usu�rias(os) do SUAS?99: Outros'
q36_velha'q36_v14612_36. Caso sim, qual o ano da �ltima atualiza��o da regulamenta��o?'
q36_98_velha'q36_98_v14806_98_36.98. Ano da �ltima atualiza��o da regulamenta��o? - N�o sabe'
q43_velha'q43_v10847_43. Se sim, informe como a fiscaliza��o � realizada:'
q44_velha'q44_v10848_44. O conselho tem planejamento das fiscaliza��es realizadas anualmente?'
q45_1_velha'q45_1_v12997_45.1. Visitas a unidades p�blicas da rede socioassistencial'
q45_1_98_velha'q45_1_98_v13026_98_45.1.98. Visitas a unidades p�blicas da rede socioassistencial - N�o Sabe'
q45_2_velha'q45_2_v12998_45.2. Visitas a entidades/organiza��es da sociedade civil'
q45_2_98_velha'q45_2_98_v13027_98_45.2.98. Visitas a entidades/organiza��es da sociedade civil - N�o Sabe'
q45_3_velha'q45_3_v12999_45.3. Total de visitas'
q45_3_98_velha'q45_3_98_v13028_98_45.3.98. Total de visitas - N�o Sabe'
q49_1_velha'q49_1_v13003_1_49. O que est� estabelecido por esta Resolu��o?1: Caracteriza��o das entidades/organiza��es'
q49_2_velha'q49_2_v13003_2_49. O que est� estabelecido por esta Resolu��o?2: Documenta��o exigida'
q49_3_velha'q49_3_v13003_3_49. O que est� estabelecido por esta Resolu��o?3: Local para entrega da documenta��o'
q49_4_velha'q49_4_v13003_4_49. O que est� estabelecido por esta Resolu��o?4: Tempo para deferimento/indeferimento'
q49_5_velha'q49_5_v13003_5_49. O que est� estabelecido por esta Resolu��o?5: Procedimentos e an�lise do processo de inscri��o'
q49_6_velha'q49_6_v13003_6_49. O que est� estabelecido por esta Resolu��o?6: Condi��es para visita �s unidades'
q49_7_velha'q49_7_v13003_7_49. O que est� estabelecido por esta Resolu��o?7: Pedido de reconsidera��o/recurso'
q49_8_velha'q49_8_v13003_8_49. O que est� estabelecido por esta Resolu��o?8: Cancelamento da inscri��o'
q49_9_velha'q49_9_v13003_9_49. O que est� estabelecido por esta Resolu��o?9: Acompanhamento e fiscaliza��o das entidades'
q49_99_velha'q49_99_v13003_99_49. O que est� estabelecido por esta Resolu��o?99: Outros'
q51_velha'q51_v13005_51. Quantas entidades fizeram pedidos de inscri��o junto a este Conselho em 2019 (De 1� de janeiro a 31 de agosto de 2019)?'
q51_98_velha'q51_98_v13006_98_51. Quantas entidades fizeram pedidos de inscri��o junto a este Conselho em 2019 (De 1� de janeiro a 31 de agosto de 2019)?51.98. N�o sabe'
q52_1_velha'q52_1_v13008_52.1. entidades com pedidos iniciados em anos anteriores'
q52_1_98_velha'q52_1_98_v13007_98_52.1. entidades com pedidos iniciados em anos anteriores.52.1.98. N�o sabe'
q52_2_velha'q52_2_v14613_52.2. entidades com pedidos iniciados este ano'
q52_2_98_velha'q52_2_98_v14614_98_52.2. entidades com pedidos iniciados este ano52.2.98. n�o sabe'
q53_1_velha'q53_1_v14615_1_53. Quais os principais motivos de indeferimento?1: Falta de documenta��o'
q53_2_velha'q53_2_v14615_2_53. Quais os principais motivos de indeferimento?2: As entidades n�o ofertam servi�os de Assist�ncia Social'
q53_3_velha'q53_3_v14615_3_53. Quais os principais motivos de indeferimento?3: As ofertas n�o est�o em conformidade'
q53_99_velha'q53_99_v14615_99_53. Quais os principais motivos de indeferimento?99: Outros'
q53_0_velha'q53_0_v14615_0_53. Quais os principais motivos de indeferimento?0: N�o houve indeferimentos'
q54_velha'q54_v13009_54. Quantos cancelamentos foram realizados em 2019 (De 1� de janeiro a 31 de agosto de 2019)?'
q54_98_velha'q54_98_v13010_98_54. Quantos cancelamentos foram realizados em 2019 (De 1� de janeiro a 31 de agosto de 2019)?54.98. N�o sabe'
q55_velha'q55_v13013_55. Quanto tempo, em m�dia, leva do momento que a entidade ou OSC faz o pedido de inscri��o at� o seu deferimento/indeferimento?'
q55_98_velha'q55_98_v13014_98_55. Quanto tempo, em m�dia, leva do momento que a entidade ou OSC faz o pedido de inscri��o at� o seu deferimento/indeferimento?55.98. N�o sabe'
q56_velha'q56_v13015_56. Quantas audi�ncias p�blicas para apresenta��o das entidades inscritas foram realizadas no �ltimo ano?'
q56_97_velha'q56_97_v13016_97_56. Quantas audi�ncias p�blicas para apresenta��o das entidades inscritas foram realizadas no �ltimo ano?56.97. N�o houve'
q63_1_velha'q63_1_v10885_1_63. Para o segmento de usu�rias(os), quem tem direito a voto no processo de elei��o dos representantes da sociedade civil?1: A(O) pr�pria(o) usu�ria(o)'
q63_2_velha'q63_2_v10885_2_63. Para o segmento de usu�rias(os), quem tem direito a voto no processo de elei��o dos representantes da sociedade civil?2: As(Os) representantes e organiza��es de usu�rias(os)'
q63_0_velha'q63_0_v10885_0_63. Para o segmento de usu�rias(os), quem tem direito a voto no processo de elei��o dos representantes da sociedade civil?0: Este Conselho n�o possui representantes de usu�rias(os)'
q64_1_velha'q64_1_v10888_1_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:1: Benefici�rias(os) do Programa Bolsa Fam�lia'
q64_2_velha'q64_2_v10888_2_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:2: Benefici�ria(o) ou fam�lia de benefici�rias(os) do BPC � Benef�cio de Presta��o Continuada'
q64_3_velha'q64_3_v10888_3_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:3: Usu�ria(o) de servi�os socioassistenciais de Prote��o Social B�sica'
q64_4_velha'q64_4_v10888_4_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:4: Usu�ria(o) de servi�os socioassistenciais de Prote��o Social Especial'
q64_5_velha'q64_5_v10888_5_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:5: Representante de associa��o comunit�ria ou de moradores'
q64_6_velha'q64_6_v10888_6_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:6: Representante de f�rum ou coletivo de usu�rias(os)'
q64_99_velha'q64_99_v10888_99_64. As(Os) representantes de usu�rias(os) ou organiza��o de usu�rias(os) s�o:99: Outros'
q65_1_velha'q65_1_v11956_1_65. As(Os) representantes de trabalhadoras(es) s�o:1: Representante de sindicato de trabalhadoras(es)'
q65_2_velha'q65_2_v11956_2_65. As(Os) representantes de trabalhadoras(es) s�o:2: Representante de conselho de classe profissional (Conselho Regional de Servi�o Social � CRESS, Conselho Regional de Psicologia � CFP, outros conselhos de classe)'
q65_3_velha'q65_3_v11956_3_65. As(Os) representantes de trabalhadoras(es) s�o:3: Representante de associa��o, f�rum ou coletivo de trabalhadoras(es)'
q65_4_velha'q65_4_v11956_4_65. As(Os) representantes de trabalhadoras(es) s�o:4: A(O) pr�pria(o) trabalhador(a) do SUAS, sem vincula��o a nenhum coletivo'
q65_99_velha'q65_99_v11956_99_65. As(Os) representantes de trabalhadoras(es) s�o:99: Outros'
q65_0_velha'q65_0_v11956_0_65. As(Os) representantes de trabalhadoras(es) s�o:0: Este Conselho n�o possui representantes de trabalhadoras(es)'
q67_1_velha'q67_1_v10891_1_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?1: Comiss�o de normas'
q67_2_velha'q67_2_v10891_2_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?2: Comiss�o de pol�tica'
q67_3_velha'q67_3_v10891_3_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?3: Comiss�o de financiamento'
q67_4_velha'q67_4_v10891_4_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?4: Comiss�o de acompanhamento de conselhos'
q67_5_velha'q67_5_v10891_5_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?5: Comiss�o de �tica'
q67_6_velha'q67_6_v10891_6_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?6: Comiss�o de acompanhamento de benef�cios e transfer�ncia de renda (BPC, Programa Bolsa Fam�lia, etc.)'
q67_7_velha'q67_7_v10891_7_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?7: Comiss�o de fiscaliza��o/monitoramento/cadastro de entidades'
q67_8_velha'q67_8_v10891_8_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?8: Comiss�o de comunica��o'
q67_99_velha'q67_99_v10891_99_67. Quais das comiss�es permanentes est�o funcionando regularmente nesse conselho?99: Outras'
.
