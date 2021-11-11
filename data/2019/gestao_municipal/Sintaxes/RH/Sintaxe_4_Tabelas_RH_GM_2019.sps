﻿* Encoding: UTF-8.
**Utilizar base completa RH  GM**



*****TABELAS PARA SEREM RODADAS NA BASE DO RH******

FREQUENCIES VARIABLES=Região Porte_pop2010 UF
  /ORDER=ANALYSIS.


FREQUENCIES VARIABLES=  Faixas_etarias q65_3  q65_9  d_65_9
  /ORDER=ANALYSIS.




* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Região Porte_pop2010 UF d_65_9bin1 d_65_9bin2 d_65_9bin3 DISPLAY=LABEL
  /TABLE Região [C] + Porte_pop2010 [C] + UF [C] BY d_65_9bin1 [S][SUM, COLPCT.COUNT PCT40.1] + 
    d_65_9bin2 [S][SUM, COLPCT.COUNT PCT40.1] + d_65_9bin3 [S][SUM, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Região [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Porte_pop2010 ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=UF [11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 
    33, 35, 41, 42, 43, 50, 51, 52, 53, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.


FREQUENCIES VARIABLES=   q65_10   d_65_10
  /ORDER=ANALYSIS.



* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Região Porte_pop2010 UF d_65_10bin1 d_65_10bin2 d_65_10bin3  d_65_10bin4 d_65_10bin5
    DISPLAY=LABEL
  /TABLE Região [C] + Porte_pop2010 [C] + UF [C] BY d_65_10bin1 [SUM, COLPCT.COUNT PCT40.1] + 
   d_65_10bin2  [SUM, COLPCT.COUNT PCT40.1] + d_65_10bin3 [SUM, COLPCT.COUNT PCT40.1] +  d_65_10bin4  
    [SUM, COLPCT.COUNT PCT40.1] + d_65_10bin5 [SUM, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Região [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Porte_pop2010 ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=UF [11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 
    33, 35, 41, 42, 43, 50, 51, 52, 53, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.




FREQUENCIES VARIABLES=   q65_11  d_65_11
  /ORDER=ANALYSIS.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Região Porte_pop2010 UF d_65_11bin1  d_65_11bin2  d_65_11bin3  d_65_11bin4 
    DISPLAY=LABEL
  /TABLE Região [C] + Porte_pop2010 [C] + UF [C] BY d_65_11bin1  [SUM, COLPCT.COUNT PCT40.1] + 
    d_65_11bin2   [SUM, COLPCT.COUNT PCT40.1] + d_65_11bin3 [SUM, COLPCT.COUNT PCT40.1] + d_65_11bin4
    [SUM, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Região [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Porte_pop2010 ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=UF [11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 
    33, 35, 41, 42, 43, 50, 51, 52, 53, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.



FREQUENCIES VARIABLES=   q65_12 q65_13 q65_15  q65_16 q65_17
  /ORDER=ANALYSIS.




