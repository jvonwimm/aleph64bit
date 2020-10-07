*DK edfilh
       SUBROUTINE EDFILH
C.----------------------------------------------------------------------
C   D.Zwierski  (X)                                    October 87
C! Fill standard histog. for digits
C  Si FHISJO(4)=.TRUE., then fills histograms with digital
C  values taken from EWDI and ETDI BOS banks
C
C  Called by ECDIGI
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "ecstat.h"
#include "bcs.h"
#include "bmacro.h"
C
C    initialisation of ECCONT(I) I=25,30, used for filling
C
       IF(FHISJO(4)) THEN
           WSKI=1.
           SKIZ=0.
           IDSKI=439
           DO 121 I=25,30,1
               ECCONT(I)=0.
121        CONTINUE
C
C    take digital signals on wires from EWDI bank
C
           KDSKI=NLINK('EWDI',0)
           IF(KDSKI.EQ.0) GOTO 125
           DO 123 I=1,LROWS(KDSKI),1
               DO 124 J=1,45,1
                   IESKI=ITABL(KDSKI,I,J+1)
                   IF(MOD(J,2).EQ.0) THEN
                       ECCONT(26)=ECCONT(26)+IESKI
                   ELSE
                       ECCONT(25)=ECCONT(25)+IESKI
                   ENDIF
                   ZPSKI=J-0.5
                   ESKI=FLOAT(IESKI)/1000
                   CALL HFILL(IDSKI+18,ZPSKI,SKIZ,ESKI)
                   IF(IESKI.NE.0) THEN
                       CALL HFILL(IDSKI+19,ZPSKI,SKIZ,WSKI)
                   ENDIF
124            CONTINUE
123        CONTINUE
           ECCONT(25)=ECCONT(25)/1000.
           ECCONT(26)=ECCONT(26)/1000.
           CALL HFILL(IDSKI+16,ECCONT(26),ECCONT(25),WSKI)
           ECCONT(26)=ECCONT(26)+ECCONT(25)
           CALL HFILL(IDSKI+17,ECCONT(26),SKIZ,WSKI)
C
C    take digital signals on towers from ETDI bank
C
125        KDSKI=NLINK('ETDI',0)
           IF(KDSKI.EQ.0) GOTO 225
       ROWSKI=FLOAT(LROWS(KDSKI))
       CALL HFILL(IDSKI+24,ROWSKI,SKIZ,WSKI)
           DO 223 I=1,LROWS(KDSKI),1
               DO 224 J=2,4,1
                   IESKI=ITABL(KDSKI,I,J)
                   ECCONT(25+J)=ECCONT(25+J)+IESKI
                   ECCONT(30)=ECCONT(30)+IESKI
224            CONTINUE
223        CONTINUE
           ECCONT(27)=ECCONT(27)/1000.
           ECCONT(28)=ECCONT(28)/1000.
           ECCONT(29)=ECCONT(29)/1000.
           ECCONT(30)=ECCONT(30)/1000.
           CALL HFILL(IDSKI+20,ECCONT(30),SKIZ,WSKI)
           CALL HFILL(IDSKI+21,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+22,ECCONT(28),SKIZ,WSKI)
           CALL HFILL(IDSKI+23,ECCONT(29),SKIZ,WSKI)
       ENDIF
225    CONTINUE
       RETURN
       END
#endif
