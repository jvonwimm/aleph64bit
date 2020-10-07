      SUBROUTINE NAMESL(STRING,MODE)
C -----------------------------------------------------------------
C
C     produce list of bank names from LBF-file
C
C -----------------------------------------------------------------
#ifndef DOC
#include "sbkpar.h"
#include "paging.h"
#include "fildat.h"
#include "sbkcom.h"
      CHARACTER*4 BANK,STRING,BANKS(NBNK),OLDBNK
      CHARACTER*80 BCOM,BCOMS(NBNK),SPACES
C --------------------------------------------------------------------
      NB1=0
      MODE=INDEX(STRING,'*')
      M1=MODE-1
      WRITE(LOUT,3)
    3 FORMAT(' BOS banks included in the ALEPH documentation scheme:')
      CALL PAGE
      WRITE(LOUT,4)
    4 FORMAT(' -----------------------------------------------------')
      CALL PAGE
      I=0
   20 I=I+1
      IF (I.GT.IADARR(0)) GOTO 50
      BANK=NAMARR(I)
      IF(MODE.EQ.1)GOTO 25
      IF(STRING(1:M1).NE.BANK(1:M1))GOTO 20
   25 CONTINUE
      NB1=NB1+1
      BANKS(NB1)=BANK
      GOTO 20
   50 CONTINUE
      IF(NB1.EQ.0) GOTO 99
      L1=1
      JBPL=14
   51 L2=L1+JBPL-1
      IF(L2.GT.NB1)L2=NB1
      WRITE(LOUT,120)(BANKS(L),L=L1,L2)
      CALL PAGE
      L1=L1+JBPL
      IF(L1.LE.NB1)GOTO 51
      IF(XUNDOC)THEN
C     look for banks defined more than once
        NDUP=0
        DO 60 L=2,NB1
          IF(BANKS(L).EQ.BANKS(L-1))THEN
            IF(NDUP.EQ.0)THEN
              WRITE(LOUT,120)'BANKS DEFINED MORE THAN ONCE:'
              NDUP=1
              CALL PAGE
            ENDIF
            WRITE(LOUT,120)BANKS(L)
            CALL PAGE
          ENDIF
   60   CONTINUE
        IF(NDUP.EQ.0)THEN
          WRITE(LOUT,120)'ALL BANKS UNIQUELY DEFINED'
          CALL PAGE
        ENDIF
C
C     look for banks identical in first 3 letters
        NDUP=0
        NDUP1=0
        DO 63 L=2,NB1
          IF(BANKS(L)(1:3).EQ.BANKS(L-1)(1:3))THEN
            IF(NDUP.EQ.0)THEN
              WRITE(LOUT,120)'BANKS identical in first 3 letters:'
              NDUP=1
              CALL PAGE
            ENDIF
            IF(NDUP1.EQ.0)NDUP1=L-1
          ELSE
            IF(NDUP1.NE.0)THEN
              WRITE(LOUT,120)(BANKS(L3),L3=NDUP1,L-1)
              CALL PAGE
              NDUP1=0
            ENDIF
          ENDIF
C
   63   CONTINUE
        IF(NDUP.EQ.0)THEN
          WRITE(LOUT,120)'ALL BANKS different in first 3 letters'
          CALL PAGE
        ENDIF
      ENDIF
C
   99 CONTINUE
  120 FORMAT(14(' ',A))
      END
#endif
