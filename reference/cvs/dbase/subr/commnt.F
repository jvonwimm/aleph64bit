      SUBROUTINE COMMNT(STRING,MODE)
C ---------------------------------------------------------------
C
C     Print one line comment per bank from LBF-file
C
C ---------------------------------------------------------------
#ifndef DOC
#include "sbkpar.h"
#include "paging.h"
#include "fildat.h"
#include "sbkcom.h"
      CHARACTER*4 BANK,STRING,BANKS(NBNK),OLDBNK
      CHARACTER*80 BCOM,BCOMS(NBNK),SPACES
C -----------------------------------------------------------------
      I=0
      NB1=0
    1 I=I+1
      IF (LGT(STRING,NAMARR(I)).AND.I.LT.IADARR(0)) GOTO 1
      IF (LGT(STRING,NAMARR(I))) GOTO 50
      I=I-1
      IEST=MODE-1
      IF(MODE.EQ.0)IEST=4
   20 I=I+1
      BANK=NAMARR(I)
      IF(MODE.EQ.1)GOTO 21
      IF(BANK(1:IEST).NE.STRING(1:IEST))GOTO 50
   21 NB1=NB1+1
      BCOMS(NB1)=COMLIN(I)
      IF (I.LT.IADARR(0)) GOTO 20
   50 CONTINUE
      IF(NB1.EQ.0)GOTO 29
      NB2=0
C       this is in case a bank is defined twice
      OLDBNK='????'
      DO 28 K=1,NB1
        BANK=BCOMS(K)(1:4)
        MDUP=0
        IF(OLDBNK.EQ.BANK)MDUP=99
        OLDBNK=BANK
        IF(XBOOKG.OR.XREFER)THEN
          WRITE(LOUT,2)BANK,BANK
    2     FORMAT(' ',A,2('==================================='),A)
          CALL PAGE
        ENDIF
        IF(.NOT.XCONTT)THEN
          WRITE(LOUT,101)BCOMS(K)
  101     FORMAT(' ',A)
          CALL PAGE
        ENDIF
        IF(MODE.NE.0)THEN
          IF(XCONTT)CALL CONTENT(BANK,MDUP)
          IF(XBOOKG)CALL GTBOOK(BANK)
          IF(XREFER)CALL GTREFER(BANK)
        ENDIF
   28 CONTINUE
   29 CONTINUE
      END
#endif
