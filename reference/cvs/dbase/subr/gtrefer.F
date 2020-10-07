      SUBROUTINE GTREFER(STRING)
C ----------------------------------------------------------------
C
C     produce list of bank references
C
C ---------------------------------------------------------------
#ifndef DOC
#include "sbkpar.h"
#include "paging.h"
#include "fildat.h"
      CHARACTER*4 STRING
C -----------------------------------------------------------------
      NB=0
      I=0
   10 I=I+1
      IF (I.LT.IADARR(0).AND.NAMARR(I).NE.STRING) GOTO 10
      IF (NAMARR(I).NE.STRING.OR.NUMREF(I).LT.1) GOTO 99
      WRITE(LOUT,210)STRING
      CALL PAGE
      WRITE(LOUT,211)
      CALL PAGE
      WRITE(LOUT,212)
      CALL PAGE
  210 FORMAT(' Bank ',A,' is referenced in:')
  211 FORMAT(' ========================')
  212 FORMAT(' Program   Deck    Card  ')
      DO 220 NB=1,NUMREF(I)
        WRITE(LOUT,214) REFLIN(I,NB)
  214   FORMAT(' ',A)
  220 CALL PAGE
      NB=NUMREF(I)
   99 IF(NB.EQ.0)THEN
        WRITE(LOUT,215)
        CALL PAGE
  215   FORMAT(' bank is never referenced!')
      ENDIF
      CLOSE (LINP2)
      END
#endif
