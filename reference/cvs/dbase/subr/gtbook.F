      SUBROUTINE GTBOOK(STRING)
C ----------------------------------------------------------------
C ----------------------------------------------------------------
#ifndef DOC
#include "paging.h"
#include "sbkpar.h"
#include "fildat.h"
      CHARACTER*4 BANK,STRING
C ---------------------------------------------------------------------
      NB=0
      I=0
   10 I=I+1
      IF (I.LT.IADARR(0).AND.NAMARR(I).NE.STRING) GOTO 10
      IF (NAMARR(I).NE.STRING.OR.NUMBKD(I).LT.1) GOTO 99
      WRITE(LOUT,210)STRING
      CALL PAGE
      WRITE(LOUT,211)
      CALL PAGE
      WRITE(LOUT,212)
      CALL PAGE
  210 FORMAT(' Bank ',A,' is booked in:')
  211 FORMAT(' ========================')
  212 FORMAT(' Program   Deck    Card     Banknumber')
      DO 220 NB=1,NUMBKD(I)
        WRITE(LOUT,214) BKDLIN(I,NB)
  214   FORMAT(' ',A)
  220 CALL PAGE
      NB=NUMBKD(I)
   99 IF(NB.EQ.0)THEN
        WRITE(LOUT,215)
        CALL PAGE
  215   FORMAT(' bank is never booked!')
      ENDIF
      CLOSE (LINP2)
      END
#endif
