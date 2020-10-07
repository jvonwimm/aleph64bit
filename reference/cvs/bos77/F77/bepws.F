      SUBROUTINE BEPWS(JW,LUN,NAME,NR)
*
*     WRITE SINGLE BANK
*
#include "boscom.h"
#include "param.h"
      CHARACTER*4 NAME
      INTEGER JW(*)
C     ...
      JW1=JW(1)
      NAMA=INTCHA(NAME)
#include "namez.h"
      IF(NAMI.EQ.0) GOTO 100
C
C     RESET LINK BANKS
C
      JND=JW(IDL)+NAMI-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0
#include "index.h"
      IF(INDI.EQ.0) GOTO 100
*
*     CHANGE INDICES, TO SIMULATE CONDITION, WHERE ONLY THE BANK
*     BANK TO BE WRITTEN IS STORED
*
      IF(JW1.EQ.1) JW(INDJ-1)=JW(INDI-1)
      NAMS=JW(NAMI)
      JW(NAMI  )=INDI
      INDS=JW(INDI-1)
      JW(INDI-1)=0
*
      CALL BEPWR(JW,LUN,NAME)
*
      JW(NAMI)=NAMS
      IF(JW1.NE.1) JW(INDI-1)=INDS
  100 RETURN
      END
