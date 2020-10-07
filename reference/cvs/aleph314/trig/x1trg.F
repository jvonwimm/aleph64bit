      SUBROUTINE X1TRG
C ----------------------------------------------------------------------
C.
C. - Author : E. Blucher - 89/11/2 for ALPHA, JULIA
C. ----Modified version of X1TRIG by A. Putzer, C. Geweniger.
C. - Modified : C. Geweniger - 890900   for ALEPHLIB 9.9
C.
C. - Apply level1 trigger conditions using information from X1AD
C.   and XTEB banks.
C.
C! - This routine simulates the application of the level 1 trigger.
C.
C? - The subroutines called correspond to the functions applied
C?   online
C.
C. - Banks    : X1TH  return if it does not exist
C.              XTEB  book the bank with 1 row
C.
#ifndef DOC
      SAVE
#include "bcs.h"
#include "x1namc.h"
#include "xtebjj.h"
C.
C---Apply trigger only if threshold bank exists.
      IF (IW(NAX1TH).EQ.0) GO TO 999
C
      CALL X1INP(IERR)
      IF(IERR.EQ.1)GOTO 999
C
C--- Initialize bank XTEB
C
      IF (IW(NAXTEB).NE.0) THEN
         KXT = IW(NAXTEB)+LMHLEN
         DO 1 I=1,LXTEBA
            IW(KXT+I) = 0
 1       CONTINUE
      ELSE
         LEN=LMHLEN+LXTEBA
         CALL AUBOS('XTEB',0,LEN,KXTEB,IGARB)
         IF (KXTEB.EQ.0) GOTO 999
         IW(KXTEB+1)=LXTEBA
         IW(KXTEB+2)=1
      ENDIF
C
      CALL BLIST(IW,'E+','XTEB')
C.
      CALL X1DISC
C.
      CALL X1APTR
C.
      CALL X1OUTP
C
 999  CONTINUE
C.
      RETURN
      END
#endif
