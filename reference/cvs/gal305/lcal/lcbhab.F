*DK lcbhab
      SUBROUTINE LCBHAB(LCBHA,EFORW,EBACK)
C--------------------------------------------------------------
C! Simulates the Bhabha trigger
C! Author   : J.D.Hansen & P.H.Hansen - 860604
C! Modified : P.H.Hansen - 871001
C!
C!   Input : 'LTTR'
C!   Output: LCBHA  = 0 ... No trigger
C!                  = 1 ... Trigger
C!           EFORW  = trigger energy in subdetector A
C!           EBACK  = trigger energy in subdetector B
C!           update 'LCWS'
C!
C! - Called by LCTRIG                            from this .HLB
C ------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION JTRIG(12,2)
#include "iocom.h"
#include "jqcom.h"
#include "lccomc.h"
#include "lcnamc.h"
#include "bmacro.h"
C -------------------------------------------------------------
      LCBHA = 0
      EFORW = 0.
      EBACK = 0.
      KINDX = IW(NALTTR)
      IF(KINDX .LE. 0)                               GOTO 999
      LEL = LCOLS(KINDX)
C
C? Form super segments of adjecent pairs in the inner ring
      DO 100 IDET = 1,2
         DO 110 I = 1,12
           ISEG = (IDET-1)*12 + I
           KSEG = KROW(KINDX,ISEG)
           JTRIG(I,IDET) = 0
           DO 120 ISTOR = 1,3
             KL = KSEG + ISTOR
             IF (I .NE. 12) THEN
               JTRIG(I,IDET) = JTRIG(I,IDET) + IW(KL) + IW(KL+LEL)
             ELSE
               JTRIG(I,IDET) = JTRIG(I,IDET) + IW(KL) + IW(KL-11*LEL)
             ENDIF
  120      CONTINUE
  110   CONTINUE
        IF (LCPRNT .LT. 4)                                   GOTO 100
        WRITE(LOUTIO,5000) IDET,(JTRIG(J,IDET),J=1,12)
  100 CONTINUE
C
C? Loop over trigger segments and apply the trigger theshold
C? on both sides
      DO 200 I = 1,12
        J = I + 6
        IF (J .GT. 12) J = J - 12
        IF (JTRIG(I,1).GT.LCBHTR.AND.JTRIG(J,2).GT.LCBHTR) GOTO 210
  200 CONTINUE
C - Failure
      GOTO 999
C - Success
  210 LCBHA = 1
C
C? Energy in the trigger segments
      EFORW = FLOAT(JTRIG(I,1))*.001
      EBACK = FLOAT(JTRIG(J,2))*.001
C
C? Update the summary bank
      KINDX = IW(NALCWS)
      IF(KINDX .LE. 0)                                  GOTO 999
      IW(KINDX+LMHLEN+2) = IW(KINDX+LMHLEN+2) + 1
      IW(KINDX+LMHLEN+3) = IW(KINDX+LMHLEN+3) + JTRIG(I,1)
      IW(KINDX+LMHLEN+4) = IW(KINDX+LMHLEN+4) + JTRIG(J,2)
  999 CONTINUE
C
 5000 FORMAT(' +++ LCBHAB +++ component',I10,/,6I10,/,6I10)
      END
#endif
