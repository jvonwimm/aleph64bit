*DK edtcor
      SUBROUTINE EDTCOR
C-----------------------------------------------------------------
C      O.CALLOT   31-OCT-85
C! Correct tower digit
C. - called from ECDIGI                                 this .HLB
C
C----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "edpara.h"
#include "edcond.h"
#include "jqcom.h"
#include "ecnamc.h"
#include "bmacro.h"
C --------------------------------------------------------
      JETDI = IW(NAETDI)
      IF( JETDI .EQ. 0 ) GOTO 990
      NETDI = LROWS( JETDI )
      LETDI = LCOLS( JETDI )
      KETDI = JETDI + LMHLEN
      DO 10 I=1,NETDI
        DO 15 J=1,3
          IW(KETDI+1+J) = KALEDT(J) * IW(KETDI+1+J)
   15   CONTINUE
        KETDI = KETDI + LETDI
   10 CONTINUE
      IF(EDSAVG.EQ.'YES') THEN
        JENDI = IW (NAENDI)
        IF( JENDI .EQ. 0 ) GOTO 990
        LENDI = LCOLS( JENDI )
        KENDI = JENDI + LMHLEN
        DO 20 I=1,NETDI
           DO 25 J=1,3
              IW(KENDI+4+J) = 1000000.*RW(KENDI+4+J)*FLOAT(KALEDT(J))
  25       CONTINUE
           KENDI = KENDI + LENDI
  20     CONTINUE
      ENDIF
  990 RETURN
      END
#endif
