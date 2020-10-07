      SUBROUTINE TRMVEP(KTSCL,IC,KTSPU,RPULS,JP1,NSPL)
C
C------------------------------------------------------------------
C! Cut low edge pulses from a subcluster
C
C  Author:  R. Johnson   15-12-89
C
C  Input:  KTSCL        /I      BOS pointer to subcluster bank
C          IC           /I      Subcluster number
C          KTSPU        /I      BOS pointer to subpulse bank
C          RPULS        /I      List of pulse heights in subcluster
C          JP1          /I      First pulse in RPULS to be used for
C                               coordinate calculation
C          NSPL         /I      Number of pulses in RPULS to be used
C                               for coordinate calculation
C  Output: JP1,NSPL     /I      Updated values after removal of
C                               useless edge pulses
C
C------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tscljj.h"
#include "tspujj.h"
#include "tpccut.h"
C
      DIMENSION RPULS(*)
C
#include "bmacro.h"
C
      IF (NSPL.GT.2 .AND. NSPL.LE.IMXPTP) THEN
        IOFSP=ITABL(KTSCL,IC,JTSCPO)
        PKMX=0.
        DO 834 I=1,ITABL(KTSCL,IC,JTSCNS)
          PKMX=AMAX1(PKMX,RTABL(KTSPU,IOFSP+I,JTSPPC))
  834   CONTINUE
C
  856   CONTINUE
        IF (NSPL.EQ.3) THEN
          RAT=RPULS(JP1+2)/RPULS(JP1)
          IF (RAT.GT.RTMXTP) THEN
            PEAK=RTABL(KTSPU,IOFSP+JP1,JTSPPC)
            RAT=PEAK/PKMX
            IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
              JP1=2
              NSPL=2
            ENDIF
          ELSEIF (RAT.LT.(1./RTMXTP)) THEN
            PEAK=RTABL(KTSPU,IOFSP+JP1+2,JTSPPC)
            RAT=PEAK/PKMX
            IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
              NSPL=2
            ENDIF
          ENDIF
        ELSE
          NSAV=NSPL
          IF (RPULS(JP1).LT.RPULS(JP1+NSPL-1)) THEN
            PEAK=RTABL(KTSPU,IOFSP+JP1,JTSPPC)
            RAT=PEAK/PKMX
            IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
              NSPL=NSPL-1
              JP1=JP1+1
            ENDIF
          ELSE
            PEAK=RTABL(KTSPU,IOFSP+JP1+NSPL-1,JTSPPC)
            RAT=PEAK/PKMX
            IF (PEAK.LT.CMNPTP .AND. RAT.LT.RLTHTP) THEN
              NSPL=NSPL-1
            ENDIF
          ENDIF
          IF (NSPL.NE.NSAV) GO TO 856
        ENDIF
      ENDIF
      RETURN
      END
#endif
