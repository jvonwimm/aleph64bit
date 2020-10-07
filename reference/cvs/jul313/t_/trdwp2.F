      SUBROUTINE TRDWP2(IT0,NWSMP,IPH,ICHG,TIM,LMPH,IER)
C
C------------------------------------------------------------------------
C! Reduce TPC wire pulses not handled by the TPP, using the default
C! algorithm used for all data taken so far.
C
C  Author:    R. Johnson    16-1-91
C  Modified:  R. Johnson    24-8-92   Return LMPH to calling routine
C  Modified:  R. Johnson    2-3-93    Fix bug such that LMPH REALLY
C                                     is returned
C
C  Input:    IT0       /I        Time of 1st bucket in the pulse
C            NWSMP     /I        Number of samples in the pulse
C            IPH(n)    /I        Array of sample pulse heights
C  Output:   ICHG      /I        Charge estimate
C            TIM       /R        Time estimate
C            LMPH      /I        Length of pulse above threshold
C            IER       /I        Error code
C                                0= all OK
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "twirct.h"
C
      DIMENSION IPH(NWSMP)
C
      IER=0
C
C++   Loop only on the samples above the TPD threshold
C     (i.e. skip the pre and post samples) and find the total
C     charge and charge-weighted time above a threshold
C
      IS2=0
      ICHG=0
      ISMPT=0
      LMPH=0
      DO 300 IS=1,NWSMP
        IF (IPH(IS).GT.JTHRTW) THEN
          LMPH=LMPH+1
          ICHG= ICHG + IPH(IS)
          ISMPT= ISMPT + IPH(IS)*IS
          IS2=IS2 + IPH(IS)*(IS**2)
          IF (IS.GT.NPRETW .AND. IS.LE.(NWSMP-NPSTTW)) THEN
            ITST=IPH(IS)+JTOLTW
            IF (ITST.LT.IPH(IS-1) .AND. ITST.LT.IPH(IS+1)) THEN
              IER=1
            ENDIF
          ENDIF
        ENDIF
  300 CONTINUE
C
C++   Calculate and cut on the RMS width of the pulse
C
      IF (ICHG.EQ.0) THEN
        TIM=FLOAT(IT0-1)+0.5*FLOAT(NWSMP)
        IER=3
      ELSE
        RNS=FLOAT(ICHG)
        TIM=FLOAT(IT0-1) + FLOAT(ISMPT)/RNS
        WRM= FLOAT(IS2)/RNS - (FLOAT(ISMPT)/RNS)**2
        IF (WRM.GT.WRMXTW) THEN
          IER=2
        ENDIF
      ENDIF
C
      END
#endif
