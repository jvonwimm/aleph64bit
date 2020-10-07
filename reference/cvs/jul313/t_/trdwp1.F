      SUBROUTINE TRDWP1(IT0,NWSMP,IPH,ICHG,TIM,IER)
C
C----------------------------------------------------------------------
C! Reduce those TPC wire pulses not already done by the TPP
C
C  R. Johnson    16-1-91
C
C  Input:    IT0       /I        Time of 1st bucket in the pulse
C            NWSMP     /I        Number of samples in the pulse
C            IPH(n)    /I        Array of sample pulse heights
C  Output:   ICHG      /I        Charge estimate
C            TIM       /R        Time estimate
C            IER       /I        Error code
C                                0= all OK
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "twirct.h"
C
      LOGICAL GOOD
      DIMENSION IPH(NWSMP)
C
      IER=0
C
C++   Loop only on the samples above the TPD threshold
C     (i.e. skip the pre and post samples) and find the maximum.
C     Also, look for valleys which are too large to tolerate.
C     Also, calculate the rms width of the pulse.
C
      IS1=IPH(NPRETW+1)
      IS2=IPH(NPRETW+1)
      IS3=IPH(NPRETW+1)
      IMAX=NPRETW+1
      DO 300 IS=NPRETW+2,NWSMP-NPSTTW
        IF (IPH(IS).GE.IPH(IMAX)) THEN
          IMAX=IS
        ENDIF
C
        ITST=IPH(IS)+JTOLTW
        IF (ITST.LT.IPH(IS-1) .AND. ITST.LT.IPH(IS+1)) THEN
          IER=1
        ENDIF
        IS1=IS1 + IPH(IS)*IS
        IS2=IS2 + IPH(IS)*(IS**2)
        IS3=IS3 + IPH(IS)
  300 CONTINUE
C
C++   Cut on the RMS width of the pulse
C
      IF (IS3.EQ.0) THEN
        ICHG=0
        TIM=FLOAT(IT0-1)+0.5*FLOAT(NWSMP)
        IER=6
        RETURN
      ELSE
        RNS=FLOAT(IS3)
        WRM= FLOAT(IS2)/RNS - (FLOAT(IS1)/RNS)**2
        IF (WRM.GT.WRMXTW) THEN
          IER=2
        ENDIF
C
      ENDIF
C
C++   Find the charge estimate from the sum of highest three samples
C++   and subtract the pedestal
C
      IF (IPH(IMAX-1).GT.IPH(IMAX+1)) THEN
        IF (IPH(IMAX-2).GT.IPH(IMAX+1)) THEN
          ICHG= IPH(IMAX-2)+IPH(IMAX-1)
        ELSE
          ICHG= IPH(IMAX-1)+IPH(IMAX+1)
        ENDIF
      ELSE
        IF (IPH(IMAX+2).GT.IPH(IMAX-1)) THEN
          ICHG= IPH(IMAX+2)+IPH(IMAX+1)
        ELSE
          ICHG= IPH(IMAX+1)+IPH(IMAX-1)
        ENDIF
      ENDIF
      ICHG=ICHG+IPH(IMAX)
C
C++   Calculate the threshold = 1/2 the maximum
C
      THR= RTHRTW*FLOAT(IPH(IMAX))
      ITHR=INT(THR)
C
C++   Check if the last presample or 1st postsample is above threshold
C
      GOOD= IPH(NPRETW).LT.ITHR.AND.IPH(NWSMP-NPSTTW+1).LT.ITHR
C
C++   Check if the last presample is below the first valid sample.
C++   Only a hardware failure can cause this to be false.
C
      IF (IPH(NPRETW).GT.IPH(NPRETW+1)) GOOD=.FALSE.
      IF (IPH(NWSMP-NPSTTW+1).GT.IPH(NWSMP-NPSTTW)) GOOD=.FALSE.
C
C++   Use the threshold method for good pulses
C
      IF (GOOD) THEN
C
C++     Find the first pulse above threshold
C
        DO 600 IS=NPRETW+1,IMAX
          IF (IPH(IS).GT.ITHR) THEN
C
C++         Interpolate to find the beginning time
C
            T1= FLOAT(IS-1) + (THR-FLOAT(IPH(IS-1)))
     &                             /FLOAT(IPH(IS)-IPH(IS-1))
            GO TO 601
          ENDIF
  600   CONTINUE
        CALL RERROR('TRDWP1',1,
     &          'Input data format error.  Unphysical pulse shape.')
        IER=4
        GOOD=.FALSE.
  601   CONTINUE
C
C++     Find the last pulse above threshold
C
        DO 700 IS=NWSMP-NPSTTW,IMAX,-1
          IF (IPH(IS).GT.ITHR) THEN
C
C++         Interpolate to find the ending time
C
            T2= FLOAT(IS+1) - (THR-FLOAT(IPH(IS+1)))
     &                             /FLOAT(IPH(IS)-IPH(IS+1))
            GO TO 701
          ENDIF
  700   CONTINUE
        CALL RERROR('TRDWP1',2,
     &          'Input data format error.  Unphysical pulse shape.')
        IER=4
        GOOD=.FALSE.
  701   CONTINUE
C
C++     Average the two times to get the final value
C
        TIM= FLOAT(IT0-1) + 0.5*(T1+T2)
      ENDIF
      IF (.NOT.GOOD) THEN
C
C++     The threshold method won't work, so we'll just use the
C++     old weighted mean method.
C
        ISMPH=0
        ISMPT=0
        DO 3323 IS=NPRETW+1,NWSMP-NPSTTW
          IF (IPH(IS).GE.JTHRTW) THEN
            ISMPH= ISMPH + IPH(IS)
            ISMPT= ISMPT + IPH(IS)*IS
          ENDIF
 3323   CONTINUE
        IF (ISMPH.EQ.0) THEN
          TIM=FLOAT(IT0-1)+0.5*FLOAT(NWSMP)
          IER=5
        ELSE
          TIM=FLOAT(IT0-1) + FLOAT(ISMPT)/FLOAT(ISMPH)
        ENDIF
      ENDIF
      END
#endif
