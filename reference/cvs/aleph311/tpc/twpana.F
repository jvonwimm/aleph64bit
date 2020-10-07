      SUBROUTINE TWPANA(IT0,NWSMP,IPH,ICHG,TIM,IER)
C
C----------------------------------------------------------------------
C! Reduce a TPC wire pulse to charge and time, as done in the TPP
C! This algorithm has not been used for Aleph production.
C! See TWPOLD.
CKEY TPC WIRE REDUCTION
C
C  Author:  R. Johnson    21-1-91
C
C  Input:    IT0       /I        Time of 1st bucket in the pulse
C            NWSMP     /I        Number of samples in the pulse
C            IPH(n)    /I        Array of sample pulse heights
C  Output:   ICHG      /I        Charge estimate
C            TIM       /R        Time estimate
C            IER       /I        Error code  0= all OK
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "twrprm.h"
C
      PARAMETER (MXPRT=5)
      DIMENSION IPH(NWSMP)
      DATA NPRT/0/
C
C++   Check if the last presample is higher than the 1st valid sample
C++   (The hardware should not allow this).
C
      IF (IPH(NPREWP).GT.IPH(NPREWP+1)) THEN
        IER=2
        RETURN
      ENDIF
      IF (IPH(NWSMP-NPSTWP+1).GT.IPH(NWSMP-NPSTWP)) THEN
        IER=2
        RETURN
      ENDIF
C
C++   Loop only on the samples above the TPD threshold
C     (i.e. skip the pre and post samples) and find the maximum
C
      DO 300 IS=NPREWP+2,NWSMP-NPSTWP
        IF (IPH(IS).LT.IPH(IS-1)) THEN
          IMAX=IS-1
          GO TO 301
        ENDIF
  300 CONTINUE
C
C++   No maximum found.  Not reducable.
C
      IER=3
      RETURN
  301 CONTINUE
C
C++   Check for too much saturation
C
      NSAT=0
      IS=IMAX
  320 CONTINUE
        IF (IPH(IS).EQ.255) THEN
          NSAT=NSAT+1
          IF (NSAT.GT.MXSTWP) THEN
            IER=5
            RETURN
          ENDIF
          IF (IS.EQ.1) GO TO 321
          IS=IS-1
          GO TO 320
        ENDIF
  321 CONTINUE
C
C++   Loop over the remaining valid samples to look for valleys
C
      DO 400 IS=IMAX+2,NWSMP-NPSTWP
        IF (IPH(IS).GT.IPH(IS-1)) THEN
          IER=4
          RETURN
        ENDIF
  400 CONTINUE
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
      ICHG=ICHG+IPH(IMAX) - 3*IPEDWP
C
C++   Calculate the threshold = 1/2 the maximum
C
      THR= RTHRWP*FLOAT(IPH(IMAX)-IPEDWP) + FLOAT(IPEDWP)
      ITHR=INT(THR)
C
C++   Skip if the last presample or 1st postsample is above threshold
C
      IF (IPH(NPREWP).GE.ITHR) THEN
        IER=6
        RETURN
      ENDIF
      IF (IPH(NWSMP-NPSTWP+1).GE.ITHR) THEN
        IER=7
        RETURN
      ENDIF
C
C++   Find the first pulse above threshold
C
      DO 600 IS=NPREWP+1,IMAX
        IF (IPH(IS).GT.ITHR) THEN
C
C++       Interpolate to find the beginning time
C
          T1= FLOAT(IS-1) + (THR-FLOAT(IPH(IS-1)))
     &                           /FLOAT(IPH(IS)-IPH(IS-1))
          GO TO 601
        ENDIF
  600 CONTINUE
      IF (NPRT.LT.MXPRT) THEN
        NPRT=NPRT+1
        WRITE(6,631) (IPH(K),K=1,NWSMP)
  631   FORMAT(' TWPANA: unphysical pulse shape: ',/
     &         20(1X,I3))
      ENDIF
      IER=8
      RETURN
  601 CONTINUE
C
C++   Find the last pulse above threshold
C
      DO 700 IS=NWSMP-NPSTWP,IMAX,-1
        IF (IPH(IS).GT.ITHR) THEN
C
C++       Interpolate to find the ending time
C
          T2= FLOAT(IS+1) - (THR-FLOAT(IPH(IS+1)))
     &                           /FLOAT(IPH(IS)-IPH(IS+1))
          GO TO 701
        ENDIF
  700 CONTINUE
      IF (NPRT.LT.MXPRT) THEN
        NPRT=NPRT+1
        WRITE(6,631) (IPH(K),K=1,NWSMP)
      ENDIF
      IER=9
      RETURN
  701 CONTINUE
C
C++   Average the two times to get the final value
C
      TIM= FLOAT(IT0-1) + 0.5*(T1+T2)
      IER=0
C
      END
#endif
