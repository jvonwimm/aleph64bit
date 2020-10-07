      SUBROUTINE TWPOLD(IT0,NWSMP,IPH,ICHG,TIM,NSMPO,IER)
C
C------------------------------------------------------------------------
C! Reduce a TPC wire pulse to charge and time, as was done in the TPP for
C! all data taken so far.  This is the default production algorithm.
CKEY TPC WIRE REDUCTION
C
C  Author:  R. Johnson    21-1-91
C
C  Input:    IT0       /I        Time of 1st bucket in the pulse
C            NWSMP     /I        Number of samples in the pulse
C            IPH(n)    /I        Array of sample pulse heights
C  Output:   ICHG      /I        Charge estimate
C            TIM       /R        Time estimate
C            NSMPO     /I        Number of samples above threshold
C            IER       /I        Error code
C                                0= all OK
C                                1= pulse does not appear to be from
C                                   a single track
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "twrprm.h"
C
      DIMENSION IPH(NWSMP)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C++   Check if the last presample is higher than the 1st valid sample
C++   (The hardware should not allow this).
C
      IF (IPH(NPREWP).GT.IPH(NPREWP+1)) THEN
        IER=2
        RETURN
      ENDIF
C
C++   Loop only on the samples above the TPD threshold
C     (i.e. skip the pre and post samples) and find the total
C     charge and charge-weighted time above a threshold
C
      ITHEF=JTHRWP+IPEDWP
      NSAT=0
      ICHG=0
      ISMPT=0
      NSMPO=0
      DO 300 IS=1,NWSMP
        IF (IPH(IS).GT.ITHEF) THEN
          IF (IPH(IS).EQ.255) THEN
            NSAT=NSAT+1
C
C++         Skip this pulse if too many samples are saturated
C
            IF (NSAT.GT.MXSTWP) THEN
              IER=5
              RETURN
            ENDIF
          ENDIF
          NSMPO=NSMPO+1
          ICHG= ICHG + (IPH(IS)-IPEDWP)
          ISMPT= ISMPT + (IPH(IS)-IPEDWP)*IS
        ENDIF
  300 CONTINUE
      IF (NSMPO.GT.MXABWP) THEN
        IER=6
        RETURN
      ENDIF
      IF (ICHG.EQ.0) THEN
        TIM=FLOAT(IT0-1) + 0.5*FLOAT(NWSMP)
      ELSE
        TIM=FLOAT(IT0-1) + FLOAT(ISMPT)/FLOAT(ICHG)
      ENDIF
      IER=0
C
      END
#endif
