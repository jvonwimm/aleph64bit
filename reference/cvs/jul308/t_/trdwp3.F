      SUBROUTINE TRDWP3(NSAT,IT0,NWSMP,IPH,NHIT,IQ,TIME,LENGTH,IQUAL)
C
C-----------------------------------------------------------------------
C! Reduce TPC wire pulses not handled by the TPP, using the default
C! algorithm used for all data taken so far.
C
C  Author:    R. Johnson    16-1-91
C  Modified:  R. Johnson    24-8-92   Return LMPH to calling routine
C  Modified:  R. Johnson    2-3-93    Fix bug such that LMPH REALLY
C                                     is returned
C             R. Johnson    5-9-93    Major changes to add possibility
C                                     of splitting double peaks.  Also,
C                                     use threshold alg. for times.
C
C  Input:    NSAT      /I        Number of saturated samples
C            IT0       /I        Time of 1st bucket in the pulse
C            NWSMP     /I        Number of samples in the pulse
C            IPH(n)    /I        Array of sample pulse heights
C  Output:   NHIT
C            IQ        /I        Charge estimate
C            TIME      /R        Time estimate
C            LENGTH    /I        Length of pulse above threshold
C            IQUAL     /I        Error code
C                                0= all OK
C
C----------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION IQ(*),TIME(*),LENGTH(*),IQUAL(*)
C
#include "tpgpar.h"
#include "tpccon.h"
#include "twirct.h"
C
      DIMENSION IPH(NWSMP),IPK(2),T(2),PH(2),I1(2),I2(2)
      LOGICAL FMIN,GOOD
C
      IER=0
C
C++   Completely ignore the first and last sample if there are
C++   more than one pre and post samples.
C
      IF (NPRETW.GT.1) THEN
        JS1=2
      ELSE
        JS1=1
      ENDIF
      IF (NPSTTW.GT.1) THEN
        JS2=NWSMP-1
      ELSE
        JS2=NWSMP
      ENDIF
C
C++   Loop only on the samples above the TPD threshold
C     (i.e. skip the pre and post samples) and find the total
C     charge and charge-weighted time above a threshold
C
      IS2=0
      ICHG=0
      ISMPT=0
      LMPH=0
      FMIN=.FALSE.
      DO 300 IS=JS1,JS2
        IF (IPH(IS).GT.JTHRTW) THEN
          LMPH=LMPH+1
          ICHG= ICHG + IPH(IS)
          ISMPT= ISMPT + IPH(IS)*IS
          IS2=IS2 + IPH(IS)*(IS**2)
C
C++       Look for a local minimum in the pulse
C
          IF (IS.GT.NPRETW .AND. IS.LE.(NWSMP-NPSTTW)) THEN
            IF (IPH(IS).LT.IPH(IS-1)) THEN
              IF (IPH(IS).LT.IPH(IS+1)) THEN
                IF (FMIN) THEN
                  IER=1
                ELSE
                  FMIN=.TRUE.
                  IMN=IS
                ENDIF
              ELSEIF (IPH(IS).EQ.IPH(IS+1)) THEN
                IF (IPH(IS).LT.IPH(IS+2)) THEN
                  IF (FMIN) THEN
                    IER=1
                  ELSE
                    FMIN=.TRUE.
                    IMN=IS
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  300 CONTINUE
      IF (FMIN.AND.NSAT.GT.0) THEN
        IER=6
      ENDIF
      IF (IER.EQ.0.AND.FMIN) THEN
        IER=4
C
C++     Try to analyze the pulse as a double peak
C
        NPK=0
        NHIT=0
        DO 500 IS=NPRETW+1,NWSMP-NPSTTW
          IF (IPH(IS).GT.IPH(IS-1)) THEN
            IF (IPH(IS).GT.IPH(IS+1)) THEN
              NPK=NPK+1
              IF (NPK.GT.2) GO TO 501
              IPK(NPK)=IS
            ELSEIF (IPH(IS).EQ.IPH(IS+1)) THEN
              IF (IPH(IS).GT.IPH(IS+1)) THEN
                NPK=NPK+1
                IF (NPK.GT.2) GO TO 501
                IPK(NPK)=IS
              ENDIF
            ENDIF
          ENDIF
  500   CONTINUE
  501   CONTINUE
        IF (NPK.EQ.2) THEN
          IF (IPK(1).LT.IMN-1.AND.IPK(2).GT.IMN+1) THEN
            CALL TPARAB(FLOAT(IPK(1)),IPH(IPK(1)-1),T(1),PH(1))
            CALL TPARAB(FLOAT(IPK(2)),IPH(IPK(2)-1),T(2),PH(2))
            IF ((T(2)-T(1))*TVDRFT(1).GT.SPACTW) THEN
              I1(1)=JS1
              I2(1)=IMN-1
              I1(2)=IMN+1
              I2(2)=JS2
              NHIT=2
              DO 603 K=1,NHIT
                ICHG=IPH(IMN)/2
                ISMPT=ICHG*IMN
                IS2=ICHG*(IMN**2)
                LMPH=1
                DO 600 IS=I1(K),I2(K)
                  IF (IPH(IS).GT.JTHRTW) THEN
                    ICHG=ICHG+IPH(IS)
                    ISMPT= ISMPT + IPH(IS)*IS
                    IS2=IS2 + IPH(IS)*(IS**2)
                    LMPH=LMPH+1
                  ENDIF
  600           CONTINUE
                IF (ICHG.GT.0) THEN
                  RNS=FLOAT(ICHG)
                  WRM=FLOAT(IS2)/RNS - (FLOAT(ISMPT)/RNS)**2
                ELSE
                  WRM=0.
                ENDIF
                IQ(K)=ICHG
                TIME(K)=FLOAT(IT0-1)+T(K)
                LENGTH(K)=LMPH
                IF ((IPH(IPK(1))*10)/IPH(IMN).GT.MNRATW
     &                    .AND.ICHG.GT.0.AND.WRM.LE.WRMXTW) THEN
                  IER=0
                  IQUAL(K)=0
                ELSE
                  IQUAL(K)=5
                ENDIF
  603         CONTINUE
            ENDIF
          ENDIF
        ENDIF
        IF (IER.EQ.0) GOTO 999
      ENDIF
C
C++   Calculate and cut on the RMS width of the pulse for singles
C
      NHIT=1
      RT0=FLOAT(IT0-1)
      IF (ICHG.EQ.0) THEN
        TIM=RT0+0.5*FLOAT(NWSMP)
        IER=3
        ITHR=-255
      ELSE
        RNS=FLOAT(ICHG)
        TIM=RT0 + FLOAT(ISMPT)/RNS
        WRM= FLOAT(IS2)/RNS - (FLOAT(ISMPT)/RNS)**2
        IF (WRM.GT.WRMXTW) THEN
          IER=2
        ENDIF
        THR=THRSTW*RNS/FLOAT(LMPH)
        ITHR=INT(THR)
      ENDIF
C
C++   Calculate the time estimate from 2 threshold crossings
C
      GOOD= IPH(NPRETW).LT.ITHR.AND.IPH(NWSMP-NPSTTW+1).LT.ITHR
      IF (IPH(NPRETW).GT.IPH(NPRETW+1)) GOOD=.FALSE.
      IF (IPH(NWSMP-NPSTTW+1).GT.IPH(NWSMP-NPSTTW)) GOOD=.FALSE.
      IF (GOOD) THEN
        DO 660 IS=NPRETW+1,JS2
          IF (IPH(IS).GT.ITHR) THEN
            T1= FLOAT(IS-1) + (THR-FLOAT(IPH(IS-1)))
     &                             /FLOAT(IPH(IS)-IPH(IS-1))
            GO TO 661
          ENDIF
  660   CONTINUE
        CALL RERROR('TRDWP3',1,
     &          'Input data format error.  Unphysical pulse shape.')
        IER=7
        GOOD=.FALSE.
  661   CONTINUE
        DO 700 IS=NWSMP-NPSTTW,JS1,-1
          IF (IPH(IS).GT.ITHR) THEN
            T2= FLOAT(IS+1) - (THR-FLOAT(IPH(IS+1)))
     &                             /FLOAT(IPH(IS)-IPH(IS+1))
            GO TO 701
          ENDIF
  700   CONTINUE
        CALL RERROR('TRDWP3',2,
     &          'Input data format error.  Unphysical pulse shape.')
        IER=7
        GOOD=.FALSE.
  701   CONTINUE
        TIME(NHIT)=RT0+0.5*(T1+T2)
      ENDIF
      IF (.NOT.GOOD) THEN
        TIME(NHIT)=TIM
      ENDIF
C
      IQ(NHIT)=ICHG
      LENGTH(NHIT)=LMPH
      IQUAL(NHIT)=IER
C
  999 RETURN
C
      END
#endif
