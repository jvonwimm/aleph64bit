      SUBROUTINE TPCT0(IER,DT)
C
C-----------------------------------------------------------------------
C! TPC T0 monitoring
C!
C!  Author:   W. Wiedenmann   12-09-89
C!  Modified: R. Johnson      18-09-89
C!  Modified :- E. Lancon             25-FEB-1992
C!     use alephlib routine TPDVEL to load drift velocity
C!
C!  Output:  IER      /I     If non-zero, then no valid T0 info for
C!                           this event
C!           DT       /R     Difference in time between TPC side A
C!                           and side B in microseconds.
C!
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "alcons.h"
#include "frftjj.h"
#include "frtljj.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "tpcojj.h"
#include "tpgeom.h"
#include "tpccon.h"
#include "rlunit.h"
C
      DIMENSION DVSA (3), DVSB (3)
      DIMENSION NTR(2),ZV(2),CUTLM(2),NEND(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
C
C------ Initialize cuts on the first call
C
        NFRFT=NAMIND('FRFT')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
        NTPCO=NAMIND('TPCO')
C
        KTCT0  = NLINK('TCT0',0)
        IF (KTCT0.GT.0) THEN
          NTRCT    = IW(KTCT0+1)
          CUTLM(1) = RW(KTCT0+2)
          CUTLM(2) = RW(KTCT0+3)
          CUTD0    = RW(KTCT0+4)
          CUTZ0    = RW(KTCT0+5)
          CUTRI    = RW(KTCT0+6)
          NHCUT    = IW(KTCT0+7)
          IF (LOUTRL.GT.0) WRITE(LOUTRL,100)
     +      NTRCT,CUTLM(1),CUTLM(2),CUTD0,CUTZ0,CUTRI,
     +      NHCUT
 100      FORMAT(/
     +      '  TPC T0 MONITOR initialized on first call        '/,
     +      '  -----------                                     '/,
     +      '  TPC-T0 : Cuts found on cards                    '/,
     +      '  1) min. number of fitted tracks on one side.... ',
     +      I10/,
     +      '  2) min./max. lamda for a track (degrees)....... ',
     +      F10.3,' / ',F10.3/,
     +      '  3) max. allowed d0 (cm)........................ ',
     +      F10.3/,
     +      '  4) max. allowed z0 (cm)........................ ',
     +      F10.3/,
     +      '  5) max. invers radius.......................... ',
     +      F10.5/,
     +      '  6) number of required hits on a track.......... ',
     +      I10)
        ELSE
          NTRCT    = 2
          CUTLM(1) = 0.
          CUTLM(2) = 50.
          CUTD0    = 2.
          CUTZ0    = 10.
          CUTRI    = 0.002
          NHCUT    = 10
          IF (LOUTRL.GT.0) WRITE(LOUTRL,200)
     +      NTRCT,CUTLM(1),CUTLM(2),CUTD0,CUTZ0,CUTRI,
     +      NHCUT
 200      FORMAT(/
     +      '  TPC T0 MONITOR initialized on first call        '/,
     +      '  -----------                                     '/,
     +      '  TPC-T0 : Default cuts used **********           '/,
     +      '  1) min. number of fitted tracks on one side.... ',
     +      I10/,
     +      '  2) min./max. lamda for a track (degrees)....... ',
     +      F10.3,' / ',F10.3/,
     +      '  3) max. allowed d0 (cm)........................ ',
     +      F10.3/,
     +      '  4) max. allowed z0 (cm)........................ ',
     +      F10.3/,
     +      '  5) max. invers radius.......................... ',
     +      F10.5/,
     +      '  6) number of required hits on a track.......... ',
     +      I10)
        ENDIF
      ENDIF
      IER = 1
      DT = 999999.
      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      IF (KFRFT.LE.0 .OR. KFRTL.LE.0) GOTO 999
      KFTCL = IW(NFTCL)
      KTPCO = IW(NTPCO)
      IF (KFTCL.LE.0 .OR. KTPCO.EQ.0) GOTO 999
      CALL TPDVEL ( ' ', DVSA, DVSB, IER1)
      IF ( IER1.NE.0 ) GOTO 999
      VD = 0.5*(DVSA(3) + DVSB(3))
      IF ( VD.LE.0. ) GOTO 999
C
      NTRKS = LROWS(KFRFT)
      IF(NTRKS.LT.(2*NTRCT)) GOTO 999
C
      DO 10 I=1,2
        NTR(I) = 0
   10 ZV(I) = 0.0
C
C-----Loop over tracks, selecting suitable ones.
C
      DO 20 I=1,NTRKS
        NTPC=ITABL(KFRTL,I,JFRTNT)
        IF (NTPC.EQ.0)                                      GOTO 21
        ALAMDA = RADEG * ATAN(RTABL(KFRFT,I,JFRFTL))
        IF(ABS(ALAMDA).LT.CUTLM(1))                         GOTO 20
        IF(ABS(ALAMDA).GT.CUTLM(2))                         GOTO 20
        D0 = RTABL(KFRFT,I,JFRFD0)
        IF(ABS(D0) .GT.CUTD0 )                              GOTO 20
        Z0 = RTABL(KFRFT,I,JFRFZ0)
        IF(ABS(Z0) .GT.CUTZ0 )                              GOTO 20
        RI = RTABL(KFRFT,I,JFRFIR)
        IF(ABS(RI) .GT.CUTRI )                              GOTO 20
        IF(NTPC.LT.NHCUT)                                   GOTO 20
C
C------- Take only tracks with all coordinates in 1 TPC end
C
        NEND(1)=0
        NEND(2)=0
        IOFF=KFTCL+LMHLEN+ITABL(KFRTL,I,JFRTIT)
        DO 17 JJ=1,NTPC
          IC=IW(IOFF+JJ)
          ID=ITABL(KTPCO,IC,JTPCIN)
          IROW=ID/100000
          ISLOT=(ID-IROW*100000)/1000
          JEND=IENDTP(ISLOT)
          NEND(JEND)=NEND(JEND)+1
   17   CONTINUE
        IF (NEND(1).NE.0 .AND. NEND(2).NE.0)                GOTO 20
C
        IF (NEND(1).GT.0) THEN
          ISIDE = 1
        ELSE
          ISIDE = 2
        ENDIF
        NTR(ISIDE) = NTR(ISIDE) + 1
        ZV(ISIDE) = ZV(ISIDE) + Z0
C
   20 CONTINUE
   21 CONTINUE
      IF ((NTR(1).GE.NTRCT) .AND. (NTR(2).GE.NTRCT)) THEN
        DZE   = ZV(1)/FLOAT(NTR(1)) - ZV(2)/FLOAT(NTR(2))
        DT = DZE/VD
C
C--      Accumulate sums in TMONIT for printout at end of job
C
        NT0STM=NT0STM+1
        T0SHTM=T0SHTM+DT
        T0S2TM=T0S2TM+DT**2
      ENDIF
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
