      SUBROUTINE ITLNK1(ITRY,TCURV,TCUER,TPHIS,ERPHI,IER)
C-----------------------------------------------------------------------
C! ITC: form links.
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- I. Tomalin  89/09/01
C!
C!   Input:
C!     ITRY   /I  and   TCURV /R  :
C!       Link curvatures have to be compatible with TCURV (the curvature
C!       of the TPC track), to within errors determined by ITRY.
C!     TCUER  /R  :
C!       The 1 sigma error**2 upon TCURV.
C!     TPHIS  /R  :
C!       The TPC track intersects the scattering radius at an azimuthal
C!       angle of TPHIS with respect to the origin.
C!     ERPHI  /R  :
C!       This is the 1 sigma uncertainty upon TPHIS.
C!      commons:     /ITRKCC/
C!                   /ITRKPA/
C!                   /ITRKER/
C!                   /ITGAPC/
C!                   /IROADC/
C!                   /ITWICC/
C!      params:       ITCOJJ
C!
C!   Output:
C!      IER    /I : Error flag : IER = 0
C!      IER = 1 if link search incomplete due to work bank problems etc.
C!      commons:     /BCS/,/IWBNKT/
C!      work bank:   ILNKIW
C!
C!   called by : IGICHA
C!   calls     : none
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "itrkcc.h"
#include "iroadc.h"
#include "itrker.h"
#include "itrkpa.h"
#include "itgapc.h"
#include "iwbnkt.h"
#include "itcojj.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Intialise:
C
      IER = 1
      IW(ILNKIW+LMHROW) = 0
      KITC0 = NLINK('ITCO',0)
C
C Loop over ITC layers.
C
      DO 60 LAY1=NLAYIR,2,-1
        N1 = NHITIR(LAY1)
        IF (N1.EQ.0) GOTO 60
C
C Loop over coordinates found in layer LAY1.
        DO 50 I=1,N1
          IH = IHITIR(I,LAY1)
          IF (IH.GT.0) PHI = RTABL(KITC0,ABS(IH),JITCP1)
          IF (IH.LT.0) PHI = RTABL(KITC0,ABS(IH),JITCP2)
          RADI = RTABL(KITC0,ABS(IH),JITCRA)
          SRPI = RTABL(KITC0,ABS(IH),JITCSR)
C
C Loop over layers inside LAY1.
          NLCON = 0
          DO 30 LAY2=LAY1-1,1,-1
            IF (LAY1-LAY2.GT.MXJMPD(1)+1) GOTO 50
C
            N2 = NHITIR(LAY2)
            IF (N2.EQ.0) GOTO 30
            NLCON = NLCON + 1
            IF (NLCON.GT.MXJMPC(1)+1) GOTO 50
C
            IGAP = IGAPIT(LAY2,LAY1)
            IF(IGAP.LE.0) GOTO 30
C
C Loop over coordinates found in layer LAY2.
            DO 20 J=1,N2
              JH = IHITIR(J,LAY2)
              IF (JH.GT.0) PHJ = RTABL(KITC0,ABS(JH),JITCP1)
              IF (JH.LT.0) PHJ = RTABL(KITC0,ABS(JH),JITCP2)
              RADJ = RTABL(KITC0,ABS(JH),JITCRA)
              SRPJ = RTABL(KITC0,ABS(JH),JITCSR)
C Calculate the link's curvature.
              DIJ2 = RADI**2 + RADJ**2 - 2.0*RADI*RADJ*COS(PHI-PHJ)
              DJK2 = RADJ**2 + RSCAIT**2 -
     +                               2.0*RADJ*RSCAIT*COS(PHJ-TPHIS)
              DKI2 = RSCAIT**2 + RADI**2 -
     +                               2.0*RSCAIT*RADI*COS(TPHIS-PHI)
              YYY = 2.0/SQRT(DIJ2*DJK2*DKI2)
              TA = YYY*(RADI*RADJ*SIN(PHI-PHJ) +
     +                  RADJ*RSCAIT*SIN(PHJ-TPHIS) +
     +                  RSCAIT*RADI*SIN(TPHIS-PHI))
C  Calculate the approximate error on the curvature.
C  (Assuming that ABS(TA) << 1.0/RADI).
              ERCUR = YYY*SQRT( SRPI*
     +         (RADJ*COS(PHI-PHJ)-RSCAIT*COS(TPHIS-PHI))**2
     +              +           SRPJ*
     +         (RSCAIT*COS(PHJ-TPHIS)-RADI*COS(PHI-PHJ))**2
     +              + ((RSCAIT*ERPHI)*(RADI*COS(TPHIS-PHI)-
     +                                 RADJ*COS(PHJ-TPHIS)))**2)
C
              IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
                WRITE(LDEBRL,14) (LROWS(ILNKIW)+1),LAY1,I,LAY2,J,
     +          TCURV,TA,ERCUR
   14           FORMAT(' ITLNK1 : LINK=',I3,' being tested. ',/,
     +          'LAY(HIT)1=',2I2,' LAY(HIT)2=',2I2,' TC=',E10.3,
     +          ' MC=',E10.3,' ERR=',E10.3)
              ENDIF
              IF (ABS(TA-TCURV).LT.SIGNUM(ITRY)*SQRT(ERCUR**2+TCUER))
     +        THEN
                KLNK = KNEXT(ILNKIW)
                IW(KLNK+1) = IH
                IW(KLNK+2) = JH
                IW(KLNK+3) = IGAP
                RW(KLNK+4) = TA
                IW(ILNKIW+LMHROW) = IW(ILNKIW+LMHROW) + 1
C
                IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
                  WRITE(LDEBRL,16) LROWS(ILNKIW),(IW(KLNK+II),II=1,3),
     +            RW(KLNK+4)
   16             FORMAT(5X,'ITLNK1 : Link formed: Link=',I4,
     +            '  Coords.= ',2I4,' IGAP',I4,' Curvature',F10.5)
                ENDIF
C See if the ILNKIW bank needs extending.
                IF(LFRWRD(ILNKIW).LE.0) THEN
                  LEN = IW(ILNKIW)
                  IF (LEN.GT.LMHLEN+4*2500) GOTO 900
                  CALL WBANK(IW,ILNKIW,LEN+4*50,*950)
                END IF
              ENDIF
C
   20       CONTINUE
   30     CONTINUE
   50   CONTINUE
   60 CONTINUE
      IER = 0
      GOTO 999
C
C Error returns.
  900 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL RERROR('ITLNK1',1,
     +'No. of links too great.')
      GOTO 999
  950 CONTINUE
      CALL RERROR('ITLNK1',2,
     +'Insufficient space to extend ILNKIW work bank.')
  999 CONTINUE
      END
#endif
