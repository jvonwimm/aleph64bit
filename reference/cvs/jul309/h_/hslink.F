      LOGICAL FUNCTION HSLINK(IDIR1,COR1A,COR2A,IDIR2,COR1B,COR2B)
C*************************************************************
C! Function to connect clusters to patterns                  *
C!                                                           *
C! Author  : G. BAGLIESI  880323                             *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C! HSLINK=.TRUE.  ->   patterns linked                       *
C! HSLINK=.FALSE. ->   patterns not linked                   *
C! IDIR = STRIP DIRECTION INDEX (HCAL)                       *
C! IDIR = 20  (ECAL)                                         *
C! IDIR = 30  (HCAL TOWER)                                   *
C?
C!======================================================================
#ifndef DOC
#include "hdpcut.h"
#include "alcons.h"
      PARAMETER(PH60 = 60.*DEGRA,PH120 = 120.*DEGRA, PH240 = 240.
     +*DEGRA,PH300 = 300.*DEGRA, PH360 = 360.*DEGRA,PH10 = 10.*DEGRA)

      PARAMETER (THLOW=35.*DEGRA,THHIGH=145.*DEGRA)
      HSLINK = .FALSE.
C
C  The relevant cases are selected
C
      IF(IDIR1.LT.10.AND.(IDIR2.EQ.20.OR.IDIR2.EQ.30)) THEN
         COR1  = COR1A
         COR2  = COR2A
         IDIR  = IDIR1
         THEEC = COR1B
         PHIEC = COR2B
         IDIRE = IDIR2
      ELSE IF((IDIR1.EQ.20.OR.IDIR1.EQ.30).AND.IDIR2.LT.10) THEN
         COR1  = COR1B
         COR2  = COR2B
         IDIR  = IDIR2
         THEEC = COR1A
         PHIEC = COR2A
         IDIRE = IDIR1
      ELSE
         GOTO 10
      ENDIF
C
C  else we try to link
C
         IF(IDIRE.EQ.20) THEN
            PHIL = PHI1HD
            YSUZ = YSZ1HD
         ELSE IF(IDIRE.EQ.30) THEN
            PHIL = PHI2HD
            YSUZ = YSZ2HD
         ENDIF
      IF(PHIEC.LT.0.) PHIEC = PHIEC + PH360
C
C  If the pattern is too large we treat it separately
C
      IF(IDIR.EQ.-1) THEN
         IF((ABS(COR2-PHIEC).LT.PHIL*4.
     +   .OR.ABS(COR2-PHIEC).GT.PH360-PHIL*4.)
     +   .AND.(THEEC.GE.THLOW.AND.THEEC.LE.THHIGH)) HSLINK = .TRUE.
         GOTO 10
      ENDIF
C
C  BARREL
C
      IF(IDIR.EQ.1) THEN
         IF((ABS(COR2-PHIEC).LT.PHIL.OR.ABS(COR2-PHIEC).GT.PH360-PHIL)
     +   .AND.(THEEC.GE.THLOW.AND.THEEC.LE.THHIGH)) HSLINK = .TRUE.
         GOTO 10
      ENDIF
C
C  ENDCAP
C
      YSUZD = COR2/COR1
C
C  We check which ENDCAP is
C
      IF(COS(THEEC)/COR1.LT.0.) GOTO 10
C
      IF(IDIR.EQ.2) THEN
         YSUZA = TAN(THEEC) * SIN (PHIEC)
         IF((PHIEC.LE.PH60+PH10.OR.PHIEC.GE.PH300-PH10).AND.
     +    ABS(YSUZD-YSUZA).LT.YSUZ) HSLINK = .TRUE.
         GOTO 10
      ENDIF
      IF(IDIR.EQ.3) THEN
         YSUZA = TAN(THEEC) * SIN (PHIEC)
         IF(PHIEC.GE.PH120-PH10.AND.PHIEC.LE.PH240+PH10.AND.
     +    ABS(YSUZD-YSUZA).LT.YSUZ) HSLINK = .TRUE.
         GOTO 10
      ENDIF
      IF(IDIR.EQ.4) THEN
         IF(COR1.GT.0.) THEN
            YSUZA = TAN(THEEC) * SIN (PHIEC-PH60)
         ELSE
            YSUZA = TAN(THEEC) * SIN (PHIEC-PH120)
         ENDIF
         IF(PHIEC.GE.PH60-PH10.AND.PHIEC.LE.PH120+PH10.AND.
     +    ABS(ABS(YSUZD)-YSUZA).LT.YSUZ) HSLINK = .TRUE.
         GOTO 10
      ENDIF
      IF(IDIR.EQ.5) THEN
         IF(COR1.GT.0.) THEN
            YSUZA = TAN(THEEC) * SIN (PHIEC-PH240)
         ELSE
            YSUZA = TAN(THEEC) * SIN (PHIEC-PH300)
         ENDIF
         IF(PHIEC.GE.PH240-PH10.AND.PHIEC.LE.PH300+PH10.AND.
     +    ABS(ABS(YSUZD)-YSUZA).LT.YSUZ) HSLINK = .TRUE.
         GOTO 10
      ENDIF
   10 CONTINUE
      RETURN
      END
#endif
