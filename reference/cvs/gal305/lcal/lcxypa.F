*DK lcxypa
      SUBROUTINE LCXYPA(MODU,LAY,XYZ,IPAD)
C--------------------------------------------------------------
C!   Convert coordinates into tower address
C!   Author   : J.Dines Hansen & P.Hansen - 860301
C!   Modified : P.Hansen - 870920
C!
C! - INPUT : LAY      = Layer No.               [1,38]
C!           MODU     = Module No.              [1,4]
C!           XYZ(3)   = subcomponent coordinates
C! - OUTPUT: IPAD     = Addr. (ICOL + 16*(IROW-1) + 512*(MODU-1))
C!                      (0 if XYZ is outside the active volume)
C!                      -1 if XYZ is outside the pads but inside
C!                       the wires)
C!
C! - Called by  LCSHOW                          from this .HLB
      SAVE
      DIMENSION XYZ(*)
#include "jqcom.h"
#include "iocom.h"
#include "ljjpar.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "lccomd.h"
#include "bmacro.h"
C ------------------------------------------------------------
#ifndef DOC
C? IF input is out of range THEN report error
      IPAD=0
      IF(LAY.GT.LCNWPL .OR. LAY.LE.0)     GOTO 998
      IF(MODU.LE.0.OR.MODU.GT.4)          GOTO 998
C
C? Geometry bank pointers
      KLMTY = IW(NALMTY)
      KLLAY = IW(NALLAY)
      KLWRG = IW(NALWRG)
      KLCPG = IW(NALCPG)
C
C? Impact point in wire plane quadrant
      X = ABS(XYZ(1))
      Y = ABS(XYZ(2))
C
C? Set flag for alternating dead zones
      IF (MOD(LAY,2).EQ.0 .AND. XYZ(1)*XYZ(2).GT.0.) THEN
        IFDED = 1
        IODD = 2
      ELSEIF (MOD(LAY,2).EQ.1 .AND. XYZ(1)*XYZ(2).LT.0.) THEN
        IFDED = 0
        IODD = 2
      ELSE
        IFDED = 0
        IODD =1
      ENDIF
C
C? IF impact point not in the active area THEN IPAD=0
C? IF impact point is not covered by pads THEN IPAD=-1
      IF (X .LT. RTABL(KLWRG,1,JLWRXL))          GOTO 999
      IF (X .GT. RTABL(KLWRG,20,JLWRXL)
     &          +RTABL(KLWRG,20,JLWRTW))         GOTO 999
C? Loop over tubes
      DO 10 I=1,19
        IF(X.GT.RTABL(KLWRG,I+1,JLWRXL))        GOTO  10
        DY2 = RTABL(KLWRG,I,JLWRD2)
        YHIGH = RTABL(KLWRG,I,JLWRYH)
        YLOW  = RTABL(KLWRG,I,JLWRYL)
        IF(YLOW.GT.DY1) YLOW = YLOW+DY1
        IF(IFDED.NE.0) THEN
          YHIGH = YHIGH - DY2
        ELSE
          YHIGH = YHIGH - DY1
        ENDIF
C? Throw hits outside end supports
        IF(Y.GT.YHIGH.OR.Y.LT.YLOW)             GOTO 999
C
C? Throw hits on the cathode the places where it is cut
        IF(I.EQ.5.AND.Y.LT.Y5CUT)               GOTO 997
        IF(I.LE.5.AND.Y.LT.Y0CUT-X*DYCUT)       GOTO 997
C
C? Throw hits on top of internal supports
C?  These are placed differently in the four modules
C?  Module 1
        IF(MODU.EQ.1) THEN
          IF(XYZ(2).GT.0.) THEN
            IF(Y.GT.YSUP(1,1)-DY3.AND.
     &         Y.LT.YSUP(1,1)+DY3.AND.
     &         X.GT.XLSUP(1,1).AND.X.LT.XHSUP(1,1)) GOTO 999
          ELSE
            IF(Y.GT.YSUP(2,1)-DY3.AND.
     &         Y.LT.YSUP(2,1)+DY3.AND.
     &         X.GT.XLSUP(2,1).AND.X.LT.XHSUP(2,1)) GOTO 999
          ENDIF
C?  Module 2
        ELSEIF(MODU.EQ.2) THEN
          DY4 = 0.
          IF(MOD(LAY,2).EQ.1) THEN
            IF(XYZ(2).GT.0) THEN
              DY4 = -1.75
            ELSE
              DY4 =  1.75
            ENDIF
          ENDIF
          IF(Y.GT.YSUP(IODD,1)-DY3+DY4.AND.
     &       Y.LT.YSUP(IODD,1)+DY3+DY4.AND.
     &       X.GT.XLSUP(IODD,1).AND.X.LT.XHSUP(IODD,1)) GOTO 999
C?  Module 3
        ELSEIF(MODU.EQ.3) THEN
          IF(XYZ(2).GT.0.) THEN
            IF(Y.GT.YSUP(2,1)-DY3.AND.
     &         Y.LT.YSUP(2,1)+DY3.AND.
     &         X.GT.XLSUP(2,1).AND.X.LT.XHSUP(2,1)) GOTO 999
          ELSE
            IF(Y.GT.YSUP(1,1)-DY3.AND.
     &         Y.LT.YSUP(1,1)+DY3.AND.
     &         X.GT.XLSUP(1,1).AND.X.LT.XHSUP(1,1)) GOTO 999
          ENDIF
C?  Module 4
        ELSE
          IF(Y.GT.YSUP(IODD,1)-DY3.AND.
     &       Y.LT.YSUP(IODD,1)+DY3.AND.
     &       X.GT.XLSUP(IODD,1).AND.X.LT.XHSUP(IODD,1)) GOTO 999
        ENDIF
C
C? Finally check the internal support at large x
        IF(IODD.EQ.2.AND.Y.GT.YSUP(1,2)-DY3
     &              .AND.Y.LT.YSUP(1,2)+DY3.AND.
     &       X.GT.XLSUP(1,2).AND.X.LT.XHSUP(1,2)) GOTO 999
C
        GOTO 20
   10 CONTINUE
C
C? Determine pad size and offset
   20 SIZE = RTABL(KLMTY,1,JLMTCC)*FLOAT(ITABL(KLLAY,LAY,JLLAPS))
      DX = FLOAT(MOD(ITABL(KLLAY,LAY,JLLAPS),2))*HUNIT
C
C? Apply corrections if present
      IF(KLCPG.GT.0) THEN
        SIZE = SIZE+RTABL(KLCPG,LAY,JLCPPS)
        DX   = DX  +RTABL(KLCPG,LAY,JLCPPO)
      ENDIF
C
C? Determine row and column
      IY = MIN0(15,INT((Y-HUNIT)/SIZE)+1)
      IX = MIN0(16,INT((X-DX)/SIZE+.5))
      IF(IX.LE.0)                                 GOTO 999
      IF(IY.EQ.0) IY=1
C
      IF(IY.GE.1.AND.IY.LE.3) THEN
        IX = MAX0(4,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.4) THEN
        IX = MAX0(3,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.5) THEN
        IX = MAX0(2,IX)
        GOTO 30
      ENDIF
C
      IF(IY.GE.6.AND.IY.LE.8) THEN
        IX = MIN0(15,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.9) THEN
        IX = MIN0(14,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.10.OR.IY.EQ.11) THEN
        IX = MIN0(13,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.12) THEN
        IX = MIN0(12,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.13) THEN
        IX = MIN0(11,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.14) THEN
        IX = MIN0(10,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.15) THEN
        IX = MIN0(8,IX)
        IX = MAX0(3,IX)
        GOTO 30
      ENDIF
   30 CONTINUE
C
      IF (XYZ(2) .GE. 0.) THEN
        IY = IY + 15
      ELSE
        IY = 16 - IY
      ENDIF
C
C? Encode channel address
      IPAD    = IX + 16*(IY-1) + 512*(MODU-1)
      GOTO 999
C
  997 IPAD = -1
      GOTO 999
C
  998 WRITE(LOUTIO,1000) MODU,LAY
  999 CONTINUE
C
 1000 FORMAT(' +++ LCXYPA +++ Argument out of range',2I10)
      END
#endif
