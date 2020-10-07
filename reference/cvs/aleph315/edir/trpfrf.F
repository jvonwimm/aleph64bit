      SUBROUTINE TRPFRF(I,PP1,IRF)
C-----------------------------------------------------------------------
CKEY EDIR TRACKS 4-MOMENTUM
C! Charged tracks momentum.
C-
C   Input  : I   = PFRF track number
C   Output : PP1 = PX, PY, PZ and PTOT*charge
C            IRF = Return code
C                   0 : success
C                  -1 : overflow problem in pt value
C-
C   Called by   : LEPTO,TRUSLU
C   Calls  : None
C   Input banks : PFRF
C-
C                                Author: J.C.Brient     date : 29/6/89
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
C --
      PARAMETER ( COSPT = 0.3 , BFIL = 1.5)
      DIMENSION PP1(4)
      LOGICAL DEBUG
      DATA DEBUG/.FALSE./
C --
#include "bmacro.h"
C --
      IRF   = 0
      JPFRF = IW(NAMIND('PFRF'))
      IF(JPFRF.LE.0) GOTO 999
C --
      RT    = 100. * RTABL(JPFRF,I,JPFRIR)
      IF(ABS(RT).LT.0.000001) GO TO 999
      IF(ABS(RT).GT.1000000.) GO TO 999
      FPHI  = RTABL(JPFRF,I,JPFRP0)
      PXY = COSPT * BFIL/RT
      IF(PXY .GT. 0.) THEN
         CH = -1.
      ELSE
         CH = 1.
         PXY = - PXY
      ENDIF
      PP1(1)   =  PXY * COS(FPHI)
      PP1(2)   =  PXY * SIN(FPHI)
      PP1(3)   =  PXY * RTABL(JPFRF,I,JPFRTL)
      PP = VMOD(PP1,3)
      PP1(4)   = CH * PP
C --
      RETURN
  999 CONTINUE
      IRF = -1
      RETURN
      END
#endif
