      SUBROUTINE QPTRCK(NT,PZ,PTOT)
C----------------------------------------------------------------------
CKEY EDIR TRACK MOMENTUM
C! Get TPC Track momentum.
C-
C   Input  : NT   =  Track number
C   Output : PZ   =  Track Z momentum component
C            PTOT =  Track total momentum
C-
C   Called by   : MUELID,TRKHAD
C   Calls  : None
C   Input banks : PFRT
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
C --
      PARAMETER(AAAKEC=29.9792458*1.E-5)
C --
#include "bmacro.h"
C --
C   Standard momentum calculation from magnetic field,
C   inverse radius and the tangent of the deep angle.
C --
      PTOT = 0.
      QMFLD = ALFIEL(QMFLD)
      QMFLDC=QMFLD*AAAKEC
C --
      KPFRF = IW(NAMIND('PFRF'))
      IF(KPFRF.LE.0) GOTO 999
C --
      RINR = RTABL(KPFRF,NT,JPFRIR)
      IF(RINR.EQ.0.) GOTO 999
      PS = QMFLDC / RINR
      IF (PS .GT. 0.)  THEN
        PT = PS
      ELSE
        PT = - PS
      ENDIF
      TL = RTABL(KPFRF,NT,JPFRTL)
      PZ = PT * TL
      PTOT = SQRT (PT**2 + PZ**2)
C --
  999 RETURN
      END
#endif
