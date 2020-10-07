      SUBROUTINE GETXYB(IRUN,IFOUN,IFL,XYZ,DXYZ,OFS,VLUM)
C --------------------------------------------------------------
C! Gets the beam position per run from bank 'RXYZ'
C - J.Boucrot 15-Dec-1991
CKEY ALEF LFIL BEAM
C
C - Input     : IRUN     / INTE   = run number
C - Output arguments :
C         IFOUN    / INTE   = 0 if no information found for run IRUN
C                           = 1 if information found
C All arguments described below are defined only if IFOUN = 1 :
C         IFL      / INTE   = flag to tell how the beampos was obtained
C                           = -1 no beam position ( not enough events )
C                           =  0 position obtained with ITC/TPC only
C                           =  1 position obtained using the VDET
C         XYZ(3)   / REAL   = mean beam position :  X,Y,Z coordinates
C         DXYZ(3)  / REAL   = errors on X,Y,Z
C         OFS      / REAL   = offset ( mean D0 )
C         VLUM     / REAL   = VDET lumi for this run
C ----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rxyzjj.h"
      INTEGER ALGTRO
      REAL XYZ(*),DXYZ(*)
#include "bmacro.h"
C ----------------------------------------------------------------
      IFOUN = 0
      JRXYZ = ALGTRO('RXYZ',IRUN,JRXYRN,JROW)
      IF (JRXYZ.NE.0.AND.JROW.GT.0) THEN
         JRXYZ = IABS(JRXYZ)
         KRXYZ = KROW(JRXYZ,JROW)
         IFL = IW(KRXYZ+JRXYFL)
         CALL UCOPY(RW(KRXYZ+JRXYBX),XYZ(1),3)
         CALL UCOPY(RW(KRXYZ+JRXYEX),DXYZ(1),3)
         VLUM = RW(KRXYZ+JRXYLU)
         OFS  = RW(KRXYZ+JRXYOF)
         IFOUN=1
      ENDIF
 999  RETURN
      END
#endif
