      SUBROUTINE QTRKCH(NT,ICHG)      
C----------------------------------------------------------------------
CKEY EDIR TRACK CHARGE    
C! Get TPC Track Charge.
C-
C   Input  : NT   =  Track number
C   Output : ICHG =  Charge of track, 0 if error                
C-
C   Called by   : TRKWWS            
C   Calls  : None
C   Input banks : PFRT
C-
C                                      Author: J. Yamartino 28 May 1996
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
C --
      EXTERNAL ALFIEL, NAMIND
      INTEGER ICHG
      PARAMETER(AAAKEC=29.9792458*1.E-5)
      DATA NAPFRF / 0 /
C --
#include "bmacro.h"
C --
C   Standard momentum calculation from magnetic field,
C   inverse radius and the tangent of the dip angle.
C --
      QMFLD = ALFIEL(QMFLD)
      QMFLDC=QMFLD*AAAKEC
      ICHG = 0
C --
      IF( NAPFRF .EQ. 0 ) NAPFRF = NAMIND( 'PFRF' )
      KPFRF = IW(NAPFRF)
      IF(KPFRF.LE.0) GOTO 999
C --
      RINR = RTABL(KPFRF,NT,JPFRIR)
      IF(RINR.EQ.0.) GOTO 999
      PS = QMFLDC / RINR
      IF (PS .GT. 0.)  THEN
        ICHG =  1
      ELSE
        ICHG = -1
      ENDIF
C --
  999 RETURN
      END
#endif
