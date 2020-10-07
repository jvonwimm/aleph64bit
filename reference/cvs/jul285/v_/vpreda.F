      SUBROUTINE VPREDA
C--------------------------------------------------------------------
C!   Prepare data step before reconstruction
C!   Author    : G. Bagliesi   860910
C!   Modifiew  : D. Brown 14-5-90
C!
C!   Input      : Minivertex raw data  (VPLH,VHLS banks)
C!   Output     : Vertex detector final hits and coordinates
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C!   Modified  : Manoj Thulasidas Dec 5, 1994
C!   New scheme for smearing of MC events
C!
C!   Modified  : Manoj Thulasidas May 15, 1996
C!   Take out the ad-hoc call to VDMCEF95 which does face suppression
C!   It has been moved to VDMCEF under the name VDFSUP
C?
C!======================================================================
C
#ifndef DOC
#include "vrecon.h"
      INTEGER IER
C
C  Local variables
C
      INTEGER I,ISTAT
      LOGICAL LASER,DUMMY
C
C  Functions
C
      LOGICAL VTRLAS
      EXTERNAL VTRLAS 
C
C  Make the final clusters from the raw data banks
C
      IF(MKVFPH)THEN
        CALL VDECOD
      END IF
C
C  Convert these to positions
C
      IF(MKVDXY)THEN
        CALL VDXYZT
      END IF
C
C call VDNOIS if it is a special run to produce the hot channel list
C check for a laser trigger first
C
      LASER = VTRLAS(DUMMY)
      IF(MKVHOT.AND..NOT.LASER)CALL VDNOIS
C
C  Special for MC; kill the dead lines
C  And call the hit smearing routine
C
      IF (MCEVNT) THEN
        CALL VDMCEF(IER)
        IF(IER.LT.0) CALL RERROR('VDMCEF',IER,' Error in VDEM bank')
        CALL VDISMR
      ENDIF
C
      RETURN
      END
#endif
