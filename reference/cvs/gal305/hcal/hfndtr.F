*DK hfndtr
      SUBROUTINE HFNDTR(COOR,IPOR,ITOW,ITRI)
C---------------------------------------------------------------
C
C!         Evaluate theta phi trigger adress
C!
C!                         Author: G.Catanesi 25/08/88
C!
C! - Input:
C!         COOR(1-3)/R  : X,Y,Z in the ALEPH system
C!         IPOR /I      :portion #
C!         ITOW(1-3)/I  : nphi,ntheta,nstack
C! - Output :
C!         ITRI(1-2)/I  : nphi trigger, ntheta trigger
C!
C!  - Called by :HCCRTO
C!  - Calls     :HCCYL
C ---------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "hcpara.h"
#include "hctrig.h"
      DIMENSION COOR(*), ITOW(*), ITRI(*)
C-------------------------------------------------------
C
      CALL HCCYL(COOR,R,THETA,PHI)
C
C
      IF (IPOR.EQ.LPBAR) THEN
         PHIR    = TWOPI/NHCBPR
         ITRI(2) = NHCBTS(ITOW(2))
      ELSE
         PHIR    = TWOPI/NHCEPR
         ITRI(2) = NHCETS(ITOW(2))
      ENDIF
C
      ITRI(1) = INT (PHI/PHIR) + 1
C
      RETURN
      END
#endif
