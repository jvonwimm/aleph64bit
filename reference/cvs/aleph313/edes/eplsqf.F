      REAL FUNCTION EPLSQF(PLAN,NPLANS,LISPL,POINT)
C.----------------------------------------------------------------------
CKEY ECALDES QUASI LINEAR SEARCH / INTERNAL
C     H.Videau      Creation 26/10/87   Modification 03/02/88
C! Quasi linear search on planes,REAL result.
C  Makes a quasi linear search through the planes of a pencil to find
C  the ones surrounding the point,then interpolates.
C  A n'utiliser que pour une suite monotone!
C  To be used only for monotonic series!
C  This is the fastest search of the three but the more restrictive.
C   Calls: none
C   Called internally
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input variables
      INTEGER NPLANS
      REAL PLAN(ECALLS,ECALLP),LISPL(ECALLP,NPLANS),POINT(ECALLS-1)
C  Locales
      INTEGER IC,ICOLD,ICNEW
      REAL CL1,CL2,VOLD,VNEW
C
C Calcul des coordonnees locales : CLi = PLANi.POINT (i=1,2)
      CL1 = PLAN(1,1)*POINT(1) + PLAN(2,1)*POINT(2) +
     +      PLAN(3,1)*POINT(3) + PLAN(4,1)
      CL2 = PLAN(1,2)*POINT(1) + PLAN(2,2)*POINT(2) +
     +      PLAN(3,2)*POINT(3) + PLAN(4,2)
C--- recherche dans une suite quasi-lineaire
      IC    = NPLANS
      ICOLD = 1
      ICNEW = NPLANS
      VOLD  = CL1*LISPL(2,ICOLD) - CL2*LISPL(1,ICOLD)
      IF(VOLD.GT.0.) GO TO 5
      VNEW  = CL1*LISPL(2,ICNEW) - CL2*LISPL(1,ICNEW)
      IF(VNEW.LT.0.) GO TO 6
 3    IC = INT((FLOAT(ICOLD)*VNEW-FLOAT(ICNEW)*VOLD)/(VNEW-VOLD))
      IF(IC.LT.1)      IC=1
      IF(IC.GT.NPLANS) IC=NPLANS
      IF(IC.EQ.ICNEW) GO TO 2
      ICOLD = ICNEW
      VOLD  = VNEW
      ICNEW = IC
      VNEW  = CL1*LISPL(2,ICNEW) - CL2*LISPL(1,ICNEW)
      GO TO 3
 2    IF(IC.NE.NPLANS) THEN
        IF(IABS(ICOLD-ICNEW).GT.1) THEN
          ICOLD = IC + 1
          VOLD  = CL1*LISPL(2,ICOLD) - CL2*LISPL(1,ICOLD)
          GO TO 3
                                   ENDIF
                        ENDIF
      VOLD= CL1*LISPL(2,IC+1) - CL2*LISPL(1,IC+1)
      EPLSQF = FLOAT(IC) -VNEW/(-VNEW+VOLD)
      GO TO 999
 5    CONTINUE
      VNEW  = CL1*LISPL(2,ICOLD+1) - CL2*LISPL(1,ICOLD+1)
      EPLSQF=1. -VOLD/(-VOLD+VNEW)
      GO TO 999
 6    CONTINUE
      VOLD  = CL1*LISPL(2,ICNEW-1) - CL2*LISPL(1,ICNEW-1)
      EPLSQF=FLOAT(NPLANS-1)-VOLD/(-VOLD+VNEW)
C
 999  CONTINUE
      END
#endif
