        SUBROUTINE YPSIVN(PR,ELIP1,PSIN,XD0)
C-------------------------------------------------
C! Find the psi angle respect to the pr point
CKEY YV0 ANGLE /USER
C      AUTHOR  : M.A.CIOCCI 3/4/91
C
C      INPUT:
C           PR(3)/R   :Generally the mean value of primary vertex
C           ELIP1(5)/R: FIVE PARAMETERS OF THE TRACK AS IN FRFT
C
C      OUTPUT:
C           PSIN/R    :Angle psi for track at minum distance from pr
C                      on the space
C           xd0(3)/r  :coordinates of point at minimum distance from
C                      from pr
C
C
C
C                  DESCRIPTION
C                  ===========
C       Finds the psi angle of a charged track minimizing the distance
C       between helix and point pr.
C       Searchs the points coordinate at minimum distance from pr(3)
C       in the x-y plane (yd0nfn), calculates psi angle mod twopi
C       (yfixyn),  minimizing also the distance in z calculate
C       psi angle defining phase(yzphan)
C
C
C       CALLED: user
C
C       CALLS:  YD0NFN,YZPHAN,YFIXYN
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL YFIXYN
#include "bcs.h"
#include "frftjj.h"
C
        REAL PR(*),XD0(*),CF
        REAL ELIP1(*)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
#include "bmacro.h"
C
C+      ELIP1: THE FIVE PARAMETERS OF THE HELIX
C       FROM THE FRFT BANK
C
C
C+       HERE DEFINE THE SIX NEW PARAMETERS OF
C+       OF THE TWO HELICES
C
       CF=-ELIP1(1)/ABS(ELIP1(1))
       RHOF=ABS(1./ELIP1(1))
       D0F=-CF*ELIP1(4)
       XKF=-CF/(RHOF-D0F)
       XLAMF=ELIP1(2)
       FI0F=ELIP1(3)
       Z0F=ELIP1(5)
C
C+        SEARCHES THE COORDINATES OF POINT AT MINIMUM DISTANCE
C+        FROM PR(3) IN THE X-Y PLANE  (XD0(3))
C
         CALL YD0NFN(PR,RHOF,D0F,FI0F,CF,XD0,SIGND)
C
C
C+      CALCULATE PSI ANGLE AT XD0
C+
C
C
        PSIN=YFIXYN(CF,XD0(1),XD0(2),RHOF,FI0F,D0F)
C
C+   PSI MINIMIZING ALSO THE Z DISTANCE (PSI= PSI+N*TWOPI)
C
        CALL YZPHAN(PR,RHOF*XLAMF,Z0F,PSIN,XD0)
         RETURN
         END
#endif
