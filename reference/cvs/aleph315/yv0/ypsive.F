        SUBROUTINE YPSIVE(PR,KAPOI,PSIN)
C-------------------------------------------------
C! Find the psi angle respect to the pr point
CKEY YV0 ANGLE /USER
C      AUTHOR  : M.A.CIOCCI 21/1/90
C
C      INPUT:
C           PR(3)/R   :Generally the mean value of primary vertex
C           KAPOI/I   :Number of track in the frft bank.
C
C      OUTPUT:
C           PSIN/R    :Angle psi for track at minum distance from pr
C                      on the X-Y plane.
C
C
C
C                  DESCRIPTION
C                  ===========
C       Finds the psi angle of a charged track minimizing the distance
C       between helix and point pr.
C       Searchs the points coordinate at minimum distance from pr(3)
C       in the x-y plane (yd0nfi), calculates psi angle mod twopi
C       (yfixyp),  minimizing also the distance in z calculate
C       psi angle defining phase(yzpha1)
C
C
C       CALLED: user
C
C       CALLS:  YD0NFI,YZPHA1,YFIXYP
C       BANKS: FRFT
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL YFIXYP
#include "bcs.h"
#include "frftjj.h"
C
        REAL PR(3),XD0(2)
        REAL ELIP1(5)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
      INTEGER NFRFT
      DATA NFRFT / 0 /
#include "bmacro.h"
C
C+      ELIP1,ELIP2: THE FIVE PARAMETERS OF THE TWO HELICES
C+      AS READ  FROM THE FRFT BANK
C
      IF ( NFRFT .EQ. 0 ) THEN
        NFRFT = NAMIND( 'FRFT' )
      ENDIF
      KFRFT = IW( NFRFT )
           DO 10 IPAR=1,5
           ELIP1(IPAR)=RTABL(KFRFT,KAPOI,JFRFIR-1+IPAR)
  10   CONTINUE
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
C
C+        SEARCHES THE COORDINATES OF POINT AT MINIMUM DISTANCE
C+        FROM PR(3) IN THE X-Y PLANE  (XD0(2))
C
         CALL YD0NFI(PR,RHOF,D0F,FI0F,CF,XD0)
C
C
C+      CALCULATE PSI ANGLE AT XD0
C+
C
C
        PSIN=YFIXYP(CF,XD0(1),XD0(2),RHOF,FI0F,D0F)
C
C+   RECALCULATE PSI MINIMIZING ALSO THE Z DISTANCE (PSI= PSI+N*TWOPI)
C
        CALL YZPHA1(PR,RHOF*XLAMF,Z0F,PSIN)
         RETURN
         END
#endif
