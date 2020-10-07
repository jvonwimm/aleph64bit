       FUNCTION YFIXYN(CHARG,X,Y,RO,FI,D0)
C------------------------------------------------------------
C!Finds the psi angle at the point (x,y) def mod twopi
CKEY YV0 /USER
C
C   AUTHOR :M.A. CIOCCI,L.ROLANDI  7-4-88
C   MODIFIED:
C
C
C       INPUT:
C              CHARG/REAL    TRACK CHARGE
C              X,Y/REAL      POINT COORDINATES ON HELIX
C              RO/REAL       RADIUS OF CURVATURE NOT SIGNED
C              FI/REAL       FI0 (SEE FRFT BANK)
C              D0/REAL       D0  (D0=-CHARGE*D0BANK FRFT)
C
C    CALLED BY User
C
C
C--------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
       YFIXYN=ATAN2(-(CHARG*X-(RO-D0)*SIN(FI)),
     $ CHARG*Y+(RO-D0)*COS(FI))
       YFIXYN=YFIXYN-FI
      IF (YFIXYN.LT.-PI) THEN
        YFIXYN=YFIXYN+TWOPI
      ELSEIF (YFIXYN.GT.PI) THEN
        YFIXYN=YFIXYN-TWOPI
      ENDIF
      YFIXYN=-CHARG*YFIXYN
       RETURN
       END
#endif
