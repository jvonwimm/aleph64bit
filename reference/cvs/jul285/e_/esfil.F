      SUBROUTINE ESFIL  (IERR,EFLOW)
C=======================================================================
C!   Calculate Energy from ECAL wire
C-
C-   Authors : M.N. Minard , M. Pepe       02/10/89
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      EXTERNAL NAMIND
#include "bmacro.h"
C
C-    Calculate E-From Ecal wires
C
      IERR = 0
      EFLOW = 0
      NAEWDI = NAMIND('EWDI')
      NEWDI = 0
      KEWDI = IW (NAEWDI)
      IF ( KEWDI.NE.0) NEWDI = LROWS ( KEWDI)
      IF(NEWDI.GT.36.OR.NEWDI.LE.0)GOTO 999
      IF ( KEWDI.EQ.0 ) IERR = 1
C
C-     Initialise array
C
      DO 10 IEWDI = 1,NEWDI
      IMOD = ITABL ( KEWDI,IEWDI,1)
      ISC = (IMOD-1)/12 +1
      ILN = MOD (IMOD-1,12) +1
      DO 20 IPL = 1,45
      EFLOW = EFLOW+FLOAT(ITABL(KEWDI,IEWDI,1+IPL))
     &/1000000.
 20   CONTINUE
 10   CONTINUE
 999  CONTINUE
      RETURN
      END
#endif
