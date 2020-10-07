      SUBROUTINE FLOWEH (EFLOW)
C=======================================================================
C!   Calculate Energy of event when E-Flow is not performed
C-
C-   Authors : M.N. Minard , M. Pepe       15/09/89
C-
C
C?
C!======================================================================
#ifndef DOC
#include "hclujj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DIMENSION ECAL(2,3)
      DATA FEE1,FEE2,FEB1,FEB2,FHAD/1.34,1.82,1.26,1.65,0.95/
#include "bmacro.h"
C
C-    Calculate E-From Ecal wires
C
      NAEWDI = NAMIND('EWDI')
      NEWDI = 0
      KEWDI = IW (NAEWDI)
      IF ( KEWDI.NE.0) NEWDI = LROWS ( KEWDI)
      IF(NEWDI.GT.36.OR.NEWDI.LE.0)GOTO 19
C
C-     Initialise array
C
      CALL VZERO ( ECAL,6)
      DO 10 IEWDI = 1,NEWDI
      IMOD = ITABL ( KEWDI,IEWDI,1)
      ISC = (IMOD-1)/12 +1
      ILN = MOD (IMOD-1,12) +1
      DO 20 IPL = 1,45
      IF ( ITABL(KEWDI,IEWDI,1+IPL).LE.0) GO TO 20
      IS = 1
      IF (IPL.GE.34 ) IS = 2
      ECAL (IS,ISC) = ECAL(IS,ISC)+FLOAT(ITABL(KEWDI,IEWDI,1+IPL))
     &/1000000.
 20   CONTINUE
 10   CONTINUE
 19   CONTINUE
C
C-    Calculate Hcal Energy
C
      NAHCLU = NAMIND('HCLU')
      KHCLU = IW (NAHCLU)
      NHCLU = 0
      IF (KHCLU.GT.0) NHCLU = LROWS(KHCLU)
      EPHCO = 0.
      DO 30 IHCLU = 1,NHCLU
      EPHCO = EPHCO + RTABL ( KHCLU,IHCLU ,JHCLEC)
 30   CONTINUE
C
C-    Now calculate sum from differents contributions
C
      EFLOW = (ECAL(1,1)+ECAL(1,3))*FEE1+ECAL(1,2)*FEB1
     +      + (ECAL(2,1)+ECAL(2,3))*FEE2+ECAL(2,2)*FEB2
     +      + EPHCO*FHAD
      RETURN
      END
#endif
