      SUBROUTINE SICLCO(RACLU,PHCLU,ZCLU,IRD,IPH,IST)
C----------------------------------------------------------------
C!  - Cluster position correction in Sical
C!
C!   Author   :         John Rander-B.Bloch               27-JAN-1992
C!
C!   Inputs:  SiCAL shower cluster radial and phi positions
C!            RACLU = Cluster radius as from barycenter method
C!            PHCLU = Cluster Phi postion as from barycenter method
C!            ZCLU  = Cluster z   postion as from barycenter method
C!
C!   Outputs: Returns positions corrected for pad curvature and
C!            barycenter algorithm,RACLU and PHCLU.
C!            IRD,IPH,IST indices of correponding R , Phi , z bins
C!
C?   Description: Position relative to pad's lowest edge is used
C?   ===========  to calculate corrections using periodic function
C?                with phase shift. The abberation due to pad curvature
C?                is corrected in R using 1/R**2 variation of the effect
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#ifndef DOC
#include "bcs.h"
#include "scorjj.h"
      DIMENSION PHCL(3)
      DATA IFIR / 0/
C ==== DIMENSIONS IN CM =====
C  All correction factors now taken from SCOR bank on Data Base
C radial correction constants
      DATA CORR1,CORR2,CORR3/1.009,0.0300,0.325/
C phi correction constants
      DATA CORPH1,CORPH2,CORPH3,CORPH4/0.04384,0.04496,0.04494,0.500/
C
#include "bmacro.h"
C -------------------------------------------------------------------
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        NASCOR = NAMIND ('SCOR')
        KSCOR = IW(NASCOR)
        IF (KSCOR.GT.0) THEN
          CORR1 = RTABL(KSCOR,1,JSCORC)
          CORR2 = RTABL(KSCOR,1,JSCORC+1)
          CORR3 = RTABL(KSCOR,1,JSCORC+2)
          CORPH1= RTABL(KSCOR,1,JSCOPC)
          CORPH2= RTABL(KSCOR,1,JSCOPC+1)
          CORPH3= RTABL(KSCOR,1,JSCOPC+2)
          CORPH4= RTABL(KSCOR,1,JSCOPC+3)
        ENDIF
      ENDIF
C step 1: convert positions to internal pad units (three different phi s
      CALL SIXTOI(RACLU,PHCLU,ZCLU,IRD,IPH,IST,RACL,PHCL,IOK)
C
C step 2: correct radial offset
C   - pad curvature
      RCORR =CORR1/(RACLU*RACLU)
C   - barycenter algorithm
      RBARY=CORR2*SIN(TWOPI*RACL+CORR3*SIN(TWOPI*RACL))
C==> yields
      RACLU=RACLU-RCORR-RBARY
C
C step 3: correct phi offset
      PBARY=CORPH1*SIN(TWOPI*PHCL(1)+CORPH4*SIN(TWOPI*PHCL(1)))
     &     +CORPH2*SIN(TWOPI*PHCL(2)+CORPH4*SIN(TWOPI*PHCL(2)))
     &     +CORPH3*SIN(TWOPI*PHCL(3)+CORPH4*SIN(TWOPI*PHCL(3)))
C==> yields
      PHCLU=PHCLU-PBARY
      IF(PHCLU.LT.0.0) PHCLU=TWOPI+PHCLU
      IF(PHCLU.GT.TWOPI) PHCLU=PHCLU-TWOPI
  999 RETURN
      END
#endif
#endif
