      SUBROUTINE E4PAL(ICLE,ITRCE,NTR,IERR)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR :  CALCULATE ENERGY CORRECTION IN OVERLAP
C!
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C!      GIVE 3X4 CENTERED STOREYS OF CLUSTER ICLE ,OF TRACK ITRTE
C!      REFERED AS NUMBER ITRCE IN CLUSTER ASSOCIATION
C!     IERR=0 OK
C!     IER=-1 AUHCYL CANT DO EXTRAPOLATION
C!     IER=-2 E4XYZ0 CANT DO EXTRAPOLATION
C!     IER=-3 E4XYZ0 CANT DO EXTRAPOLATION
C!     IER=1  SELECTION OF STOREYS FAILED IN THE OVERLAP
C!     IER=2  SELECTION OF STOREYS WITH CLUSTER BARYCENTER
C!     IER=3  NO STOREYS SELECTED
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "e4com0.h"
#include "e4com1.h"
#include "e4com2.h"
#include "etpinf.h"
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
#include "eiflag.h"
#include "bmacro.h"
C
      JTRAK=NTR
      ICLU=ICLE
      ITRCL=ITRCE
      NBE4ST(ITRCL)=0
      E4CLU(ITRCL)=0
CRAJOUT 20/6/90
      CALL VZERO(XTOW,NETRA*NTOWMX*3)
CRAJOUT 04/10/90
      TETIFL=0.
      PHIIFL=0.
      IERR=0
      DO 1 IJ=1,3
 1    IJDEAD(IJ)=0
C
C!!         TRACK < 1 GEV WILL GO IN E4BARY
C
      IF(PTTP.LT.1)THEN
C       TAKE STOREYS CENTERED ON THE BARYCENTER OF THE CLUSTER IF ONE
      KECT1=IW(NAECT1)
      KERL3=IW(NAERL3)
      NN1=ITABL(KECT1,ICLU,JECTER)
      NN2=ITABL(KERL3,NN1,JERLER)
        IERR=2
        IF(NN2.EQ.0)THEN
           CALL E4BARY
        ELSE
C        MORE THAN 1 TRACK IN THIS CLUSTER NO E ID.
         IERR=3
         RETURN
        ENDIF
      ELSE
C     SEARCH FOR STOREYS CENTERED ON THE TRACK EXTRAPOLATION
        CALL E4FIND(IERR)
        IF(IERR.NE.0)THEN
          TETIFL=0.
          PHIIFL=0.
      KECT1=IW(NAECT1)
      KERL3=IW(NAERL3)
      NN1=ITABL(KECT1,ICLU,JECTER)
      NN2=ITABL(KERL3,NN1,JERLER)
          IERR=2
          E4CLU (ITRCE) = 0
          IF(NN2.EQ.0)THEN
             CALL E4BARY
          ELSE
C            MORE THAN 1 TRACK IN THIS CLUSTER NO E ID.
             IERR=3
             RETURN
          ENDIF
        ENDIF
C
      ENDIF
C
      CALL E4FIDE
      RETURN
      END
#endif
