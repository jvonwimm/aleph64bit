          REAL FUNCTION HCSAMP(IPOR)
C------------------------------------------------
CKEY HCALDES HCAL TUBE DIMENSION / USER
C
C!  Return the average tube dimension in the subdetector IPOR
C!
C!                           Author:G.Catanesi 071088
C!
C!     Input:
C!             IPOR/I subdetector number
C!
C!-------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
      IF (IPOR.EQ.2)THEN
         HCSAMP = HCSABL
      ELSE
         HCSAMP = HCSAEC
      ENDIF
C
         RETURN
         END
#endif
