      SUBROUTINE HGETJU(ARRAY)
C-----------------------------------------------------------------
CKEY HCALDES HCAL GEOM DBASE JULIA / USER
C
C! transfer global Hcal geom data to Julia
C!
C! G.Capon             jan 1988
C!
C-------------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
           DIMENSION ARRAY(*)
C
      ARRAY(1)=HCTIRF(2)
      ARRAY(2)=HCIRTH
      ARRAY(3)=HCTUTH
      ARRAY(4)=HCRMIN(1)
      ARRAY(5)=HCRMAX(1)
      ARRAY(6)=HCRMIN(2)
      ARRAY(7)=HCRMAX(3)
      ARRAY(8)=HCRMAX(2)
      ARRAY(9)=HCZMIN(2)
      ARRAY(10)=HCZMIN(1)
C
      RETURN
      END
C
#endif
