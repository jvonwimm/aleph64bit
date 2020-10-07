      SUBROUTINE HGETGA(ARRAY)
C---------------------------------------------------------------
C
CKEY HCALDES HCAL GEOM DBASE GALEPH / USER
C!Trasfers same geometrical quantities from DB to Galeph
C!
C!            Author : G.Catanesi  88/04/18
C!
C!     INPUT :  Hcgeom common
C!
C!     OUTPUT: ARRAY/R  geometrical quantities to be trasferred
C!
C---------------------------------------------------------------
C
#ifndef DOC
#include "hcgeom.h"
C
      DIMENSION ARRAY(*)
C
C  -------------------------------------------------------
C
C
C
      ARRAY(1) = HCSMTH
      ARRAY(2) = HCIRTH
      ARRAY(3) = HCLSLA
      ARRAY(4) = HCTUTH
      ARRAY(5) = FLOAT(NHCINL)
      ARRAY(6) = FLOAT(NHCOUL)
      ARRAY(7) = FLOAT(NHCTRE)
      ARRAY(8) = HCPHOF
C
      INDEX = 8
C
      DO 10 J=1,LHCNL
         ARRAY(INDEX +J)= FLOAT(NHCTU1(J))
         ARRAY(INDEX+LHCNL +J)= HCLARA(J)
         ARRAY(INDEX+2*LHCNL +J)= HCLAWI(J)
   10 CONTINUE
C
      INDEX = INDEX + 3*LHCNL
C
      DO 20 J=1,LHCTR
         ARRAY(INDEX +J) = FLOAT(IHCREG(J))
   20 CONTINUE
C
      INDEX = INDEX + LHCTR
C
      DO 30 J=1,LPHC
         ARRAY(INDEX+J) = HCZMIN(J)
         ARRAY(INDEX+LPHC+J)= HCZMAX(J)
         ARRAY(INDEX+2*LPHC+J)= HCRMIN(J)
         ARRAY(INDEX+3*LPHC+J)= HCRMAX(J)
         ARRAY(INDEX+4*LPHC+J)= HCWINO(J)
         ARRAY(INDEX+5*LPHC+J)= HCLTNO(J)
         ARRAY(INDEX+6*LPHC+J)= HCTIRF(J)
         ARRAY(INDEX+7*LPHC+J)= FLOAT(NHCPLA(J))
   30 CONTINUE
C
      RETURN
      END
#endif
