      SUBROUTINE HCBOUN
C.** **************************************************************
C
CKEY HCALDES HCAL TOWER THETA / INTERNAL
C! Compute theta (proj) boundaries of Hcal towers
C
C. G. CATANESI 2/12/1985      modified by - F.Ranjard - 880216
C.
C.                                                                *
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
      ARSINH(X)=ALOG(X+SQRT(X*X+1.))
C
C ------------------------------------------------------------
C.
C.  THETA SUBDIVISION IN HADRONIC CALIRIMETER
C.
      ZMAX= HCRSIZ * TAN(PIBY2-HCTHRF) +.1
      RMAX= HCZSIZ * TAN(HCTHRF) +.1
C.
C.    PROJECTED SIZE OF PADS (COSTANTS)
C.
      EPS1= HCRSIZ / NHCBAR * ARSINH(ZMAX / HCRSIZ)
      EPS2= HCZSIZ / NHCECA * ARSINH(RMAX / HCZSIZ)
C.
C.  LOOP TO CALCULATE THETA VALUES IN BARREL
C.
      JJ=0
      NHC = LHCTR/2 + 1
      HCTHUL(NHC)= PIBY2
   10 CONTINUE
      JJ=JJ+1
      ZNEW = HCRSIZ * SINH( JJ * EPS1 /HCRSIZ)
      IF (ZNEW.LE.ZMAX) THEN
         HCTHUL(NHC-JJ) = PIBY2 - ATAN2( ZNEW , HCRSIZ )
                                                                 GOTO 10
      ENDIF
C.
C.  LOOP TO CALCULATE THETA VALUES IN END-CAPS
C.
      KK=1
   20 CONTINUE
      KK=KK+1
      RNEW = HCZSIZ * SINH( KK * EPS2 / HCZSIZ )
      IF (RNEW.LE.RMAX) THEN
         HCTHUL(KK-1) = ATAN2( RNEW , HCZSIZ )
                                                                 GOTO 20
      ENDIF
C
C   GET THETA VALUES .GT. 90.deg
C
      DO 30 N=1,NHC-1
         HCTHUL(NHC*2-N) = PI - HCTHUL(N)
   30 CONTINUE
C
      RETURN
      END
#endif
