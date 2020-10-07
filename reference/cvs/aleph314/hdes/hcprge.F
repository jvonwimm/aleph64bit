        SUBROUTINE HCPRGE
C--------------------------------------------------------------
CKEY HCALDES HCAL GEOM PRINT / USER
C! Print Hcal geometrical and readout constants
C!
C! G.Capon 870404
C!
C--------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
#include "alcons.h"
#include "bcs.h"
C               print Geometrical constants
C
        LOUT = IW(6)
        WRITE(LOUT,800)
C
        WRITE(LOUT,802)HCRMIN(1),YBAST1,YBARMX,HCZMAX(1)
        WRITE(LOUT,803)HCRMIN(2),HCRMAX(2),HCRMAX(3)
        WRITE(LOUT,805)HCZMIN(2),HCZMIN(3),ZENST1,ZENDMX
        WRITE(LOUT,804)HCIRTH,HCTUTH,NHCBFS,NHCEFS
        WRITE(LOUT,806)NHCTRE,HCSABL,HCSAEC,XBARR0
        WRITE(LOUT,801)XENDC0,HCAPSL
        DO 10 N=1,LHCNL
   10   WRITE(LOUT,808)N,NEITHC(N),(NEITSP(N,K),K=1,2),(HCSPLT(N,K) ,K
     +  =1,2)
C
C?              print theta boundaries
C
        DO 20 N=1,63
        TET=RADEG*HCTHUL(N)
        WRITE (LOUT,807) N,TET
   20   CONTINUE
C
C
C
         RETURN
  800   FORMAT(1X,10('----------'),/, 20X,'HCAL  GEOMETRY  DATA',/, 1X,
     +  10('----------'))

  801   FORMAT(' XENDC0,HCAPSL',2F8.3)
  802   FORMAT(/,' HCRMIN(1),YBAST1,YBARMX,HCZMAX(1)',4F10.1)
  803   FORMAT(' HCRMIN(2),HCRMAX(2),HCRMAX(3)       ',4F10.1)
  804   FORMAT(' HCIRTH,HCTUTH,NHCBFS,NHCEFS',2F8.1,2I8)
  805   FORMAT(' HCZMIN(2),HCZMIN(3),ZENST1,ZENDMX',4F10.1)
  806   FORMAT (' NHCTRE,HCSABL,HCSAEC,XBARR0=',I5,3F8.3,/)
  807    FORMAT (' N, THETA(PROJ) DEG',I4,F6.1)
  808   FORMAT(' LAYER #,NEITHC,NEITSP(2),HCSPLT(2)',4I5,2F8.2)
         END
#endif
