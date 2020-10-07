      SUBROUTINE FKINK(ITRCK,PROPA,IFAIL)
C-----***********************************------------------------------
C!    Find kink in an ITC-TPC track
C!
C!    AUTHOR        :- GEORG STIMPFL 88/8/08
C!
C!    =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
C!
C!    INPUT  : ITRCK = track number in FRFT bank
C!
C!    OUTPUT : PROPA = Probability that the parameters of the two
C!                     track halves are the same
C!                     (i.e. probability to have no kink)
C!             IFAIL = error flag
C!                     0 ... everything okay
C!                     1 ... missing input banks
C!                     2 ... number of coordinates outside limits
C!
C!    Requires at least 10 coordinates (ITC & TPC)
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "frtljj.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C--------------------------------------------------------------------
      KKFRTL(II) = KFRTL+2+(II-1)*(IW(KFRTL+1))
C--
      IF (FIRST) THEN
        NAFRTL = NAMIND('FRTL')
        NAFICL = NAMIND('FICL')
        NAFTCL = NAMIND('FTCL')
        FIRST = .FALSE.
      ENDIF
C--
      IFAIL = 0
      KFRTL = IW(NAFRTL)
      KFICL = IW(NAFICL)
      KFTCL = IW(NAFTCL)
      IF (KFRTL.LE.0) THEN
        IFAIL = 1
        RETURN
      ENDIF
      IF (KFICL.LE.0 .AND. KFTCL.LE.0) THEN
        IFAIL = 1
        RETURN
      ENDIF
C--
C--   Get coordinate lists
      IOFFI = ITABL(KFRTL,ITRCK,JFRTII)
      NPTIT = ITABL(KFRTL,ITRCK,JFRTNI)
      IOFFT = ITABL(KFRTL,ITRCK,JFRTIT)
      NPTTP = ITABL(KFRTL,ITRCK,JFRTNT)
C--
      IF (KFICL.GT.0) THEN
        IOFFI = KFICL+3+IOFFI
      ELSE
        NPTIT = 0
      ENDIF
      IF (KFTCL.GT.0) THEN
        IOFFT = KFTCL+3+IOFFT
      ELSE
        NPTTP = 0
      ENDIF
      CALL FKKPAR(NPTIT,NPTTP,IW(IOFFI),IW(IOFFT),PROCH,PROPA,IFAIL)
      END
#endif
