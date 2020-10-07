      SUBROUTINE OMBOUT (XIPB, IBUNB, BCURB, GRATB, IER)
C----------------------------------------------------------------------
C!  - Output results of BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C!   Modified by : J. Nash 8-May-91 Changed IW to RW in appropriate plac
C!   Input:  XIPB  = Array containing the positions and angles at the IP
C!                   (x, x', y, y') for each beam
C!           IBUNB = Bunch id for each beam
C!           BCURB = Beam current at each BOM for each beam
C!           GRATB = Gain ratio of each pair of electrodes for each beam
C!           IER   = 0  OK
C!                 < 0  Fatal error in BOM analysis
C!                 > 0  Non-fatal data quality error flag
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bomflg.h"
#include "bombjj.h"
#include "bomejj.h"
C
      REAL XIPB(4,*), BCURB(2,*), GRATB(4,*)
      INTEGER IER, NCOL, NROW, KBOMB, KBOME, JBOME, IROW, IERFL(2),
     &        IGARB, IBUNB(*)
C
      DATA NCOL, NROW /12, 2/
#include "bmacro.h"
C.......................................................................
C
      CALL AUBOS ('BOMB', 0, 3+LMHLEN, KBOMB, IGARB)
C
      IF (KBOMB .EQ. 0) GOTO 999
C
      IW (KBOMB + LMHCOL) = 3
      IW (KBOMB + LMHROW) = 1
      RW (KBOMB + LMHLEN + JBOMXX) = (XIPB(1,1) + XIPB(1,2)) / 2.
      RW (KBOMB + LMHLEN + JBOMYY) = (XIPB(3,1) + XIPB(3,2)) / 2.
      IW (KBOMB + LMHLEN + JBOMEE) = IER
C
      CALL AUBOS ('BOME', 0, NCOL*NROW+LMHLEN, KBOME, IGARB)
C
      IF (KBOME .EQ. 0) GOTO 999
C
      IW (KBOME + LMHCOL) = NCOL
      IW (KBOME + LMHROW) = NROW
C
      IERFL(1) = IER
      IERFL(2) = IMCURR
C
      DO 100 IROW = 1, NROW
        JBOME = KBOME + LMHLEN + (IROW-1)*NCOL
C
        RW (JBOME + JBOMXI) =  XIPB(1,IROW)
        RW (JBOME + JBOMXP) =  XIPB(2,IROW)
        RW (JBOME + JBOMYI) =  XIPB(3,IROW)
        RW (JBOME + JBOMYP) =  XIPB(4,IROW)
        IW (JBOME + JBOMBU) = IBUNB(IROW)
        RW (JBOME + JBOMIA) = BCURB(1,IROW)
        RW (JBOME + JBOMIB) = BCURB(2,IROW)
        RW (JBOME + JBOMR1) = GRATB(1,IROW)
        RW (JBOME + JBOMR2) = GRATB(2,IROW)
        RW (JBOME + JBOMR3) = GRATB(3,IROW)
        RW (JBOME + JBOMR4) = GRATB(4,IROW)
        IW (JBOME + JBOMER) = IERFL(IROW)
C
  100 CONTINUE
C
  999 RETURN
      END
#endif
