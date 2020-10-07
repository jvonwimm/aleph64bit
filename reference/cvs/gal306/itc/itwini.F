*DK itwini
      SUBROUTINE ITWINI
C.
C...ITWINI  2.30  920213  19:31                         R.Beuselinck
C.
C!  Initialise layout of ITC and time expansion constants for readout.
C.
C.  Called by: ITIRUN                                    from this .HLB
C.      Calls: ISBGIN                                    from this .HLB
C.             BLIST, NAMIND                             from BOS77
C.             ALTELL                                    from ALEPHLIB
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "itelec.h"
#include "itexpc.h"
#include "itrotc.h"
#include "itwirc.h"
      EXTERNAL NAMIND
#include "ijjpar.h"
#include "bmacro.h"
C
C--  Get ITC readout geometry and other constants from the databse.
C--
      IND = IW(NAMIND('ITCC')) + LMHLEN
      NLAY   = IW(IND+JITCNL)
      WZMXIT = RW(IND+JITCWL)
      WSAGIT = RW(IND+JITCWS)
      ITSHFT = WSAGIT.NE.0.
C
C--  Fill the sense wire layout data.
C--
      IND = IW(NAMIND('ILYR'))
      NR  = LROWS(IND)
      IF (NR.NE.NLAY)
     +  CALL ALTELL('ITWINI: Wrong number of layers in ILYR',0,'STOP')
      DO 30 I=1,NR
        JROW = KROW(IND,I)
        LAY = IW(JROW+JILYLN)
        NWIRIT(LAY) = IW(JROW+JILYNW)
        RWIRIT(LAY) = RW(JROW+JILYRW)
        PHWRIT(LAY) = PI*IW(JROW+JILYPO)/NWIRIT(LAY)
        CELHIT(LAY) = RW(JROW+JILYCH)
        CELWIT(LAY) = TWOPI*RWIRIT(LAY)/NWIRIT(LAY)
   30 CONTINUE
      NOFF = 0
      DO 35 I=1,NR
        IWIRIT(I) = NOFF
        NOFF = NOFF + NWIRIT(I)
   35 CONTINUE
C
C--  Fill the Z-scalar data with fixed numbers until the obsolete
C--  IZSC bank is replaced.  These numbers are only used by the
C--  SPP trigger (which doesn't exist yet).
C--
      PLEN   = 55.
      CLOKIT = 20.
      TSTRIT = 732.
      TBINIT = 6.
      DO 40 I=1,NLAY
        PLENIT(I) = 0.5*PLEN * RWIRIT(NLAY)/RWIRIT(I)
   40 CONTINUE
C
C--  Fill /ITELEC/ with the R-phi & Z TDC parameters.
C--
      IND = IW(NAMIND('IRFE')) + LMHLEN
      TDCRIT = RW(IND+JIRFBW)
      TOFFIT = TDCRIT*IW(IND+JIRFZB)
      DO 50 I=1,NLAY
        ITTDCL(I) = IW(IND+JIRFCL+I-1)
        ITTDCH(I) = IW(IND+JIRFCH+I-1)
   50 CONTINUE
      IND = IW(NAMIND('IZFE')) + LMHLEN
      TDCZIT = RW(IND+JIZFBW)
      ITZTZB = IW(IND+JIZFZB)
      ZEXPIT = RW(IND+JIZFEX)
      IND2 = IW(NAMIND('IZNL')) + LMHLEN
      IF (IW(IND+JIZFOO).EQ.0) THEN
        ZPARIT(1) = RW(IND2+JIZNS1)
        ZPARIT(2) = RW(IND2+JIZNS2)
        ZPARIT(3) = RW(IND2+JIZNS3)
      ELSE
        ZPARIT(1) = 0.
        ZPARIT(2) = 0.
        ZPARIT(3) = 1000.
      ENDIF
      DO 60 I=1,NLAY
        ZLOFIT(I) = RW(IND2+JIZNLO+I-1)
        ITZTDL(I) = IW(IND+JIZFCL+I-1)
        ITZTDH(I) = IW(IND+JIZFCH+I-1)
        EXPFIT(I) = ZEXPIT * RWIRIT(NLAY)/RWIRIT(I)
   60 CONTINUE
C
C--  Get the Z resolution parameters for each layer.
C--
      IND = IW(NAMIND('IZRS'))
      DO 70 I=1,LROWS(IND)
        JROW = KROW(IND,I)
        LAY           = IW(JROW+JIZRLN)
        ZRESIT(1,LAY) = RW(JROW+JIZRCO)
        ZRESIT(2,LAY) = RW(JROW+JIZRCO+1)
   70 CONTINUE
C
C--  Initialise the S-bend interpolation.
C--
      CALL ISBGIN(WZMXIT, ZPARIT(1), ZPARIT(2), ZPARIT(3))
      END
#endif
