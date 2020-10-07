*DK ittofe
      SUBROUTINE ITTOFE
C.
C...ITTOFE  2.00  920213  19:21                          R.Beuselinck
C.
C!  Propagate induced wire signals to the preamps at either end of ITC.
C.
C.  Input:  IHIT, JDITWP banks.
C.  Output: JDITFN, JDITFP banks.
C.
C.  JDITFN contains: Hit.Id, Wire.Id, End (+/-), pulse start time,
C.                 number of pulses in ITFP,
C.                 zero address of 1st pulse in ITFP.
C.
C.  JDITFP contains: pulse height, pulse length.
C.
C.  This version produces fixed length pulses only of unique height.
C.  The JDITFP bank allows for an arbitrary pulse shape to be recorded
C.  as a continuous series of rectangular pulses.
C.
C.  Called by: ITDIGI, ISBEND                           from this .HLB
C.      Calls: WBANK, WDROP, BLIST, BKFRW               from BOS77
C.
C.  Named banks used:
C.  IHIT   - read only.
C.  JDITWP - read only.
C.  JDITFN - created and filled
C.  JDITFP - created and filled
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "jobcom.h"
#include "bcs.h"
#include "itelec.h"
#include "itwirc.h"
#include "itnamc.h"
      EXTERNAL INTCHA
#include "bmacro.h"
C ------------------------------------------------------------------
C
C--  Get induced signals and propagate them to the end of the chamber.
C--
      IF (JDITWP.EQ.0) GO TO 998
      LWWP = IW(JDITWP+1)
      LNWP = IW(JDITWP+2)
C
      KIHIT = IW(NAIHIT)
      IF (KIHIT.EQ.0) GO TO 998
      LWHT = LCOLS(KIHIT)
      LNHT = LROWS(KIHIT)
C--  Create banks JDITFN and JDITFP.
C--
      CALL WBANK (IW,JDITFN,LMHLEN+LITFN*LNWP*2,*998)
      IW(JDITFN+LMHCOL) = LITFN
      IW(JDITFN+LMHROW) = 0
      IW(JDITFN-3) = INTCHA ('ITFN')
      CALL WBANK (IW,JDITFP,LMHLEN+LITFP*LNWP*2,*998)
      IW(JDITFP+LMHCOL) = LITFP
      IW(JDITFP+LMHROW) = 0
      IW(JDITFP-3) = INTCHA ('ITFP')
C
      JWP  = JDITWP + LMHLEN
      JHT  = KIHIT + LMHLEN
      LWFN = LCOLS (JDITFN)
      LWFP = LCOLS (JDITFP)
      LNFN = 0
      LNFP = 0
C
C--  Loop over all induced signals in the ITWP bank.
C--
      DO 100 I=1,LNWP
        IDHT = IW(JWP+1)
        NW   = IW(JWP+2)
        TPLS = RW(JWP+3)
C
C--     Find hit position on wire and compute timing offset for
C--     signal propagation to the ends of the wire after smearing the
C--     true Z position by the resolution parameterization.
C--
        ZPLS = RTABL(KIHIT,IDHT,6)
        LAY  = ITABL(KIHIT,IDHT,2)
        DZ = ZRESIT(1,LAY) + ZRESIT(2,LAY)*ZPLS**2
        CALL RANNOR(ZERR,WASTE)
        ZERR = ZERR*DZ
        ZPLS = ZPLS + ZERR
        IF (ABS(ZPLS).GT.WZMXIT) ZPLS = SIGN (WZMXIT,ZPLS)
C
C--     Note that the global timing offset is computed according to
C--     a signal at Z=0.  Therefore the correction required is
C--     relative to this point.
C--     Compute measured Z according to S-bend relation. Adjust by
C--     the individual layer offset. Finally convert to a time-diff.
C--
        CALL ISBEND(ZPLS, ZMEAS)
        ZMEAS = ZMEAS - ZLOFIT(LAY)
        DT = 2.0*ZMEAS/CLGHT
C
C--     Assume that the deviation from linear is shared symmetrically
C--     between the signals at +/- Z.
C--
        DPOS = ZPLS/CLGHT - 0.5*DT
        DNEG = - DPOS
        ZPOS = - ZPLS
        ZNEG = + ZPLS
        TPOS = ZPOS/CLGHT + TPLS + DPOS
        TNEG = ZNEG/CLGHT + TPLS + DNEG
        JFN = KNEXT (JDITFN)
        JFP = KNEXT (JDITFP)
C--     zero offset for start of pulse data.
        IPADR = JFP - JDITFP
C
C--     Fill (+/-) front end signal data.
C--
        IW(JFN+1) = IDHT
        IW(JFN+2) = NW
        IW(JFN+3) = +1
        RW(JFN+4) = TPOS
C--     number of pulses stored at front end.
        IW(JFN+5) = 1
        IW(JFN+6) = IPADR
        RW(JFP+1) = 1.
        RW(JFP+2) = 1.
        CALL UCOPY(IW(JFN+1),IW(JFN+LWFN+1),LWFN)
        IW(JFN+LWFN+3) = -1
        RW(JFN+LWFN+4) = TNEG
        IW(JFN+LWFN+6) = IPADR + IW(JFN+5)*LWFP
        IPADR = IW(JFN+LWFN+6)
        RW(JDITFP+IPADR+1) = 1.
        RW(JDITFP+IPADR+2) = 1.
        LNFN = LNFN + 2
        LNFP = LNFP + IW(JFN+5) + IW(JFN+LWFN+5)
        IW(JDITFN+2) = LNFN
        IW(JDITFP+2) = LNFP
        JWP = JWP + LWWP
  100 CONTINUE
C
  999 RETURN
C - not enough space for JDITFN or JDITFP banks
  998 CONTINUE
      IW(1) = LITWBK
      CALL WDROP (IW,JDITWB)
      CALL ALTELL ('ITTOFE: not enough space for ITFx banks ',1,'NEXT')
      END
#endif
