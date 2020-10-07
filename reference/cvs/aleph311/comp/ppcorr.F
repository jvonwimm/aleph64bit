      FUNCTION PPCORR(IJUL,NVDET,ERCORR,IFAIL)
C-----------------------------------------------------------------------
C! Correct particle momenta for effects of residual distortions in
C! the central tracking detector.
C!   This is the so called "sagitta correction".
C! It depends upon cos(theta) and the year of data taking.
C!
C!    Author   :  I.R. Tomalin
C!                Based on subroutine QFBDEV written by I. ten Have.
C!    Date     :  10-3-1994
C!    Modified :  I.Tomalin 30/1/97.
C!                Add LEP2 corrections and prepare move to ALEPHLIB.
C!    Modified :  A.Waananen 28/1/98
C!                Use .NEQV. instead of .NE. for comparison of two logicals
C!
C! Input Arguments:
C!    IJUL    (INTE) : JULIA track number.
C!    NVDET   (INTE) : If your analysis only uses tracks with at
C!                       least 1 VDET hit, set NVDET=1; otherwise =0.
C!    Also uses EVEH and FRFT banks, plus PFIX and PNFX database banks.
C!
C! Output Arguments:
C!    PPCORR  (REAL) : Scale factor applied/to be applied to momentum.
C!    ERCORR  (REAL) : Statistical error on this factor.
C!    IFAIL   (INTE) : = 0 if all OK.
C!                     < 0 if no corrections available for this run.
C!                     > 0 if error occurred.
C!
C! Motivation:
C!   Every year, even after the detector alignment is finished and
C! corrections have been made for field distortions etc., it is found
C! that Ebeam/P in Z0 -> mu+mu- events is not precisely 1, presumably
C! because of residual distortions.
C!   (Typically, in the region |cos(theta)| > 0.9, Ebeam/p is about 0.94
C! for +ve tracks and 1.06 for -ve tracks. Elsewhere, Ebeam/p is usually
C! consistent with 1 to within a percent or so. The effect is not quite
C! forward-backward symmetric).
C!   The relative bias in momentum is proportional to P, so most people
C! analysing hadronic events can ignore it. Exceptions include analyses
C! using the ECAL electron identifiers in the region |cos(theta)| > 0.9,
C! or analyses which are very sensitive to systematic biases in the
C! momenta (e.g. jet charge, tau polarization).
C!   This routine provides a correction for the momenta based upon
C! Ebeam/P measurements in Z0 -> mu+mu- events. It assumes that the
C! corrections for -ve and +ve particles are equal in size, but of
C! opposite sign. This is observed to be true, apart from a constant
C! offset, Ebeam/p = 1.002, which is also present in the MC and so not
C! corrected for.
C!   In principle, the correction depends upon your track selection
C! cuts, but providing that the corrections have a small effect on your
C! analysis, you can ignore this. Argument NVDET does correct for this
C! to first approximation however.
C!
C! If corrections are not up to date, please contact the tracking group.
C-----------------------------------------------------------------------
      SAVE
      LOGICAL FIRST,NEWJUL,LASJUL
      DATA FIRST/.TRUE./
      DATA LASJUL/.FALSE./
C
      PARAMETER(NBIN=20,BSIZE=0.1)
      DIMENSION PCORR(NBIN,0:1),ECORR(NBIN,0:1)
C
      INTEGER AGETDB
      CHARACTER*4 CHAINT
      EXTERNAL AGETDB,CHAINT
C
#include "evehjj.h"
#include "frftjj.h"
#include "rhahjj.h"
#include "pfixjj.h"
#include "alcons.h"
#include "bcs.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
      PPCORR = 1.0
      ERCORR = 0.0
      IFAIL  = 0
C
C Initialize.
      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRFT = NAMIND('FRFT')
        NEVEH = NAMIND('EVEH')
        NRHAH = NAMIND('RHAH')
        NPFIX = NAMIND('PFIX')
        NPNFX = NAMIND('PNFX')
      END IF
C
      KEVEH = IW(NEVEH)
      KFRFT = IW(NFRFT)
      KRHAH = IW(NRHAH)
      IF (KEVEH.EQ.0.OR.KFRFT.EQ.0.OR.KRHAH.EQ.0) THEN
        IFAIL = 1
        GOTO 999
      END IF
C
C Do nothing if this is Monte Carlo.
      KRUN = IW(KEVEH+JEVERN)
      IF (KRUN.LE.2000) GOTO 999
C
C See if this is new or old version of JULIA.
      JULVER = 0
      DO 10 IROW = 1,LROWS(KRHAH)
        IF (CHAINT(ITABL(KRHAH,IROW,JRHAPN)).EQ.'JULI')
     +             JULVER = ITABL(KRHAH,IROW,JRHAPV)
   10 CONTINUE
C
      IF (JULVER.GT.0) THEN 
        NEWJUL = JULVER.GE.300
      ELSE 
        IFAIL = 2
        GOTO 999
      END IF
C
C Access database bank containing corrections.
C They correspond to mean value of 0.5*(1/p(+) - 1/p(-)) of dimuons,
C quoted in twenty bins of cos(theta) from -1 to +1.
C
      IF (NEWJUL) THEN
        ISTAT = AGETDB('PNFX',KRUN)
        KPFIX = IW(NPNFX)
      ELSE
        ISTAT = AGETDB('PFIX',KRUN)
        KPFIX = IW(NPFIX)
      END IF
C
      IF (ISTAT.EQ.0.OR.KPFIX.EQ.0) THEN
C No correction bank found for this run.
        IFAIL = -1
        GOTO 999
C
      ELSE IF ((ISTAT.LT.0).OR.(NEWJUL.NEQV.LASJUL)) THEN
        LASJUL = NEWJUL
C New correction bank loaded. Unpack it into arrays.
        DO 20 IVDOPT = 0,1
          IROW = IVDOPT + 1
          JPFIX = KROW(KPFIX,IROW)
          CALL UCOPY(RW(JPFIX + JPFICO),PCORR(1,IVDOPT),NBIN)
          CALL UCOPY(RW(JPFIX + JPFIER),ECORR(1,IVDOPT),NBIN)
   20   CONTINUE
C Note also run range for which it is valid.
        IVAL1 = ITABL(KPFIX,1,JPFIVR)
        IVAL2 = ITABL(KPFIX,1,JPFIVR + 1)
      ENDIF
C
C Verify that corrections are valid for this run.
      IF (KRUN.LT.IVAL1.OR.KRUN.GT.IVAL2) THEN
        IFAIL = -2
        GOTO 999
      END IF
C
C Find magnetic field.
      IF (KRUN.NE.LRUN) THEN
        LRUN = KRUN
        QMFLD = ALFIEL(DUM)
      END IF
C
C Find cos(theta), momentum and charge of this track.
      IF (IJUL.LT.1.OR.IJUL.GT.LROWS(KFRFT)) THEN
        WRITE(6,30) IJUL,(LROWS(KFRFT))
   30   FORMAT(' PPCORR FATAL ERROR: Crazy input arguments ',2I8)
        STOP
      END IF
      TANL = RTABL(KFRFT,IJUL,JFRFTL)
      CTHETA = TANL/SQRT(1 + TANL**2)
      RINV = RTABL(KFRFT,IJUL,JFRFIR)
      QCH  = SIGN(1.0,-RINV*QMFLD)
      PT   = 1.0E-5*CLGHT*QMFLD/MAX(ABS(RINV),1.0E-10)
      PMOM = PT*SQRT(1.0 + TANL**2)
C
C Find out which cos(theta) bin this track lies in.
      IBIN = 1 + INT((1.0 + CTHETA)/BSIZE)
      IBIN = MIN(IBIN,NBIN)
C
      NVD = MIN(NVDET,1)
 
C Now find scale factor for momentum.
      IF (QCH.GT.0.0) THEN
        PPCORR = 1.0 + PCORR(IBIN,NVD)*PMOM
      ELSE
        PPCORR = 1.0 - PCORR(IBIN,NVD)*PMOM
      END IF
      ERCORR = ECORR(IBIN,NVD)*PMOM
C
  999 CONTINUE
      END



