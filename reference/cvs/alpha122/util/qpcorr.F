      FUNCTION QPCORR(ITRK,ASCALE,NVDET,ERCORR)
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
C!
C! Input Arguments:
C!    ITRK    (Integer): ALPHA track number.
C!    ASCALE  (Logical): Set .TRUE. if ALPHA variables like QX,QY,QZ
C!                       are to be rescaled by calling QVSCAL.
C!                       N.B. Don't call this routine twice for the
C!                       same track with ASCALE = .TRUE. !
C!    NVDET   (Integer): If your analysis only uses tracks with at
C!                       least 1 VDET hit, set NVDET=1; otherwise =0.
C!    Also uses EVEH and FRFT banks.
C! Output Arguments:
C!    QPCORR  (Real)   : Scale factor applied/to be applied to momentum.
C!    ERCORR  (Real)   : Statistical error on this factor.
C! Calls:
C!    Function PPCORR (ALEPHLIB).
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
C! analysis, you can ignore this. Arguement NVDET does correct for this
C! to first approximation however.
C!
C! If corrections are not up to date, please contact the tracking group.
C-----------------------------------------------------------------------
      LOGICAL ASCALE
      DATA NWARN/0/
      PARAMETER (MAXWAR=10)
C
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C Find track number in FRFT bank.
      IJUL = KTN(ITRK)
C
C Find scale factor to be applied to momentum and error on it.
      QPCORR = PPCORR(IJUL,NVDET,ERCORR,IFAIL)
C
      IF (IFAIL.NE.0.AND.NWARN.LT.MAXWAR) THEN
        NWARN = NWARN + 1
        IF (IFAIL.LT.0) THEN
          WRITE(KUPTER,10) IFAIL,KRUN
   10     FORMAT(' QPCORR warning ',I2,
     &           ' : No corrections available for run ',I6)
        ELSE
          WRITE(KUPTER,20) IFAIL,KRUN
   20     FORMAT(' QPCORR error ',I2,
     &           ' : No corrections made for run ',I6)
        END IF
      END IF
C
C Scale ALPHA momentum if required.
      IF (ASCALE) CALL QVSCAL(ITRK,QPCORR)
      END
