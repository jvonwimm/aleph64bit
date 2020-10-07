*DK tpnois
      SUBROUTINE TPNOIS
C-----------------------------------------------------------------------
C!  Produce two noise spectra: one resulting from PARALLEL noise,
C!  the other from SERIES noise.
C
C  Called from:  TPINEL
C  Calls:        WBANK, NORMCO
C
C  Inputs:  TPCONS.INC:  --TPRSER, serial resistance in preamp
C                        --TPRPAR, parallel resistance in preamp
C                        --NTMXNO, the maximum number of bins to
C                                  be put in the noise signal
C           TPCOND.INC:  --TPANBN, the analog signal time bin
C                        --NLSHAP, the length of the shaping signal
C           BANKS:       --work bank id ITSHAP, containing the
C                          shaping signal
C                        --work bank id ITDSHP, containing the
C                          derivative of the shaping signal
C
C  Outputs:  BANKS:      --work bank id ITPNOI, containing a noise
C                          spectrum arising from parallel noise
C                          convoluted with the shaping signal
C                          IW(ITPNOI+I) = parallel noise in Ith bin
C                        --work bank id ITSNOI, containing a noise
C                          spectrun arising from series noise
C                          convoluted with the derivative of the shaping
C                          signal
C
C  Note on weight due to Thevenin equivalence for series noise:
C  The series noise spectrum should include a mulplicative weighting
C  factor RS*CTOT.  However, we wish to use this noise
C  spectrum for different detector elements with different capacitances.
C  Thus, we here simply leave out this weighting factor and include it
C  when we actually need to add the noise to a signal.
C
C  M. Takashima     D. DeMille
C     16.9.84          13.1.85
C
C  Modifications:
C
C     1.  22feb89 DFC -- Add in error message in case the call to WBANK
C                        gives an error return.
C-----------------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpcons.h"
#include "tpcond.h"
#include "alcons.h"
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL CKBOLT,CROOMT,ECHARG
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
C  Open a BOS work bank for each type of noise.
C
      CALL WBANK(IW,ITPNOI,NTMXNO,*999)
      CALL WBANK(IW,ITSNOI,NTMXNO,*999)
C
C  Calculate number of RMS electrons in one second
C
      PELPS = ( 2.D0 * CKBOLT * CROOMT ) / ( TPRSER * (ECHARG**2) )
      SELPS = ( 2.D0 * CKBOLT * CROOMT ) / ( TPRPAR * (ECHARG**2) )
C
C  We will randomize the distributions according to a sigma
C  depending on avg. number of electrons per bin.
C
      PELPB = PELPS * ( TPANBN * 1.E-9 )
      SELPB = SELPS * ( TPANBN * 1.E-9 )
C
      SIGP = SQRT(SELPB)
      SIGS = SQRT(PELPB)
C
C  Now we're ready to fill the noise arrays.  Note that we subtract
C  out the DC levels and include only the variation around them.
C
      DO 1 I = 1,NTMXNO
         CALL RANNOR(RN1,RN2)
         RW(ITPNOI+I) = SIGP*RN1
         RW(ITSNOI+I) = SIGS*RN2
    1 CONTINUE
C
C  Now fold the parallel noise with the input response function and the
C  series noise with the derivative of the irf.  Note that,
C  because we need the spectrum before a point to get the result of
C  the convolution integral at that point, we must abandon the first
C  NLSHAP = length of shaping signal points of the noise spectrum.
C  This is taken care of as we go.
C
      IFIN = NTMXNO - NLSHAP
C
      DO 3 INOI = 1,IFIN
C
          SUMP = 0.
          SUMS = 0.
          ITOP = INOI + NLSHAP
C
          DO 2 ISHP = 1,NLSHAP
             SUMP = SUMP + RW(ITPNOI+ITOP-ISHP)*RW(ITSHAP+ISHP)
             SUMS = SUMS + RW(ITSNOI+ITOP-ISHP)*RW(ITDSHP+ISHP)
 2        CONTINUE
C
          RW(ITPNOI+INOI) = SUMP
          RW(ITSNOI+INOI) = SUMS
C
 3    CONTINUE
C
C  Let go of the garbage at the end of the BOS banks
C
      CALL WBANK(IW,ITPNOI,IFIN,*999)
      CALL WBANK(IW,ITSNOI,IFIN,*999)
C
      RETURN
C
  999 WRITE(6,'(/'' +++TPNOIS+++ Insufficient BOS array space!''/)')
      RETURN
C
      END
