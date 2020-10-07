      INTEGER FUNCTION XHVBIT(jhvst)
CKEY XLUMOK TRIG /INTERNAL
C----------------------------------------------------------------------
C!   Returns raw HV status bits, without run/detector specific repairs.
C!   Not to be used for MonteCarlo
C!
C!   Called from XHVSTA, VDHVOK
C!
C!   Author   :   M.Cattaneo         2-Dec-1997
C!   (Code extracted from B.Bloch adaptation of QHVSTA from Alpha)
C!
C!   Inputs:           None
C!   Outputs:          Integer JHVST = HV status bits word
C!
C!   Return value:     Flags which bank was used to get HV bit:
C!                     0 - No bank found
C!                     1 - XTCN
C!                     2 - X1RG
C!                     3 - REVH
C!                     4 - LUPA
C!                     5 - SILH
C!
C!   Description:
C!     To be used within any ALEPH program
C!     Returns the High Voltage status of ALEPH according to
C!     the bit mapping of X1RG or XTCN ( when reading Raw Data),
C!     REVH, word 1 (POT).
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
#include "x1rgjj.h"
#include "xtcnjj.h"
#include "revhjj.h"
#include "lupajj.h"
#include "silhjj.h"
      INTEGER jhvst
      INTEGER kX1RG,kXTCN,kREVH,kLUPA,kSILH
      INTEGER naX1RG,naXTCN,naREVH,naLUPA,naSILH
      LOGICAL first / .TRUE. /
      SAVE first,naLUPA,naSILH,naREVH,naXTCN,naX1RG
      INTEGER  NAMIND
      EXTERNAL NAMIND
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C Save the name indices
      IF( first ) THEN
        naX1RG = NAMIND('X1RG')
        naXTCN = NAMIND('XTCN')
        naREVH = NAMIND('REVH')
        naLUPA = NAMIND('LUPA')
        naSILH = NAMIND('SILH')
        first = .FALSE.
      ENDIF
C
      kX1RG = IW(naX1RG)
      kXTCN = IW(naXTCN)
      kREVH = IW(naREVH)
      kLUPA = IW(naLUPA)
      kSILH = IW(naSILH)
C
C  Detector HV status X1RG or XTCN bank or in word 1 of REVH 
C  or LUPA/SILH for Luminosity events on POT's 
C  If all banks missing, the event is bad, declare detector's off.
C  The order of the checks is important when reading Raw Data during
C  Julia processing : XTCN and REVH may be booked but not yet filled
C  at the time the info is needed, so try first Raw data bank X1RG 
C
      IF (kX1RG.GT.0) THEN
C! TSR is in row 2
        jhvst  = ITABL(kX1RG,2,JX1RCO+2)
        xhvbit = 2
      ELSEIF (kXTCN.GT.0) THEN
        jhvst  = ITABL(kXTCN,1,JXTCTR)
        xhvbit = 1
      ELSEIF (kREVH.GT.0) THEN
        jhvst  = ITABL(kREVH,1,JREVDS)
        xhvbit = 3
      ELSEIF (kLUPA.GT.0) THEN
        jhvst  = ITABL(kLUPA,1,JLUPHV)
        xhvbit = 4
      ELSEIF (kSILH.NE.0) THEN
        jhvst  = ITABL(kSILH,1,JSILHV)
        xhvbit = 5
      ELSE
        jhvst  = 0
        xhvbit = 0
      ENDIF

  999 CONTINUE
      END
#endif
