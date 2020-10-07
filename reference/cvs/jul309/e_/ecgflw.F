      SUBROUTINE ECGFLW ( IADD, CORGFL )
C----------------------------------------------------------------------
C!  - Correction for energy dependance due to gaz flow
C!
C!   Author   :- M.N. Minard                      19-SEP-1991
C!
C!   Inputs:
C!        - iadd          I/  Tower address in ETDI
C!
C!   Outputs:
C!        - CORGFL        R/  Correction factor for theta dependance of
C!                            absolute calibration in barrel
C!
C!                            = 1. for end Caps
C!C!    Bank used : ECGF
C!C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecgfjj.h"
#include "bmacro.h"
C
      CORGFL = 1.
C
C-    Look for correction bank
C
      KECGF = IW(NAMIND('ECGF'))
      IF ( KECGF.EQ.0) GO TO 900
C-
      ITET  = IBITS(IADD,16,8)
      IPHI  = IBITS(IADD,2,9)
      CALL EMDTOW (ITET, IPHI, ISCO, IMO, IRG)
      IMOD = (ISCO-1)*12 + IMO
C Protect against bad data in MonteCarlo
      IF( IMOD .LT. 1 .OR. IMOD .GT. 36 ) GOTO 900
      CORGFL = 1. - RTABL(KECGF,IMOD,JECGCM)*(FLOAT(ITET)-114.5)/63.5
C
 900  CONTINUE
      RETURN
      END
#endif
