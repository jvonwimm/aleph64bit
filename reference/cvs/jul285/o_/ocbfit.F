      SUBROUTINE OCBFIT (BP, BPVEC, BPERR)
C----------------------------------------------------------------------
C!  - Close form beamspot fit
C!
C!
C!   Inputs:
C!        - BP(2)           R   Input Beam Position
C!
C!   Outputs:
C!        - BPVEC(2)        R   Output Beam Position
C!        _ BPERR(2)        R   Output Beam Position Error
C!
C!   Author   :- D. Brown               8-APR-1993
C!
C!   Modified: Steve Wasserbaech, April 1995
C!   Take beam size from ALLR instead of data statement
C!   Modified: Pere Comas, July 1995
C!   Protect RSINV against an error return
C!   Modified: SW, 18 October 1995
C!   Take beam size from VBSCOM; allow customized chunks; return
C!   zeros in case of failure.
C?
C!======================================================================
#ifndef DOC
C     IMPLICIT NONE
      SAVE SIGX0L, SIGY0L
#include "bcs.h"
#include "rlunit.h"
#include "obscom.h"
C
      REAL BP(2), BPVEC(3), BPERR(3)
C
      INTEGER ICOR, JCOR, NDOF, IBUF, IP
      INTEGER ITRK, IERR, JCHOP
      REAL CP, SP, SIGMA2, D0FIT2, D0, PHI, SD02, CHISQ, ALPHA
      REAL ARRAY(3,3), VEC(3)
      REAL SIGX0L, SIGY0L
      DATA SIGX0L / -1. /
      DATA SIGY0L / -1. /
C
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
C  Zero the arrays:
C
      CALL VZERO(BPVEC,3)
      CALL VZERO(BPERR,3)
      ALPHA = 0.
      CALL VZERO(VEC,3)
      CALL VZERO(ARRAY,9)
C
C  Print a message if the beam size has changed:
C
      IF ((SIGX0 .NE. SIGX0L) .OR. (SIGY0 .NE. SIGY0L)) THEN
        WRITE (LOUTRL,'(A,2F8.4)')
     >       ' Vcbfit>  Using beam size parameters', SIGX0, SIGY0
        SIGX0L = SIGX0
        SIGY0L = SIGY0
      ENDIF
C
C  Loop over the tracks, removing outlyers:
C
      NDOF = 0
      DO IBUF=1,NBSBUF
        IP = IBSTRK(IBUF)
        DO ITRK=1,LROWS(IP)
          IF (NCHOP .EQ. 0) THEN
            JCHOP = 0
          ELSE
C(old work bank format)            JCHOP = MOD(ITABL(IP,ITRK,4)-1,NCHOP) + 1
             JCHOP = MOD(         ITRK   -1,NCHOP) + 1
          ENDIF
          IF (ICHOP .EQ. JCHOP) THEN
C(old work bank format)            D0 = RTABL(IP,ITRK,1)
C(old work bank format)            PHI = RTABL(IP,ITRK,2)
C(old work bank format)            SD02 = AMAX1(RTABL(IP,ITRK,3), S2MIN)
            IF(ITABL(IP,ITRK,1).GT.0) CALL OCBPRE(IW(KROW(IP,ITRK)+1),
     &                                            RW(KROW(IP,ITRK)+2),
     &                                            RW(KROW(IP,ITRK)+5))
            IF(ITABL(IP,ITRK,1).NE.0) THEN 
            D0 = RTABL(IP,ITRK,4)
            PHI = RTABL(IP,ITRK,3)
            SD02 = AMAX1(RTABL(IP,ITRK,10), S2MIN)
            SP = SIN(PHI)
            CP = COS(PHI)
            SIGMA2 = (SIGX0*SP)**2 + (SIGY0*CP)**2 + SD02
            D0FIT2 = (D0 - (BP(1)*SP - BP(2)*CP))**2
            IF (D0FIT2 .LE. BSD0SCUT*SIGMA2) THEN
C
C  Increment the sums:
C
              ALPHA = ALPHA + D0**2/SIGMA2
              ARRAY(1,1) = ARRAY(1,1) + SP**2/SIGMA2
              ARRAY(2,2) = ARRAY(2,2) + CP**2/SIGMA2
              ARRAY(3,3) = ARRAY(3,3) + 1./SIGMA2
              ARRAY(1,2) = ARRAY(1,2) - SP*CP/SIGMA2
              ARRAY(1,3) = ARRAY(1,3) + SP/SIGMA2
              ARRAY(2,3) = ARRAY(2,3) - CP/SIGMA2
C
              VEC(1) = VEC(1) + SP*D0/SIGMA2
              VEC(2) = VEC(2) - CP*D0/SIGMA2
              VEC(3) = VEC(3) +    D0/SIGMA2
C
              NDOF = NDOF + 1
            ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C  Symmetrize the array:
C
      ARRAY(2,1) = ARRAY(1,2)
      ARRAY(3,1) = ARRAY(1,3)
      ARRAY(3,2) = ARRAY(2,3)
C
C  Invert the matrix:
C
      CALL RSINV(3,ARRAY,3,IERR)
      IF (IERR .NE. 0) GO TO 100
C
C  Solve for the beamspot and chisquared
C
      CHISQ = ALPHA
      DO ICOR=1,3
        BPVEC(ICOR) = 0.
        DO JCOR=1,3
          BPVEC(ICOR) = BPVEC(ICOR) + ARRAY(ICOR,JCOR)*VEC(JCOR)
          CHISQ = CHISQ - ARRAY(ICOR,JCOR)*VEC(JCOR)*VEC(ICOR)
        ENDDO
      ENDDO
C
C  Get the errors:
C
      DO ICOR=1,3
        BPERR(ICOR) = SQRT(MAX(ARRAY(ICOR,ICOR),0.))
      ENDDO
C
C  Done:
C
 100  CONTINUE
#ifdef ALP2
C
C  Save results
C
*      CALL SAVE_RESULTS('BOM',CHISQ,NDOF,BPVEC,BPERR,IERR)
#endif
      RETURN
      END
#endif
