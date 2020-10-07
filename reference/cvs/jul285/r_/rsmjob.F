      SUBROUTINE RSMJOB
C----------------------------------------------------------------------
C!  - Detectors Job Summary
C!  - Modified:  M. Cattaneo              JUN-1996 - Call BMSPOT
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rstatr.h"
C--------------------------------------------------------------------
      NRUNRR(1) = NRUNRR(2)+NRUNRR(3)+NRUNRR(4)+NRUNRR(5)
C
C print summaries
C
      IF (FDETRF(JULVD).AND.JPRERF(JULVD).GE.1) THEN
        CALL VPRSUM
      ENDIF
      IF (FDETRF(JULIT).AND.JPRERF(JULIT).GE.1) THEN
        CALL IPRSUM
      ENDIF
      IF (FDETRF(JULTP).AND.JPRERF(JULTP).GE.1) THEN
        CALL TPRSUM
      ENDIF
      IF (FDETRF(JULEC).AND.JPRERF(JULEC).GE.1) THEN
        CALL EPRSUM
      ENDIF
      IF (FDETRF(JULLC).AND.JPRERF(JULLC).GE.1) THEN
        CALL LPRSUM
      ENDIF
      IF (FDETRF(JULSA).AND.JPRERF(JULSA).GE.1) THEN
        CALL SPRSUM
      ENDIF
      IF (FDETRF(JULSI).AND.JPRERF(JULSI).GE.1) THEN
        CALL SIPRSUM
      ENDIF
      IF (FDETRF(JULHC).AND.JPRERF(JULHC).GE.1) THEN
        CALL HPRSUM
      ENDIF
      IF (FDETRF(JULMU).AND.JPRERF(JULMU).GE.1) THEN
        CALL MPRSUM
      ENDIF
      IF (FDETRF(JULBC).AND.JPRERF(JULBC).GE.1) THEN
        CALL BPRSUM
      ENDIF
      IF (FDETRF(JULBO).AND.JPRERF(JULBO).GE.1) THEN
        CALL OPRSUM
      ENDIF
      IF ((FDETRF(JULYR).OR.FDETRF(JULYT))  .AND.
     +    (JPRERF(JULYR).GE.1.OR.JPRERF(JULYT).GE.1)
     +    ) THEN
        CALL YPRSUM
      ENDIF
      CALL OLSPOT(2)
      IF (JPRERF(JULSK).GE.1) THEN
        CALL RPRSUM
      ENDIF
C--------------------------------------------------------------------
      END
#endif
