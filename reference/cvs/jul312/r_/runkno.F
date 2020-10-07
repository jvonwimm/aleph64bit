      SUBROUTINE RUNKNO
C--------------------------------------------------------------------
C!  - Handle records which are not EVENT and not RUN (most likely slowco
C!
C! Author          S. Orteu            20-JAN-1988
C! Modified        J.Knobloch          12-SEP-1989
C!   Modified :- E. Lancon              4-JUN-1991
C!              Write "Slow Control" Record as they come
C!               M. Cattaneo              JUN-1996
C!              Add call to BMSPOT
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
      LOGICAL RMOD
C
C Compute EDIR Class Word and accumulate statistics
C
      CALL RNXMOD(MODEDI,RMOD)
      CALL ALCLASW (MSKED)
      CALL ALSUMCL (MSKED)
C
C Handle slow control for each detector
C
      IF (FDETRF(JULVD)) CALL VSLOWC
      IF (FDETRF(JULIT)) CALL ISLOWC
      IF (FDETRF(JULTP)) CALL TSLOWC
      IF (FDETRF(JULEC)) CALL ESLOWC
      IF (FDETRF(JULLC)) CALL LSLOWC
      IF (FDETRF(JULSA)) CALL SSLOWC
      IF (FDETRF(JULSI)) CALL SISLOWC
      IF (FDETRF(JULHC)) CALL HSLOWC
      IF (FDETRF(JULMU)) CALL MSLOWC
      IF (FDETRF(JULBC)) CALL BSLOWC
      IF (FDETRF(JULBO)) CALL OSLOWC
C
      CALL OLSPOT(0)
C
C User ROUTINE FOR SLOW CONTROL
C
      CALL USLOWC
C
C     write record
C
      DO 112 I=1,NWRTMX
        IF(JOUTRL(I).NE.NOOUTP) CALL ABWSEL ('E')
  112 CONTINUE
      IF (FDEBRF.AND.JDBDRF(JULSK).GE.3) THEN
        WRITE(LOUTRL,'(A)') '0RUNKNO- List of banks on slow control'
        CALL AUBLIS('E')
      ENDIF
      END
#endif
