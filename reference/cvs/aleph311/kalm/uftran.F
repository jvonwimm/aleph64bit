      INTEGER FUNCTION UFTRAN(R_OLD, R_NEW, STATE_OLD, T, STATE_NEW)
C! Transport a state vector to a new radius
C! Returns process time and new state vector
#ifndef DOC
      DOUBLE PRECISION R_OLD, R_NEW, PHI_NEW, STATE_OLD(*),
     &  STATE_NEW(*), T

      DOUBLE PRECISION EPS
      PARAMETER (EPS = 1.0D-15)
      DOUBLE PRECISION DELTA_PHI, DELTA_R, DELTA_R2, NUMER, DENOM,
     &  PHI_TRY

#include "uftcom.h"

      INTEGER UTPROC

C  Get the advance in process time from old radius to new

      UFTRAN = UTPROC(R_OLD, R_NEW, STATE_OLD, T)
      IF(UFTRAN.NE.0)RETURN

C  Do transport of state vector

      NUMER = ( R_OLD*DSIN(STATE_OLD(1)/R_OLD) +
     +          (DCOS(STATE_OLD(3))-DCOS(STATE_OLD(3) + T))
     +          /STATE_OLD(5)) /
     +          R_NEW
      DENOM = ( R_OLD*DCOS(STATE_OLD(1)/R_OLD) -
     +           (DSIN(STATE_OLD(3))-DSIN(STATE_OLD(3) + T))
     +           /STATE_OLD(5)) /
     +           R_NEW

      IF ( DABS(NUMER) .GT. DABS(DENOM) ) THEN
        PHI_TRY = 2.D0*DATAN2(1.D0-DENOM,NUMER)
      ELSE
        PHI_TRY = 2.D0*DATAN2(1.D0+NUMER,DENOM) - HALF_PI
      ENDIF
      CALL UBA2PI(PHI_TRY)

      STATE_NEW(1) = PHI_TRY * R_NEW
      STATE_NEW(2) = STATE_OLD(2) +
     +          T*DTAN(STATE_OLD(4))/STATE_OLD(5)
      STATE_NEW(3) = STATE_OLD(3) + T
      CALL UBA2PI(STATE_NEW(3))
      STATE_NEW(4) = STATE_OLD(4)
      STATE_NEW(5) = STATE_OLD(5)

      UFTRAN = 0
      RETURN
      END
#endif
