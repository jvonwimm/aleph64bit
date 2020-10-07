      SUBROUTINE UFSWMX(RADIUS,STATE_OLD,T_PROC,STATE_NEW)
C! Swim state vector through time T and convert to global coords
#ifndef DOC
      DOUBLE PRECISION RADIUS, STATE_OLD(*), T_PROC, STATE_NEW(*)
      DOUBLE PRECISION PHI_NEW, D0_TRK, T_CHECK

#include "uftcom.h"

      PHI_NEW = STATE_OLD(3) + T_PROC
      CALL UBA2PI(PHI_NEW)
      D0_TRK = RADIUS*DSIN(PHI_NEW-STATE_OLD(1)/RADIUS) +
     +       2.D0/STATE_OLD(5)*(DSIN(0.5D0*T_PROC))**2

      IF ( ABS(D0_TRK) .GT. RADIUS ) THEN
        IF ( T_PROC .GT. 0.D0 ) THEN
          T_PROC = T_PROC - ONE_PI
        ELSE
          T_PROC = T_PROC + ONE_PI
        ENDIF
        PHI_NEW = STATE_OLD(3) + T_PROC
        CALL UBA2PI(PHI_NEW)
        D0_TRK  = RADIUS*DSIN(PHI_NEW-STATE_OLD(1)/RADIUS) +
     +          2.D0/STATE_OLD(5)*(DSIN(T_PROC/2.D0))**2
      ENDIF

      STATE_NEW(1) = STATE_OLD(5)
      STATE_NEW(2) = DTAN( STATE_OLD(4) )
      STATE_NEW(3) = PHI_NEW
      STATE_NEW(4) = D0_TRK
      STATE_NEW(5) = STATE_OLD(2) +
     +          T_PROC/STATE_OLD(5)*DTAN(STATE_OLD(4))
      RETURN
      END
#endif
