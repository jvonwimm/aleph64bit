      SUBROUTINE UF2USE(N_PT, DBL_TRK, R_PT, I_COORD, N_COORD,
     &  INNER, OUTER, *)
C!  Find usable coordinates (consistent with input track params)
#ifndef DOC
      DOUBLE PRECISION OMEGA_TRK,D0_TRK,PHI0_TRK,TANL_TRK,Z0_TRK
      DOUBLE PRECISION DBL_TRK(5)
      DOUBLE PRECISION R_PT(*)
      INTEGER INNER, OUTER, I_COORD(*)

      PARAMETER (MIN_PT = 3)

#include "vrldcom.h"
#include "uftcom.h"

      DOUBLE PRECISION MIN_RAD, MAX_RAD

C! Load local variables

      OMEGA_TRK = DBL_TRK(1)
      TANL_TRK = DBL_TRK(2)
      PHI0_TRK = DBL_TRK(3)
      D0_TRK = DBL_TRK(4)
      Z0_TRK = DBL_TRK(5)

C Find usable points
C =======================
C The track has to completely cross the tpc pad-row to justify a hit at
C the pad-row radius similar requirements hold for itc / vd

      MAX_RAD = (2.D0/OMEGA_TRK - D0_TRK)*DSIGN(1.D0,OMEGA_TRK)
      IF ( MAX_RAD .GT. DBLE(UKRTPC) ) THEN
        MAX_RAD = MAX_RAD -1.5D0
      ELSEIF ( MAX_RAD .GT. DBLE(UKRITC) ) THEN
        MAX_RAD = MAX_RAD - 1.D0
      ELSE
        MAX_RAD = MAX_RAD - 0.01D0
      ENDIF
      OUTER = 0
  140 OUTER = OUTER + 1
      IF ( OUTER.GT.N_PT ) RETURN 1
      IF ( I_COORD(OUTER) .EQ. 0) GOTO 140
      IF ( R_PT(OUTER) .GE. MAX_RAD ) GOTO 140
      IF ( R_PT(OUTER) .LT. RAD_CUT ) RETURN 1
      MIN_RAD = ABS( D0_TRK )
      IF ( MIN_RAD .GT. DBLE(UKRTPC) ) THEN
        MIN_RAD = MIN_RAD + 1.5D0
      ELSEIF ( MIN_RAD .GT. DBLE(UKRITC) ) THEN
        MIN_RAD = MIN_RAD + 1.D0
      ELSE
        MIN_RAD = MIN_RAD + 0.01D0
      ENDIF
      MIN_RAD = MAX(MIN_RAD,DBLE(RAD_CUT))
      INNER = N_PT + 1
  150 INNER = INNER - 1
      IF ( INNER.LT.OUTER ) RETURN 1
      IF ( R_PT(INNER) .LE. MIN_RAD ) GOTO 150
      N_COORD = 0
      DO I = OUTER, INNER
        IF (I_COORD(I).NE.0) N_COORD = N_COORD + 1
      ENDDO
      IF ( N_COORD .LT. MIN_PT ) RETURN 1
      RETURN
      END
#endif
