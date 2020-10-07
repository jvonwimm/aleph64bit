      SUBROUTINE EC4TRA (JTRA,EC4TR1,EC4TR2,EC4TR3)
C----------------------------------------------------------------
C  Marc Verderi                                        2-10-94
C! Reads the energies in the four central storeys associated to
C  the track JTRA
C---------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "bmacro.h"
      EC4TR1 = -1.
      EC4TR2 = -1.
      EC4TR3 = -1.
      JEIDT = IW (NAMIND('EIDT'))
      IF (JEIDT.LE.0) GOTO 999
      NEIDT = LROWS(JEIDT)
      DO I = 1,NEIDT
         IF (ITABL(JEIDT,I,14).EQ.JTRA) THEN
            EC4TR1 = RTABL(JEIDT,I,11)
            EC4TR2 = RTABL(JEIDT,I,12)
            EC4TR3 = RTABL(JEIDT,I,13)
         ENDIF
      ENDDO
999   CONTINUE

      END
#endif
