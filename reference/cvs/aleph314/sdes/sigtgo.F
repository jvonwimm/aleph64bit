      SUBROUTINE SIGTGO (ICONS,RCONS)
C.---------------------------------------------------------------------
CKEY SCALDES GET GEOMETRY / USER
C     B.BLOCH       October 91
C! Get Geometry constants for  SICAL
C   Input : none
C   Output: ICONS  array of integer constants  DIM(5)
C           RCONS  array of real    constants  DIM(20)
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sigeco.h"
      DIMENSION ICONS(5),RCONS(20)
C - Fill array of geometrical constants
      ICONS(1) = NMODSI
      ICONS(2) = NZBNSI
      ICONS(3) = NRBNSI
      ICONS(4) = NPBNSI
      ICONS(5) = 0
      RCONS(1) = Z0SNSI(1)+DPOSSI(3,1)
      RCONS(2) = Z0SNSI(2)+DPOSSI(3,2)
      RCONS(3) = ZWIDSI
      RCONS(4) = RMINSI(1)
      RCONS(5) = RMINSI(2)
      RCONS(6) = RADSTP
      RCONS(7) = PHSHFT(1,1)
      RCONS(8) = PHSHFT(2,1)
      RCONS(9) = PHSHFT(3,1)
      RCONS(10)= PHISTP
      RCONS(11)= ZWRFSI
      RCONS(12)= ZWRLSI
      RCONS(13)= ZWFRSI
      RCONS(14)= ZWBKSI
      RCONS(15)= ZWLASI
      RCONS(16)= ZWFLSI
      RCONS(17)= OVLPSI
      RCONS(18)= 0.
      RCONS(19)= 0.
      RCONS(20)= 0.
      RETURN
      END
#endif
