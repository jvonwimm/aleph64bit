*DK lcstrt
      SUBROUTINE LCSTRT(ZABS,ISTMN,LAYMN,LMIN,ZWIRE)
C--------------------------------------------------------------
C! Convert z-coordinate to plane no.
C. - J.Dines Hansen & P.Hansen - 860513
C. - Finds first storey and layer for track/shower
C. - Called by LCSHOW, LCTRAK                    from this .HLB
C. - Input:  ZABS   = z-distance from entrance
C. - Output:
C.           ISTMN =  storey no. [1,3]
C.           LAYMN =  layer no.  [1,38]
C.           LMIN  =  layer no. within the storey
C.           ZWIRE =  Z of preceeding wire-plane
C -------------------------------------------------
#ifndef DOC
#include "lccomc.h"
C -------------------------------------------------------------
C
C Find the wire-plan preceeding the storey starting the shower
      IF (ZABS .LE. Z123LC(1)) THEN
        ISTMN = 1
        LAYMN = 0
        ZSTEP  = 0.
      ELSEIF (ZABS.LE.Z123LC(1)+Z123LC(2)) THEN
        ISTMN = 2
        LAYMN = LCNLAY(1)
        ZSTEP  = Z123LC(1)
      ELSE
        ISTMN = 3
        LAYMN = LCNLAY(1) + LCNLAY(2)
        ZSTEP  = Z123LC(1)+Z123LC(2)
      ENDIF
      LMIN = 1
      LMAX = LCNLAY(ISTMN)
      DO 100 L = 1,LMAX
       LAYMN = LAYMN + 1
       ZWIRE = ZSTEP-DWIRLC
       ZSTEP = ZSTEP + ZSTPLC(ISTMN)
       IF (ZSTEP .GT. ZABS)                            GOTO 999
  100 LMIN = LMIN + 1
  999 RETURN
      END
#endif
