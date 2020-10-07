*DK hcifir
      INTEGER FUNCTION HCIFIR(S)
C ----------------------------------------------------------------
C          G.Catanesi         1/9/86
C
C! Calculate the first plane crossed  by the shower
C
C        Called by CAHMOD
C -------------------------------------------------
#ifndef DOC
      SAVE
#include "hcgega.h"
#include "hccong.h"
#include "hcloc.h"
C --------------------------------------------------------------
C Starting values
      IF(HCPDIP.GT.0.)THEN
         HCIFIR= 0
      ELSE
         HCIFIR=-1
      ENDIF
C
      IF(S.LT.0.) GO TO 10
C
      HCIFIR = S/HCSMTH
C
      IF(HCPDIP.GT.0..AND.AMOD(S,HCSMTH).GT.HCTUAC) HCIFIR=HCIFIR+1
C
   10 CONTINUE
      HCIFIR =HCIFIR+1
      RETURN
      END
#endif
