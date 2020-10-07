*DK hcnstr
      INTEGER FUNCTION HCNSTR(DX)
C.----------------------------------------------------------------
C.                                        140486 G.Catanesi
C! Return the numbers of streamers fired in the tube by each track. elem
C.
C ------------------------------------------------------
#ifndef DOC
      SAVE
#include "hccong.h"
#include "hcloc.h"
C -----------------------------------------------------------------
      HCNSTR =0
C   cambiamenti momentanei....
      IF(DX.LT.HCSTDT)DX = HCSTDT + 0.0001
C
      HCNSTR = DX/HCSTDT
      DSTRE = MOD(DX,HCSTDT)
      IF (DSTRE.GT.0.) THEN
         PROB = DSTRE/HCSTDT
         F = RNDM(DUMMY)
         IF(F.LE.PROB)HCNSTR=HCNSTR + 1
      ENDIF
C
      IF(HCNSTR.GT.5.)THEN
         HCNSTR=INT(HCNSTR*0. 5+ 0.5)
         IF(HCNSTR.LT.5.)HCNSTR=5.
      ENDIF
C
      RETURN
      END
#endif
