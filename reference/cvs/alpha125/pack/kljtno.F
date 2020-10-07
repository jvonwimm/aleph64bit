      FUNCTION KLJTNO(IJET)
CKEY FILL JETS LEPTONS / USER
C----------------------------------------------------------------------
C! Get number of objects in Lepton Jet IJET
C                                                   J.Boucrot  15.11.94
C  Called from user
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "pljtjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      KLJTNO = 0
      IF (KLNJET.LE.0) GO TO 999
      JPLJT = IW(NAPLJT)
      IF (JPLJT.EQ.0) GO TO 999
      IJT = IJET-KLFJET+1
      IF (IJT.LE.0.OR.IJT.GT.LROWS(JPLJT)) GO TO 999
      KLJTNO = ITABL(JPLJT,IJT,JPLJNO)
 999  RETURN
      END
#endif
