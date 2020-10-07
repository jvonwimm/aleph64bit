      SUBROUTINE ENPROX(ICL,VEXTR,IGOO)
C=======================================================================
C!   Determines whether cluster is in road
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-
C    Input
C      ICL : Cluster number
C      VEXTR (1->3) : Extrapolation coordinates in stack 2 of Ecal
C    Output
C      IGOO : 0  Cluster ICL  not in track road
C             1  Cluster in track road
C?
C!======================================================================
#ifndef DOC
#include "eclujj.h"
#include "ecnmsk.h"
#include "emskex.h"
#include "emskpa.h"
#include "bcs.h"
       DIMENSION VEXTR(3), COSI(3)
#include "bmacro.h"
C
C-     Flag & pointers initialization
C
       IGOO = 0
       KECLU = IW(NAECLU)
       JECLU = KROW(KECLU,ICL)
       IF (RW(JECLU+JECLE4).LT.EMINC) GO TO 800
C
C-     Calculate cluster coordinate & distance to extrapolated track
C
      COSI (3) = RW(JECLU+JECLR4)*COS(RW(JECLU+JECLT4))
      RT = RW(JECLU+JECLR4)*SIN(RW(JECLU+JECLT4))
      COSI (1) = RT *COS (RW(JECLU+JECLF4))
      COSI (2) = RT *SIN (RW(JECLU+JECLF4))
 10   CONTINUE
C
      DR = 0.
      DO 20 IX=1,3
      DR = DR + COSI(IX)**2
 20   CONTINUE
C
 100  CONTINUE
      CANG2 = VDOTN(VEXTR,COSI,3)
      TANG2 = ACOS(CANG2)
      RANG2 = SIN(TANG2)*SQRT(DR)
      XVAR = RANG2/XNORM(2)
      IF (XVAR.LT.1.5*ZON(2)) IGOO = 1
      IF (CANG2.LT.0.)        IGOO = 0
      IF (XVAR.LT.20) IGOO =1
      IF ( CANG2.LT.0) IGOO =0
 800  CONTINUE
      RETURN
      END
#endif
