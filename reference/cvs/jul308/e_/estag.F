      SUBROUTINE ESTAG (  NERL3 , IGOO)
C=======================================================================
C!    Gives number of tracks related to cluster
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C-    Input  : NERL3 1st relation to cluster in ERL3
C-    Output : IGOO = number of tracks related to this cluster
C?
C!======================================================================
#ifndef DOC
#include "etp1jj.h"
#include "ecnmsk.h"
#include "emskex.h"
#include "bcs.h"
#include "bmacro.h"
C
C-    Pointers
C
      IERL3 = NERL3
      KERL3 = IW ( NAERL3)
      KETP1 = IW ( NAETP1 )
      IGOO  = 0
C
      IF ( KERL3.EQ.0 ) GO TO 900
C
 10   JERL3 = KROW ( KERL3,IERL3 )
      NETP1 = IW ( JERL3 + 3)
      IF ( NETP1  . EQ. 0  ) GO TO 50
      JETP1 = KROW (KETP1,NETP1 )
      IF ( IW(JETP1+JETPEF).EQ.0 ) GO TO 50
      IF (ABS(RW(JETP1+JETPCH)).LT..5) GO TO 50
      IF ( RW(JETP1+JETPPT).LT.PMINC ) GO TO 50
      IGOO = IGOO +1
 50   IERL3 = IW (JERL3 + 2)
      IF ( IERL3.NE.0 )  GO TO 10
 900  CONTINUE
      RETURN
      END
#endif
