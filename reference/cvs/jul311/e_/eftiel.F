      SUBROUTINE EFTIEL ( ICL , ITR )
C=======================================================================
C
C!    Search electron identified corresponding to cluster
C!    in E-Flow analysis
C-    ICL Input Ecal cluster
C-    ITR Output Track #
C-
C-     Author  : M.N. Minard                 04/01/91
C?
C!======================================================================
#ifndef DOC
#include "ect1jj.h"
#include "erl3jj.h"
#include "prtmjj.h"
#include "bcs.h"
#include "bmacro.h"
       ITR = 0
       NAECT1 = NAMIND ('ECT1')
       NAERL3 = NAMIND ('ERL3')
       NAPRTM = NAMIND ('PRTM')
       KECT1 = IW(NAECT1)
       KERL3 = IW(NAERL3)
       KPRTM = IW(NAPRTM)
       IF ( KECT1.NE.0) THEN
         JECT1 = KROW ( KECT1,ICL)
         NERL3 = IW(JECT1+JECTER)
 10      IF ( NERL3.NE.0 ) THEN
           JERL3 = KROW(KERL3,NERL3)
           NETP1 = IW(JERL3+JERLET)
           IF (NETP1.NE.0) THEN
             JPRTM = KROW(KPRTM,NETP1)
             IF ( IW(JPRTM+JPRTCT) .EQ. 1  ) THEN
               ITR = NETP1
             ENDIF
           ENDIF
           NERL3 = IW (JERL3+2)
           GO TO 10
         ENDIF
       ENDIF
       RETURN
       END
#endif
