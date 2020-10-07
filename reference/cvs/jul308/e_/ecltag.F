      SUBROUTINE ECLTAG ( ICL , IGOO , ICH)
C=======================================================================
C!  Tags neutral clusters to remove fake ones
C!
C!    Authors : M.N. Minard , M. Pepe       26/02/89
C!
C!  Input : Icl cluster number
C!  Output: Igoo = 1 accepted neutral cluster
C!               = 0 not accepted neutral cluster
C!          Ich  = number of charged tracks linked to cluster
C?
C!======================================================================
#ifndef DOC
#include "ect1jj.h"
#include "eclujj.h"
#include "esdajj.h"
#include "erl3jj.h"
#include "ecnmsk.h"
#include "bcs.h"
#include "bmacro.h"
      KESDA = IW (NAESDA)
      KECLU = IW (NAECLU)
      KECT1 = IW (NAECT1)
      KERL3 = IW (NAERL3)
      KETP1 = IW (NAETP1)
      JECLU = KROW (KECLU,ICL)
      IGOO = 1
      ICH = 0
C
C- Determine whether the cluster is charged or not
C
      JECT1 = KROW (KECT1,ICL)
      NERL3 = IW   (JECT1+JECTER)
      IF (NERL3.NE.0) THEN
         CALL ESTAG ( NERL3, ICHT )
         ICH = ICHT
      ENDIF
C
      IF ( ICH . EQ. 0)  THEN
        IFLA = 0
        IST  = 1
C
         DO 10 IL =1,3
           IF (RW (JECLU+JECLE4+IL).GT.0.030) THEN
             IFLA = IFLA + 1
             IST = IL
           ENDIF
 10      CONTINUE
C
C- Count the number of storeys
C
      NTESD  = 0
      NESDA  = IW (JECLU + JECLES)
 11   IF ( NESDA .EQ. 0 ) GO TO 13
      NTESD  = NTESD + 1
      NESDA  = ITABL(KESDA,NESDA,JESDES)
      GO TO 11
C
 13   CONTINUE
      IF ( IFLA.LT.2 .AND.NTESD.LT.2) THEN
C
C- Accept all stack1, no stack2 alone
C- Threshold at 200MEV in stack3
C
        IF ( IST.EQ.2 ) IGOO = 0
        IF ( IST.EQ.3.AND.RW(JECLU+JECLE4).LT.0.2) IGOO=0
      ENDIF
      ENDIF
      RETURN
      END
#endif
