      SUBROUTINE ECTHRE
C***********************************************************************
C!SET GLOBAL THRESHOLD ON CLUSTER ENERGY
C!      INPUT :'ECLU'
C!      OUTPUT:'ECLU'(MODIFIED)
C!      LOOP ON CLUSTERS
C!        CANCEL POINTER TO STOREYS(SET TO 0)
C!
C***********************************************************************
#ifndef DOC
C
#include "ecnamc.h"
#include "bcs.h"
#include "eclcom.h"
#include "eclujj.h"
C?
C? LOOP ON BANK ECLU AND SET FLAG (IE CLUSTER_POINTER TO STOREYS=0  ) FO
C? WITH TOTAL ENERGY BELOW ECGTHR.
C?
      KECLU = IW(NAECLU)
      NECLU = IW(KECLU + LMHROW)
      DO 10 ICL=1,NECLU
         IF (RW(KECLU+LMHLEN+LECLUA*(ICL-1)+ JECLE4) .LT. ECGTHR) THEN
             IW(KECLU+LMHLEN+LECLUA*(ICL-1)+ JECLES) = 0
         ENDIF
   10 CONTINUE
C
      RETURN
      END
#endif
