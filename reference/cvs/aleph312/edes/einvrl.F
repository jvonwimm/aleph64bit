      SUBROUTINE EINVRL
C.----------------------------------------------------------------------
CKEY ECALDES INVERT RELATION / INTERNAL
C     H.Videau      Creation 26/01/88    Modification  14/03/90
C! Inverts some relationships
C   Called by ECDFRD.
C   Calls: none
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Locales
      INTEGER I,K
C  Invert ECOL -> EREG
      K=1
      EREGFR(K)=1
      EREGLS(ECALRG)=ECALCL
      DO 1 I=2,ECALCL
       IF(ECOLER(I).NE.ECOLER(I-1))THEN
        K=K+1
        EREGFR(K)=I
        EREGLS(K-1)=I-1
                                   END IF
 1    CONTINUE
      DO 10 I=1,ECALRG
       EREGID(I)=I
       EREGCN(I)=EREGLS(I)-EREGFR(I)
 10   CONTINUE
C  Invert EPLN -> ESTK
      K=1
      ESTKID(K)=K
      ESTKFR(K)=1
      ESTKLS(ECALST)=ECALPL
      DO 2 I=2,ECALPL
      IF(EPLNES(I).NE.EPLNES(I-1))THEN
      K=K+1
      ESTKID(K)=K
      ESTKFR(K)=I
      ESTKLS(K-1)=I-1
                                   END IF
 2    CONTINUE
C  Invert EXRO -> ESCO plus decalage pour EXROPC
      K=1
      ESCOFR(K)=1
      ESCOLS(ECALSC)=ECALXW+3
      DO 3 I=2,ECALXW
      IF(EXROES(I).NE.EXROES(I-1))THEN
      K=K+1
      ESCOFR(K)=I+K-1
      ESCOLS(K-1)=ESCOFR(K)-1
                                   END IF
 3    CONTINUE
C  Invert EVLF -> EVOL
      DO 14 I=1,NEVOL
        EVOLFR(I) = 0
   14 CONTINUE
      DO 4 I=1,NEVLF
      IF(EVOLFR(EVLFEV(I)).EQ.0) EVOLFR(EVLFEV(I))=I
      EVOLLS(EVLFEV(I))=I
 4    CONTINUE
C  Invert EXRO -> EROW -> EXRG
      K=1
      EXRGFR(K)=1
      EXRGLS(ECALXG)=ECALXW
      DO 5 I=2,ECALXW
        IF(EROWEX(EXROER(I)).NE.EROWEX(EXROER(I-1)))THEN
          K=K+1
          EXRGFR(K)=I
          EXRGLS(K-1)=I-1
        END IF
 5    CONTINUE
C  Invert ECNC -> ETYV
      DO 16 I=1,NETYV
        ETYVFT(I) = 0
        ETYVFF(I) = 0
        ETYVFR(I) = 0
   16 CONTINUE
      DO 6 I=1,NECNC
      IF(ETYVFT(ECNCET(I)).EQ.0) ETYVFT(ECNCET(I))=I
      ETYVLT(ECNCET(I))=I
 6    CONTINUE
C  Invert EFAC -> ETYV
      DO 7 I=1,NEFAC
      IF(ETYVFF(EFACET(I)).EQ.0) ETYVFF(EFACET(I))=I
      ETYVLF(EFACET(I))=I
 7    CONTINUE
C  Invert ECRP -> ETYV
      DO 8 I=1,NECRP
      IF(ETYVFR(ECRPET(I)).EQ.0) ETYVFR(ECRPET(I))=I
      ETYVLR(ECRPET(I))=I
 8    CONTINUE
      DO 9 I=1,NETYV
      ETYVID(I)=I
      ETYVFC(I)=ETYVLF(I)-ETYVFF(I)+1
      ETYVCR(I)=ETYVLR(I)-ETYVFR(I)+1
      ETYVNG(I)=0
 9    CONTINUE
      END
#endif
