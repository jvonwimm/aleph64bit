      SUBROUTINE EHSIMT(X,V,S,SC,MD,NMGTR,
     &                    NGTR,XD,XF,VS,SS,PL)
C.----------------------------------------------------------------------
C     H.Videau      Creation 15/12/85   Modification 8/09/89
CKEY ECALDES CUT TRACK GALEPH / INTERNAL
C! Cuts long track element
C  This routine simulates GAZ TRACK ELEMENTS by slicing in
C    (NGTR<NMGTR) pieces a long track element
C Input:
C    The long Track Element is defined by sub component SC   (integer)
C                                         module        MD   (integer)
C                                         origine       X(3)
C                                         direction     V(3)
C                                         length        S
C   Output:
C   The Gaz Track elements are defined by their plane       PL (integer)
C                                         their origine     XD(3)
C                                         their extremities XF(3)
C                                         their direction   VS(3)
C                                         their length      SS
C   Calls: EPLSCN
C   Called by EHTRKE in GALEPH if ECAL Run Condition TCUTRK on.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
      EXTERNAL VDOT,EPLSQL
      INTEGER EPLSQL
C  Variables d'input
      REAL    X(ECALLS-1),V(ECALLS-1),S
      INTEGER SC,MD,NMGTR
C  Variables d'output
      REAL XD(ECALLS-1,1),XF(ECALLS-1,1),VS(ECALLS-1,1),SS(1)
      INTEGER NGTR,PL(*)
C  Declaration des locales
      REAL PX(ECALLP),PV(ECALLP),VN,ALK
      INTEGER I,K,PN
      REAL X1(ECALLS-1),X2(ECALLS-1),VV(ECALLS-1)
      INTEGER KPL1,KPL2,KPLMI,KPLMA
C
C  Initialisation des vecteurs projectifs.
      DO 2001 I=1,ECALLS-1
            X1(I)=X(I)
            VV(I)=V(I)
 2001 CONTINUE
      CALL ENORME(VV)
      DO 2004 I=1,ECALLS-1
            X2(I)=X(I)+S*VV(I)
 2004 CONTINUE
C Recherche du plan du premier point
      KPL1=EPLSQL(EALIPL(1,1,MD,SC),ECALPL+1,EPSCPC(1,1,SC),X1)
C Recherche du plan du point terminal
      KPL2=EPLSQL(EALIPL(1,1,MD,SC),ECALPL+1,EPSCPC(1,1,SC),X2)
      IF(KPL1.EQ.KPL2)     THEN
            NGTR=0
            GO TO 999
      END IF
      KPLMI=MIN(KPL1,KPL2)
      KPLMI=MAX(KPLMI,1)
      IF(KPLMI.NE.1.AND.KPLMI.NE.11.AND.KPLMI.NE.34)KPLMI=KPLMI+1
      KPLMA=MAX(KPL1,KPL2)
      KPLMA=MIN(KPLMA,ECALPL)
C
      DO 2050 PN=1,ECALLP
      PX(PN)=EALIPL(1,PN,MD,SC)*X1(1)+EALIPL(2,PN,MD,SC)*X1(2)+
     &       EALIPL(3,PN,MD,SC)*X1(3)+EALIPL(4,PN,MD,SC)
      PV(PN)=EALIPL(1,PN,MD,SC)*VV(1)+EALIPL(2,PN,MD,SC)*VV(2)+
     &       EALIPL(3,PN,MD,SC)*VV(3)
 2050  CONTINUE
      NGTR=0
      DO 2003 K=KPLMI,KPLMA
          IF(NGTR.EQ.NMGTR)       GO TO 999
          NGTR=NGTR+1
          VN=PV(2)*EPSCPC(1,K,SC)-PV(1)*EPSCPC(2,K,SC)
          IF(VN.EQ.0) GO TO 2012
          ALK=(PX(1)*EPSCPC(2,K,SC)-PX(2)*EPSCPC(1,K,SC))/VN
          SS(NGTR)=ECALGP/VN
          DO 2002 I=1,ECALLS-1
              XD(I,NGTR)=X1(I)+ALK*VV(I)
              XF(I,NGTR)=X1(I)+(ALK+SS(NGTR))*VV(I)
              VS(I,NGTR)=V(I)
              PL(NGTR)=K
 2002     CONTINUE
 2012     CONTINUE
 2003 CONTINUE
C
  999 RETURN
C
      END
#endif
