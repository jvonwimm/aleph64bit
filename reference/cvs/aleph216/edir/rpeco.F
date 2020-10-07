       SUBROUTINE RPECO( ETOWE, TGV1)
C----------------------------------------------------------------------
CKEY EDIR CLUSTER PARAMETERS
C! Calculate parameters of the 2 most energetic cluster.
C-
C   Input   : None
C   Output  : ETOWE  = Total tower energy
C             TGV1(1-2) = Energy of the 2 most energetic cluster
C             TGV1(3-4) = Polar angleof the 2 most energetic cluster
C             TGV1(5)   = Angle between the 2 most energetic cluster
C             TGV1(6-7) = PECO # of the 2 most energetic cluster
C-
C   Called by   : ECAGET
C   Calls  : None
C   Input banks : PECO
C-
C                                    Author: S.Dugey  - 910400
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pecojj.h"
C --
      DIMENSION TGV1(7)
C --
#include "bmacro.h"
C --
C   Initialise variables
C --
      ETOWE= 0.
      CALL VZERO (TGV1,7)
C --
      NAPECO = NAMIND ('PECO')
      KPECO = IW (NAPECO)
      IF ( KPECO .EQ . 0) GOTO 999
      NPECO = LROWS (KPECO)
C --
C   Loop on PECO
C --
      IPEH1 = 0
      EH1 = 0.
      DO 100 IPECO = 1,NPECO
C --
C   Skip LCAL PECO
C --
        IF ( ITABL (KPECO,IPECO,JPECKD).EQ.192) GO TO 100
        EPEC = RTABL( KPECO,IPECO,JPECER)
        ETOWE = ETOWE + EPEC
        IF(EPEC.GT.EH1) THEN
          IPEH1 = IPECO
          EH1 = EPEC
        ENDIF
 100  CONTINUE
C --
      IPEH2 = 0
      EH2 = 0.
      DO 101 IPECO = 1,NPECO
        IF ( ITABL (KPECO,IPECO,JPECKD).EQ.192) GO TO 101
        EPEC = RTABL( KPECO,IPECO,JPECER)
        IF(EPEC.GT.EH2.AND.IPECO.NE.IPEH1) THEN
          IPEH2 = IPECO
          EH2 = EPEC
        ENDIF
 101  CONTINUE
C --
C   Fill TGV1
C --
      IF(EH1.NE.0.) THEN
        TGV1(1) = EH1
        TGV1(3) = RTABL(KPECO,IPEH1,JPECTH)
        TGV1(6) = IPEH1
        IF( EH2.NE.0.) THEN
          TGV1(2) = EH2
          TGV1(4) = RTABL(KPECO,IPEH2,JPECTH)
          PH1 = RTABL(KPECO,IPEH1,JPECPH)
          PH2 = RTABL(KPECO,IPEH2,JPECPH)
          SIN1 = SIN (TGV1(3))
          SIN2 = SIN (TGV1(4))
          COTH12 = SIN1*SIN2 * (COS(PH1)*COS(PH2) + SIN(PH1)*SIN(PH2))
     +       + COS(TGV1(3)) * COS(TGV1(4))
          IF (ABS(COTH12).GT.1.) COTH12=SIGN(1.,COTH12)
          TGV1(5) = ACOS ( COTH12 )
          TGV1(7) = IPEH2
        ENDIF
      ENDIF
C --
 999  CONTINUE
      RETURN
      END
#endif
