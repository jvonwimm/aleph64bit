      LOGICAL FUNCTION VOISTO(IST1,IST2)
C.----------------------------------------------------------------------
CKEY GAMPACK NEIGHBOURING / INTERNAL
C   AUTHOR   : A. Rouge       3/01/90
C            : modify J.C.Brient 1/10/91
C! true for 2 neighbours storeys
C
C   Input :
C           IST1   itheta,jphi,stack storey 1   INTEGER
C           IST2   itheta,jphi,stack    "   2   INTEGER
C   Output:
C           Function = .TRUE. for 2 neighbour storeys
C   Calls: EMDTOW
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
#include "ecoxa.h"
      DIMENSION IST1(3),IST2(3)
      DIMENSION IDRG(4),IPLM1(4),IPLM2(8),IPLM3(12)
      LOGICAL VSN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA IDRG /95,191,287,383/
      DATA IPLM1/1,3,5,7/
      DATA IPLM2/1,2, 4,5, 7,8, 10,11/
      DATA IPLM3/1,2,3, 5,6,7, 9,10,11, 13,14,15/
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        IST46=ITOV1
        IST47=ITOV1+1
        IST48=ITOV1+2
        IST49=ITOV1+3
        IST50=ITOV1+4
C
        IST51=ITOV2+1
        IST52=ITOV2+2
        IST53=ITOV2+3
        IST54=ITOV2+4
        IST55=ITOV2+5
        IST56=ITOV2+6
      ENDIF

      VSN=.FALSE.
      IF ( IST1(1).LT.1.OR.IST1(1).GT.228) GO TO 100
      IF ( IST2(1).LT.1.OR.IST2(1).GT.228) GO TO 100
      IF ( IST1(2).LT.1.OR.IST1(2).GT.384) GO TO 100
      IF ( IST2(2).LT.1.OR.IST2(2).GT.384) GO TO 100
      CALL EMDTOW(IST1(1),IST1(2),ISC1,IMD1,IRG1)
      CALL EMDTOW(IST2(1),IST2(2),ISC2,IMD2,IRG2)
C Same stack
C ----------
      IF ( IST1(3).EQ.IST2(3) )THEN
C Same subcomponent
C -----------------
        IF(ISC1 .EQ. ISC2)THEN
C     Barrel
          IF(ISC1.EQ.2)THEN
            IF((IST1(2).EQ.IST2(2)) .AND. IABS(IST1(1)-IST2(1)).EQ.1)
     &        THEN
              VSN=.TRUE.
              GO TO 100
            ENDIF
            IF(IST1(1).EQ.IST2(1) )THEN
              IDELT=IABS(IST1(2)-IST2(2))
              IF(IDELT.EQ.1 .OR. IDELT.EQ.383)THEN
                VSN=.TRUE.
                GO TO 100
              ENDIF
            ENDIF
C      EndCap
          ELSE
C      Same Region
            IF(IRG1.EQ.IRG2)THEN
              IF((IST1(2).EQ.IST2(2)) .AND. IABS(IST1(1)-IST2(1)).EQ.1)
     &          THEN
                VSN=.TRUE.
                GO TO 100
              ENDIF
              IF(IST1(1).EQ.IST2(1) )THEN
                IDELT=IABS(IST1(2)-IST2(2))
                IF(IDELT.EQ.1 .OR. IDELT.EQ.IDRG(IRG1))THEN
                  VSN=.TRUE.
                  GO TO 100
                ENDIF
              ENDIF
            ELSEIF( IABS(IST1(1)-IST2(1)).EQ.1     .AND.
     X           (IST1(2)/(4*IRG1)) .EQ. (IST2(2)/(4*IRG2))
     X           ) THEN
C storeys at a region limit
C in the same sector
C -------------------------
              IRGMIN = MIN0(IRG1,IRG2)
              IF(IRG1 .EQ. IRGMIN) THEN
                IPHIMI = IST1(2)
                IPHIMA = IST2(2)
              ELSE
                IPHIMI = IST2(2)
                IPHIMA = IST1(2)
              ENDIF
              IF( IRGMIN .EQ .1 ) THEN
                IPHIMI = MOD ( IPHIMI - 1 , 4 ) + 1
                IPHIMA = MOD ( IPHIMA - 1 , 8 ) + 1
                IPHINX = IPLM1 ( IPHIMI )
                IF( IPHIMA.EQ.IPHINX .OR. IPHIMA.EQ.(IPHINX+1) ) THEN
                  VSN = .TRUE.
                  GO TO 100
                ENDIF
              ELSEIF( IRGMIN .EQ. 2 ) THEN
                IPHIMI = MOD ( IPHIMI - 1 , 8 ) + 1
                IPHIMA = MOD ( IPHIMA - 1 , 12 ) + 1
                IPHINX = IPLM2 ( IPHIMI )
                IF( IPHIMA.EQ.IPHINX .OR. IPHIMA.EQ.(IPHINX+1) ) THEN
                  VSN = .TRUE.
                  GO TO 100
                ENDIF
              ELSEIF( IRGMIN .EQ. 3 ) THEN
                IPHIMI = MOD ( IPHIMI - 1 , 12 ) + 1
                IPHIMA = MOD ( IPHIMA - 1 , 16 ) + 1
                IPHINX = IPLM3 ( IPHIMI )
                IF( IPHIMA.EQ.IPHINX .OR. IPHIMA.EQ.(IPHINX+1) ) THEN
                  VSN = .TRUE.
                  GO TO 100
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSEIF( IABS( IST1(3)-IST2(3) ) .EQ. 1 ) THEN
        IF(ISC1.EQ.ISC2) THEN
          IF ( IST1(1).EQ.IST2(1) .AND. IST1(2).EQ.IST2(2) ) THEN
            VSN = .TRUE.
            GO TO 100
          ENDIF
        ENDIF
      ENDIF
C
C Overlap
C ---------
      IF(ISC1.NE.ISC2)THEN
        IF(ISC1.EQ.2)THEN
          ITB = IST1(1)
          JPB = IST1(2)
          KSB = IST1(3)
          ITE = IST2(1)
          JPE = IST2(2)
          KSE = IST2(3)
        ELSEIF(ISC2.EQ.2)THEN
          ITB = IST2(1)
          JPB = IST2(2)
          KSB = IST2(3)
          ITE = IST1(1)
          JPE = IST1(2)
          KSE = IST1(3)
        ELSE
          GO TO 100
        ENDIF
C
C     ITB,JPB,KSB barrel  storey coor.
C     ITE,JPE,KSE End cap storey coor.
C
        IF(ITB.GT.100)THEN
          ITB = ITHTO+1  -ITB
          ITE = ITHTO+1  -ITE
        ENDIF
C
        IF(KSE.NE.1)GO TO 100
        IF(JPB.NE.JPE) GO TO 100
        IF    (ITE.EQ.IST46)THEN
          IF(KSB.EQ.1 .AND.
     &      (ITB.EQ.IST51.OR.ITB.EQ.IST52) )VSN=.TRUE.
          GO TO 100
        ELSEIF(ITE.EQ.IST47) THEN
          IF(KSB.EQ.1 .AND.
     &       (ITB.EQ.IST52.OR.ITB.EQ.IST53) )VSN=.TRUE.
          GO TO 100
        ELSEIF(ITE.EQ.IST48)THEN
          IF(ITB.EQ.IST53.AND.KSB.EQ.1)VSN = .TRUE.
          IF(ITB.EQ.IST54.AND.KSB.EQ.2)VSN = .TRUE.
          GO TO 100
        ELSEIF(ITE.EQ.IST49)THEN
          IF(KSB.EQ.2 .AND.
     &       (ITB.EQ.IST54.OR.ITB.EQ.IST55) )VSN=.TRUE.
          GO TO 100
        ELSEIF(ITE.EQ.IST50)THEN
          IF(KSB.EQ.2 .AND.
     &       (ITB.EQ.IST55.OR.ITB.EQ.IST56) )VSN=.TRUE.
          GO TO 100
        ELSE
          GO TO 100
        ENDIF
      ENDIF
C
  100 VOISTO=VSN
      RETURN
      END
#endif
