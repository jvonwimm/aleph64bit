        REAL FUNCTION ERPADS(RW,CL,PL)
C-----------------------------------------------------------------------
C     H.Videau      Creation 01/06/89
CKEY ECALDES PAD AREA / USER
C! Computes the area of the pad
C  It takes into account the limits of the sensitive zone of the module
C  as well as the pathological barrel pads.
C   Input :
C           RW  row number      INTEGER
C           CL  column number   INTEGER
C           PL  plane number    INTEGER
C   Calls: AIRSCT,ESRPL,EMDTOW,EPLST
C   Called by USER
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input variables
        INTEGER RW,CL,PL
C External
        REAL AIRSCT
C Locales
        INTEGER I,ST,SC,MD,RG
        INTEGER NPL
        REAL PLA(4,14),A,SECTION(4),PLST
C Execution
          ST=EPLNES(PL)
          PLST=(PL-ESTKFR(ST)+.5)/(ESTKLS(ST)-ESTKFR(ST)+1)+FLOAT(ST)
        CALL ESRPL('ALEPH',RW,CL,ST,NPL,PLA)
        CALL EMDTOW(RW,CL,SC,MD,RG)
          IF(SC.EQ.2.AND.(RW.GT.EPLNPS(PL).OR.RW.LT.EPLNPI(PL))) THEN
            ERPADS=0.
            GO TO 999
          END IF
        CALL EPLST('ALEPH',SC,MD,PLST,SECTION)
        DO 1 I=1,ECALNP
        NPL=NPL+1
        PLA(1,NPL)=-EALILP(1,I,MD,SC)
        PLA(2,NPL)=-EALILP(2,I,MD,SC)
        PLA(3,NPL)=-EALILP(3,I,MD,SC)
        PLA(4,NPL)=-EALILP(4,I,MD,SC)
 1      CONTINUE
        ERPADS=AIRSCT(NPL,PLA,SECTION)
          IF(SC.EQ.2.AND.RW.EQ.EPLNPS(PL)) THEN
            CALL ESRPL('ALEPH',RW+1,CL,ST,NPL,PLA)
            NPL=NPL+ECALNP
            ERPADS=ERPADS+AIRSCT(NPL,PLA,SECTION)
          END IF
          IF(SC.EQ.2.AND.RW.EQ.EPLNPI(PL)) THEN
            CALL ESRPL('ALEPH',RW-1,CL,ST,NPL,PLA)
            NPL=NPL+ECALNP
            ERPADS=ERPADS+AIRSCT(NPL,PLA,SECTION)
          END IF
 999    CONTINUE
        END
#endif
