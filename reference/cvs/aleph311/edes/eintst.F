      LOGICAL FUNCTION EINTST (XRW,CL,ST)
C.----------------------------------------------------------------------
CKEY ECALDES STOREY ROW STACK COLUMN / USER
C   M.Rumpf                June 87
C                                  Modification H.Videau  Fevrier 88
C! Checks storey address validity
C   Input :
C            XRW   row # (soft)  related to Theta   integer
C            CL    column #      related to Phi     integer
C            ST    stack #                          integer
C   Output:
C      if input arguments within validity range then  EINTST = .TRUE.
C      else  EINTST = .FALSE.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C    Input variables
      INTEGER XRW,CL,ST
C    Locales
      INTEGER RG,PL
C
C
            EINTST = .FALSE.
C
C Stack # valid ?
      IF (ST .LE. 0 .OR. ST .GT. ECALST)  GO TO 99
C
C Row #   valid ?
      IF (XRW .LE. 0 .OR. XRW .GT. ECALXW) GO TO 99
C Check the pathological rows
       IF(EXROES(XRW).EQ.2) THEN
          PL=ESTKFR(ST)
        IF(XRW.LT.EPLNPI(PL).OR.XRW.GT.EPLNPS(PL)) GO TO 99
                   END IF
C
C Column # valid ? - Correlation (XRW,CL) O.K?
      RG = EXRGER(EROWEX(EXROER(XRW)))
      IF (CL .GT. 0 .AND. CL .LE. ECALMD*EREGCN(RG))
     &      EINTST = .TRUE.
   99 RETURN
      END
#endif
