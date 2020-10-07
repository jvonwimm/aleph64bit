      INTEGER FUNCTION EFNDLG(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES ROW / USER
C     H.Videau      Creation 15/12/85   Modification 06/03/90
C! Finds row number
C  Looks for the row of the module MD in the sub-component SC
C  in which the point DX is located.
C   Input :
C           SC      sub component #              INTEGER
C           MD      module #                     INTEGER
C           X       point coordinates in SYSTM   REAL(3)
C           SYSTM   coordinate system name       CHARACTER*5
C                   ('ALEPH' for example)
C   Output:
C           EFNDLG  Row  index 1<= EFNDLG<= 228  INTEGER
C   Calls: EPLSQL,EFNDSS.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input variables
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
      INTEGER SC,MD
C   Called functions
      INTEGER EFNDSS,EFNDPL,EPLSQL
C    Locales
      INTEGER SS,RW,PL,BARIL,LRW
      PARAMETER (BARIL=2)
C
C
      LRW=ESCOLS(SC)-ESCOFR(SC)+1
      IF(SC.EQ.BARIL) THEN
        SS=1
      ELSE
        SS=EFNDSS(SC,MD,X,SYSTM)
      END IF
      IF(SYSTM.EQ.'ALEPH')  THEN
        RW=EPLSQL(EALIRW(1,1,SS,MD,SC),LRW,EXROPC(1,ESCOFR(SC)),X)
      ELSE
        RW=EPLSQL(ELOCRW(1,1,SS),LRW,EXROPC(1,ESCOFR(SC)),X)
      END IF
C    point hors limites
      IF(RW.GE.LRW.OR.RW.LE.0) THEN
        EFNDLG=RW+ESCORF(SC)
        GO TO 999
      END IF
C      introduction de l'offset de sous-composante.
      RW=RW+ESCORF(SC)
C   traitement des zones pathologiques du baril.
      IF(SC.EQ.BARIL) THEN
        IF(RW.LE.EPLNPI(ECALPL).OR.RW.GE.EPLNPS(ECALPL)) THEN
          PL=EFNDPL(SC,MD,X,SYSTM)
          IF(PL.NE.0.AND.PL.NE.ECALPL+1) THEN
            IF(RW.GE.EPLNPS(PL)) RW=EPLNPS(PL)
            IF(RW.LE.EPLNPI(PL)) RW=EPLNPI(PL)
          END IF
        ENDIF
      END IF
      EFNDLG=RW
C
  999 CONTINUE
C
      END
#endif
