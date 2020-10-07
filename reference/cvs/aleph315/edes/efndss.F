      INTEGER FUNCTION EFNDSS(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES SECTOR / USER
C     H.Videau      Creation 31/12/85   Modification 30/01/88
C! Finds (sub)sector number
C  Looks in the module MD of the subcomponent SC for the
C  subsector containing the point DX.
C   Input :
C           SC     sub component #             INTEGER
C           MD     module #                    INTEGER
C           X      point coordinates in        REAL(3)
C           SYSTM  coordinate system name      CHARACTER * 5
C           ('ALEPH' for example)
C   Output:
C           EFNDSS Subsector # 1<= EFNDSS<= 2  INTEGER
C   Calls: none.
C   Called by USER and EFNDLG
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input variables
      CHARACTER*(*) SYSTM
      INTEGER MD,SC
      REAL X(ECALLS-1)
C    Locales
      REAL TEST
C
C
      IF(SYSTM.EQ.'ALEPH') THEN
        TEST=X(1)*EALISE(1,MD,SC)+X(2)*EALISE(2,MD,SC)+
     &       X(3)*EALISE(3,MD,SC)+     EALISE(4,MD,SC)
                           ELSE
        TEST=X(1)*ELOCSE(1)+X(2)*ELOCSE(2)+
     &       X(3)*ELOCSE(3)+     ELOCSE(4)
                           END IF
      IF(TEST.LE.0)THEN
          EFNDSS=1
      ELSE
          EFNDSS=2
      END IF
      END
#endif
