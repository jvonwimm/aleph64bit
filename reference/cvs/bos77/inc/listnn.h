C     ------LISTNN
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR NEXT NAME IN LIST
C                    OR ZERO
C
      LENLS=LEN(LIST)
      IF(LENLS.EQ.0) THEN
         JLS=0
      ELSE IF(LENLS.EQ.1) THEN
         JLS=INDEX('CERST',LIST)
         IF(JLS.NE.0) THEN
            JLS=JLS+ILT
            NLS=JW(JW(JLS))
         END IF
      ELSE
         NLS=LENLS/4
         JLS=-1
      END IF
      ILS=0
    3 IF(JLS.EQ.0) THEN
         NAMI=0
      ELSE IF(JLS.LT.0) THEN
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAME=LIST(4*ILS-3:4*ILS)
            NAMA=INTCHA(NAME)
#include "namen.h"
         ELSE
            JLS=0
            NAMI=0
         END IF
      ELSE
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAMI=JW(JW(JLS)+ILS)
         ELSE
            JLS=0
            NAMI=0
         END IF
      END IF
C     ------
