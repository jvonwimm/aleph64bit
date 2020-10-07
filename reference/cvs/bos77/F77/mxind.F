      FUNCTION MXIND(JW,IND)

*     DETERMINE INDEX OF BANK BEHIND BANK AT INDEX IND
*        (=NEXT BANK IN BOS-ARRAY)
*     IF IND=0 INDEX OF FIRST BANK IN STORAGE IS RETURNED
*     IF IND<0 INDEX OF FIRST BANK OF LAST READ OPERATION IS RETURNED

#include "param.h"
      INTEGER JW(*)
*     ...
      JND=IND
      IF(JND.LE.0) THEN
*        INDEX OF FIRST BANK IN ARRAY
         JND=JW(INM)+3
      ELSE
         IF(JW(JND).GT.0) THEN
            JND=JND+JW(JND)+4
         ELSE
            JND=JND-JW(JND)
         END IF
      END IF
   10 IF(JND.GT.JW(IGP)) THEN
         JND=0
      ELSE
         IF(JW(JND).LT.0) THEN
            JND=JND-JW(JND)
            GOTO 10
         END IF
      END IF
  100 MXIND=JND
      RETURN
      END
