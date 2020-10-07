      SUBROUTINE BBRET(JW,ICODE,SUBPGM,NAME)
      INTEGER JW(*)
#include "param.h"
      CHARACTER *(*) SUBPGM
      CHARACTER*4    NAME
      CHARACTER*40  TEXT(6)
      DATA TEXT/'INSUFF.SPACE FOR NEW BANK         BANK= ',
     2          'INSUFF.SPACE TO INCR. LENGTH      BANK= ',
     3          '                                        ',
     4          'READ ERROR                              ',
     5          'READ ERROR (FORMAL ERROR)               ',
     6          'READ ERROR (INSUFFICIENT SPACE)         '/
C     ...
      IF(JW(LUP).EQ.0.OR.JW(LUW).EQ.0) GOTO 100
      JW(LUW)=JW(LUW)-1
      WRITE(JW(LUP),1001) SUBPGM,ICODE,TEXT(ICODE),NAME
  100 RETURN
 1001 FORMAT('0BOS--',A5,'-  RETURN CODE',I2,4X,A40,1X,A4)
      END
