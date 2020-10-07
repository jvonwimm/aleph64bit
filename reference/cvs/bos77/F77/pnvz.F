      SUBROUTINE PNVZ(NUM,STR,JS)

*     Convert integer NUM into character string STR, JS non-blank
*     characters,. JS = 8) in hexadecimal representation
*     Examples:
*     CALL PNVI( 15,STR,JS)   -> STR='0000000F' ; JS = 8
*     CALL PNVI(255,STR,JS)   -> STR='000000FF' ; JS = 8

      CHARACTER*12 STR,DIG(0:15)*1
      INTEGER ND(12)
      DATA DIG/'0','1','2','3','4','5','6','7','8','9','A','B','C',
     +         'D','E','F'/
      JS=8
      K=NUM
      DO 10 I=0,7
#include "ianshf.h"
   10 STR(JS-I:JS-I)=DIG(J)
  100 RETURN
      END
