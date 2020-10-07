      SUBROUTINE BBNOT(JW,SUBPGM)
C
C     PRINT ERROR 'ARRAY NOT INITIALIZED'
C
      INTEGER JW(*)
      CHARACTER*(*) SUBPGM
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*5 TARR(40)
      DATA TARR          /'ADARG','RETCD','     ','CUNIT','PUNIT',
     1                     'PRETC','PBNKS','PDAMS','ARNM1','ARNM2',
     2                     'ARNUM','ARLEN','ARADD','INAMA','IGAPA',
     3                     'IWRKA','NDELN','NDELW','IDELN','IDELW',
     4                     'I+LNK','ILSTC','ILSTE','ILSTR','ILSTS',
     5                     'ILSTT','ILSTU','     ','IIOST','CHECK',
     6                     'SRET1','SRET2','SRET3','SRET4','SRET5',
     7                     'SRET6','SRET7','SRET8','NGACN','NGACW'/
C     ...
      IF(JW(LUP).NE.0) THEN
         WRITE(JW(LUP),1001) SUBPGM
         WRITE(JW(LUP),1002) TARR
         WRITE(JW(LUP),1003)
         CALL BBUFM(JW,1,100,JW(LUP))
      END IF
      CALL BABEND('BBNOT')
  100 RETURN
 1001 FORMAT('0BOS++',A5,'+  ARRAY NOT INITIALIZED - STOP'/
     1       14X,'FIRST 100 WORDS OF ARRAY ARE PRINTED BELOW'/)
 1002 FORMAT(13X,10(7X,A5))
 1003 FORMAT(20X,'HISTOGRAM OF FREE WORDS IN 10 PERCENT BINS')
      END
