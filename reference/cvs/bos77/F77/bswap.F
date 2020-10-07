      SUBROUTINE BSWAP(JW,NAM1,NAM2)
C
C     EXCHANGE NAMES NAM1 AND NAM2 IN NAMED BANKS
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*4 NAM1,NAM2
      INTEGER    JW(*)
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BSWAP')
      JW1=JW(1)
      JW(1)=0
      JW2=0
      NAMA1=INTCHA(NAM1)
      NI1  =NAMIND(NAM1)
      IN1  =JW(NI1)
      NAMA2=INTCHA(NAM2)
      NI2  =NAMIND(NAM2)
      IN2  =JW(NI2)

C     RESET LINK BANKS

      JND=JW(IDL)+NI1-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0
      JND=JW(IDL)+NI2-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0

      IND=NI1+1
   10 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(IND-3)=NAMA2
         GOTO 10
      END IF

      IND=NI2+1
   20 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(IND-3)=NAMA1
         GOTO 20
      END IF

      JW(NI1)=IN2
      JW(NI2)=IN1
  100 RETURN
      END
