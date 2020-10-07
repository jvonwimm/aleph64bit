      SUBROUTINE BOSRPT

*     REPORT ON OPERATION
      CHARACTER*4 NAM1,NAM2,CHAINT, TEXTF1*32, TEXTF2*32
      INTEGER IOCT1(2),IOCT2(2)
#include "boscom.h"
#include "param.h"

      IF(IW(IDNAM).EQ.0.OR.IW(LUP).EQ.0) GOTO 100
      WRITE(IW(LUP),103)
      IDX=0
      CALL WBANK(IW,IDX,IW(IDNAM),*100)
      CALL SORTBL(IW(IDNAM+1),IW(IDX+1),-IW(IDNAM))
      NHALF=IW(IDNAM)/2
      IS   =IW(IDNAM)-NHALF
      IF(IS.GT.10) IS=((IS+9)/10)*10
*     pointer to io-statistic
      ID   =IW(IOS)
      WRITE(IW(LUP),101)
      DO 10 I=1,IS
      IF(MOD(I,10).EQ.1) WRITE(IW(LUP),'(1X)')
      J=IW(IDX+I)
      NAM1=CHAINT(IW(IDNAM+J))
      NUM1=J
      IOCT1(1)=0
      IOCT1(2)=0
      IF(ID.NE.0) THEN
         IF(2*J.LE.IW(ID)) THEN
            IOCT1(1)=IW(ID+2*J-1)
            IOCT1(2)=IW(ID+2*J  )
         END IF
      END IF
      CALL BOSFMT(NAM1,TEXTF1)
      IF(I+IS.LE.IW(IDNAM)) THEN
         J=IW(IDX+I+IS)
         NAM2=CHAINT(IW(IDNAM+J))
         NUM2=J
         IOCT2(1)=0
         IOCT2(2)=0
         IF(ID.NE.0) THEN
            IF(2*J.LE.IW(ID)) THEN
               IOCT2(1)=IW(ID+2*J-1)
               IOCT2(2)=IW(ID+2*J  )
            END IF
         END IF
         CALL BOSFMT(NAM2,TEXTF2)
      WRITE(IW(LUP),102) NUM1,NAM1,IOCT1,TEXTF1, NUM2,NAM2,IOCT2,TEXTF2
      ELSE
         WRITE(IW(LUP),102) NUM1,NAM1,IOCT1,TEXTF1
      END IF

   10 CONTINUE
      CALL WDROP(IW,IDX)
  100 RETURN
  101 FORMAT
     +(1X,2(' pos  Name      in     out  Format................  ',12X))
  102 FORMAT(1X,2(I4,2x,A4,2I8,2X,A32,4X))
  103 FORMAT(' The table below shows all used names in alphabetical ',
     +'oder. The column ''pos'' gives the order,'/
     +' in which the names were made known to the system ',
     +'(useful in reading dumps).'/
     +' The numbers under ''in'' and ''out'' count the number',
     +' of records, in which banks of the name appeared.'/)
      END
