      SUBROUTINE BTEWS(JW,LUNA,NAME,NR)
C
C     PRINT BANK IN CARD FORMAT USING FORMAT CODE
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      EXTERNAL STRPR
      INTEGER LV(3,10)
      COMMON/STRSTR/JS,IAST,IBST,LUNS
      CHARACTER NAME*4
      INTEGER JW(*)
*     ...
      LUN=IABS(LUNA)
      IND=MLINK(JW,NAME,NR)
      IF(IND.EQ.0) GOTO 100
      NAMJ=NAMIND(NAME)
      LUNS=LUN
      NBLK=0
      IBF=NLINK('+BUF',LUN)
      IF(IBF.NE.0) THEN

*     stop, if wrong unit

#include "unpackio.h"
         IF(IOMD.NE.2.OR.IOST.EQ.1) THEN
            IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) LUN
            CALL BABEND('BTEWR')
         END IF
         IF(IOST.EQ.2) GOTO 05
         IF(IW(IBF+2).NE.0) NBLK=IW(IBF+2)
      ELSE
         IBF=NBANK('+BUF',LUN,11)
      END IF
      IBF= NBANK('+BUF',LUN,11)
      IF(IBF.EQ.0) CALL BBSPC(IW,'BTEWS')
      DO 12 M=1,10
   12 IW(IBF+M)=0
#include "unpackio.h"
      IOMD=2
      IOST=2
#include "packio.h"
      IF(NBLK.LE.0) NBLK=130
      IW(IBF+2)=NBLK
   05 NBLK=IW(IBF+2)
      CALL CNVL(2,NBLK)
      CALL CNVT(NAME,STRPR)
      CALL CNVI(NR,1,STRPR)
      CALL CNVT('/',STRPR)
C
C     INITIALIZE FORMAT USE
C
      ID=IW(IDFMT+NAMJ-NSYST)
      I    =3
      ITERM=3
      LEV=0
      N=0
      NWRD=JW(IND)
      IF(ID.EQ.0) THEN
         N=NWRD
         K=1
      ELSE IF(IW(ID+3).NE.0) THEN
         N=NWRD
         K=9
      END IF
*
*     INITIALIZE
*
      JA=1
      JB=NWRD
C
C     PRINT ONE LINE
C
   10 J =JA
   20 IF(N.EQ.0) GOTO 50
   30 M=MIN(N,JB-J+1)
      N=N-M
      IF(K.EQ.1) THEN
C
C     FLOATING POINT
C
         CALL CNVF(JW(IND+J),M,STRPR)
C
C     INTEGER
C
      ELSE IF(K.EQ.2) THEN
         CALL CNVI(JW(IND+J),M,STRPR)
C
C     TEXT (A)
C
      ELSE IF(K.EQ.3) THEN
         CALL CNVA(JW(IND+J),M,STRPR)
C
C     HEXADECIMAL (Z)
C
      ELSE
         CALL CNVZ(JW(IND+J),M,STRPR)
C
      END IF
      J=J+M
      IF(J.LE.JB) GOTO 20
      CALL STRPR
      IF(LUNA.LT.0) GOTO 100
      CALL CNVT('ENDQ',STRPR)
      CALL STRPR
      GOTO 100
C
C     GET NEXT FORMAT CODE
C
   50 I=I+1
      IF(I.GT.IW(ID)) THEN
         I=ITERM
         GOTO 50
      END IF
C
C     RIGHT PARENTHESIS
C
      IF(IW(ID+I).EQ.0) THEN
         LV(3,LEV)=LV(3,LEV)+1
         IF(LV(3,LEV).GE.LV(2,LEV)) THEN
            ITERM=LV(1,LEV)-1
            LEV=LEV-1
         ELSE
            I=LV(1,LEV)
         END IF
C
C
C
      ELSE
         N=IW(ID+I)/10
         K=IW(ID+I)-10*N
         IF(K.EQ.0) THEN
C
C        LEFT PARENTHESIS
C
            LEV=LEV+1
            LV(1,LEV)=I
            LV(2,LEV)=N
            LV(3,LEV)=0
C
C
C
         ELSE
C
C           FORMAT F OR I OR A
C
            IF(IW(ID).NE.3) THEN
               IF(I.NE.IW(ID)-1)    GOTO 30
               IF(LEV.EQ.0)         GOTO 30
               IF(I.NE.LV(1,LEV)+1) GOTO 30
            END IF
            N=2147483647
            GOTO 30
         END IF
      END IF
      GOTO 50
C
  100 RETURN
C 101 FORMAT(1X,A80)
C 102 FORMAT(A80)
 1001 FORMAT('0BOS--BTEWR-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
      END
