      SUBROUTINE BBPFM(XW,NWRD,NAMJ)
C
C     PRINT BANK USING FORMAT CODE
C
#include "boscom.h"
#include "param.h"
      REAL XW(NWRD)
      CHARACTER*6 FMT(14)
      INTEGER LV(3,10)
      DATA FMT/'(1X,I5',',1H- ,','I3,1X',10*',12X',')'/
      IF(IW(LUP).LE.0) GOTO 100
C
C     INITIALIZE FORMAT USE
C
      ID=IW(IDFMT+NAMJ-NSYST)
      I    =3
      ITERM=3
      LEV=0
      N=0
C
C     INITIALIZE PRINTOUT
C
      JA=1
C
C     PRINT ONE LINE
C
   10 J =JA
      JB=MIN(J+9,NWRD)
   20 IF(N.EQ.0) GOTO 50
   30 M=MIN(N,JB-J+1)
      N=N-M
      DO 40 L=1,M
      GOTO (32,34,36),K
C
C     FLOATING POINT
C
   32 ABSARR=ABS(XW(J+L-1))
      IF(ABSARR.GT.0.1.AND.ABSARR.LT.100000.0) THEN
         FMT(J-JA+L+3)=',F12.4'
      ELSE
         FMT(J-JA+L+3)=',G12.4'
      END IF
      GOTO 40
C
C     INTEGER
C
   34 FMT(J-JA+L+3)=',I12'
      GOTO 40
C
C     TEXT (A)
C
#include "bbpfm36.h"
C
   40 CONTINUE
      J=J+M
      IF(J.LE.JB) GOTO 20
      JP=MOD(JB,1000)
      WRITE(IW(LUP),FMT,ERR=45) JA,JP,(XW(J),J=JA,JB)
   45 IF(JB.EQ.NWRD) GOTO 100
      JA=JB+1
      GOTO 10
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
      END
