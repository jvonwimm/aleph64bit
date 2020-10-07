      SUBROUTINE FPRNN(XW,NWRD,IFMT,NFMT)

*     print array using format in numeric array IFMT

      REAL XW(NWRD)
      CHARACTER*131 LINE, STR*14, STR2*12, CHAINT*4
      EQUIVALENCE (IW,WI)
      INTEGER LV(3,10),IFMT(196)
      CALL GLUNP(LUNP)
      IF(NFMT.EQ.0) GOTO 100

*     initialize format use

      I    =3
      ITERM=3
      LEV=0
      N=0

*     initialize printout

      JA=1

*     print one line

   10 J =JA
      JB=MIN0(J+9,NWRD)
      LINE=' '
   20 IF(N.EQ.0) GOTO 50
   30 M=MIN0(N,JB-J+1)
      N=N-M
      DO 40 L=1,M
      LL=12*(J-JA+L)+11
      IF(K.EQ.1) THEN
*        floating point format
         CALL PNVF(XW(J+L-1),STR,JS)
         IF(JS.LE.0.OR.JS.GT.12) THEN
C           WRITE(6,'(1X,''WRONG '',I12)') JS
            JS=12
         END IF
      ELSE IF(K.EQ.2) THEN
*        integer
         CALL PNVI(XW(J+L-1),STR,JS)
      ELSE IF(K.EQ.3) THEN
*        character
         STR=CHAINT(XW(J+L-1))
         JS=4
      ELSE IF(K.EQ.4) THEN
*        16 bit integer
         WI=XW(J+L-1)
         STR=' '
#include "iandsh.h"
         CALL PNVI(I2       ,STR2,JS)
         STR(13-JS:12)=STR2(1:JS)
         CALL PNVI(I1       ,STR2,JS)
         STR( 7-JS: 6)=STR2(1:JS)
         JS=12
      ELSE
         CALL PNVZ(XW(J+L-1),STR,JS)
      END IF
      LINE(LL-JS+1:LL)=STR(1:JS)
C
   40 CONTINUE
      J=J+M
      IF(J.LE.JB) GOTO 20
      JP=MOD(JB,1000)
      CALL PNVI(JA,STR,JS)
      LINE(7-JS:6)=STR(1:JS)
      LINE(7:7)='-'
      CALL PNVI(JP,STR,JS)
      JT=MIN0(3,JS)
      LINE(11-JT:11)=STR(JS-JT+1:JS)
      IF (LUNP.NE.0) WRITE(LUNP,'(A)') LINE
   45 IF(JB.EQ.NWRD) GOTO 100
      JA=JB+1
      GOTO 10
*     get next format code
   50 I=I+1
      IF(I.GT.NFMT) THEN
         I=ITERM
         GOTO 50
      END IF
*     right parenthesis
      IF(IFMT(I).EQ.0) THEN
         LV(3,LEV)=LV(3,LEV)+1
         IF(LV(3,LEV).GE.LV(2,LEV)) THEN
            ITERM=LV(1,LEV)-1
            LEV=LEV-1
         ELSE
            I=LV(1,LEV)
         END IF
      ELSE
         N=IFMT(I)/10
         K=IFMT(I)-10*N
         IF(K.EQ.0) THEN
*        left parenthesis
            LEV=LEV+1
            LV(1,LEV)=I
            LV(2,LEV)=N
            LV(3,LEV)=0
         ELSE
*           format F or I or A
            IF(NFMT.NE.3) THEN
               IF(I.NE.NFMT-1)      GOTO 30
               IF(LEV.EQ.0)         GOTO 30
               IF(I.NE.LV(1,LEV)+1) GOTO 30
            END IF
            N=999999999
            GOTO 30
         END IF
      END IF
      GOTO 50

  100 RETURN
      END
