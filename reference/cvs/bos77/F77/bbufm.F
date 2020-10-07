      SUBROUTINE BBUFM(ARR,I1,I2,LUNP)

*     print array ARR(I1)...ARR(I2) on unit LUNP
*     Automatic recognition of integers/floating point will not work
*     on VAX
      REAL ARR(*)
      INTEGER ILG(0:255)
      CHARACTER*131 LINE, CHLEG*48, STR*12, CHAINT*4
      LOGICAL START
      DATA CHLEG/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ .,:''()+*-/='/
      DATA START/.TRUE./
      IF(START) THEN
*        determine legal character codes
         START=.FALSE.
         DO 02 I=0,255
   02    ILG(I)=0
         DO 04 J=1,48
         I=ICHAR(CHLEG(J:J))
   04    ILG(I)=1
      END IF
C     ...
      J1=MAX0(1,I1)
      IF(LUNP.LE.0.OR.J1.GT.I2) GOTO 100
      J2=((J1-1)/10)*10+10
      IZERO=0
      LINE=' '
   20 J2=MIN0(I2,J2)
      IF(J1.NE.I1.AND.J2.NE.I2) THEN
*        test all zeros in present line
         DO 30 J=J1,J2
         IF(ARR(J).NE.0.0) GOTO 40
   30    CONTINUE
         IF(IZERO.EQ.0) IZERO=J1
         GOTO 90
      END IF
   40 IF(IZERO.NE.0) THEN
         WRITE(LUNP,'(1X,I5,A)')
     +   IZERO,' - .. and all following words are zero'
         IZERO=0
      END IF
      DO 80 J=J1,J2
      JJ=12*(J-J1)+23
*     test all 8-bit bytes in word for character
      STR=CHAINT(ARR(J))
      DO 60 K=1,4
      L=ICHAR(STR(K:K))
      IF(ILG(L).EQ.0) GOTO 70
   60 CONTINUE
      JS=4
      GOTO 75
*     test real or integer
   70 ABSARR=ABS(ARR(J))
      IF(ABSARR.LT.1.0E-20.OR.ABSARR.GT.1.E+20) THEN
         CALL PNVI(ARR(J),STR,JS)
      ELSE
         CALL PNVF(ARR(J),STR,JS)
      END IF
         IF(JS.LE.0.OR.JS.GT.12) THEN
C           WRITE(6,'(1X,''WRONG '',I12)') JS
            JS=12
         END IF
   75 LINE(JJ+1-JS:JJ)=STR(1:JS)
   80 CONTINUE
*     print one line
      CALL PNVI(J1,STR,JS)
      JT=MIN0(5,JS)
      LINE(7-JT:6)=STR(JS-JT+1:JS)
      LINE(7:7)='-'
      CALL PNVI(J2,STR,JS)
      JT=MIN0(3,JS)
      LINE(11-JT:11)=STR(JS-JT+1:JS)
      WRITE(LUNP,'(A)') LINE
      LINE=' '
C
   90 IF(J2.EQ.I2) GOTO 100
      J1=J2+1
      J2=J2+10
      GOTO 20

  100 RETURN
      END
