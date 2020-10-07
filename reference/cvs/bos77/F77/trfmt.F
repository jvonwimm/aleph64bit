      SUBROUTINE TRFMT(FMT,IFMT,NFMT)

*     translate format FMT into integer array IFMT(1)...IFMT(NFMT)
*     NFMT = 0 for wrong syntax in format statement

      CHARACTER*(*) FMT, FM*1
      INTEGER IFMT(*)

*      1   0
*      2   =3 DEFINED BY CALL
*      3   BIT CODE (=1 B32, =2 B16, =3 B08)
*      4...FMT CODES, IF BIT CODE = 0
*
*      FMT CODE   translated from...
*
*      N*10       N'('
*             0    ')'
*      N*10 + 1   N'F'          floating point
*      N*10 + 2   N'I'          integer
*      N*10 + 3   N'A'          text (4char per word)
*      N*10 + 4   N'J'          16-bit integer
*      N*10 + 5   N'K'          8-bit integer
*

*     initialize array ifmt

      IEFMT=0
      IFMT(1)=0
      IFMT(2)=3
      IFMT(3)=0
      N=3

*     test special format 'B08' etc

      KF=0
      IF(FMT.EQ.'B32') KF=1
      IF(FMT.EQ.'B16') KF=2
      IF(FMT.EQ.'B08') KF=3
      IF(FMT.EQ.'B8')  KF=3
      IF(KF.NE.0) THEN
         IFMT(3)=KF
         GOTO 20
      END IF

*     translate format

      IER=0
      LNF=LEN(FMT)
      LEV=0
      NR =0
*     loop over all characters
      DO 10 L=1,LNF
      FM=FMT(L:L)
      IF(FM.EQ.' ') GOTO 10
      NX=ICHAR(FM)-ICHAR('0')

*     a number

      IF(NX.GE.0.AND.NX.LE.9) THEN
         IF(NR.LT.0) IER=IER+1
         NR=NX+10*MAX0(0,NR)
         GOTO 10

*     a left parenthesis               NR/0

      ELSE IF(FM.EQ.'(') THEN
         IF(NR.LT.0) IER=IER+1
         LEV=LEV+1
         N=N+1
         IFMT(N)=10*MAX0(1,NR)
         NR=0
         GOTO 10

*     a right parenthesis               0/0

      ELSE IF(FM.EQ.')') THEN
         IF(NR.GE.0) IER=IER+1
         LEV=LEV-1
         N=N+1
         IFMT(N)=0

*     a komma

      ELSE IF(FM.EQ.',') THEN
         IF(NR.GE.0) IER=IER+1
         NR=0
         GOTO 10

*     a F or I or A or J or K OR Z     NR/1 or 2 or 3 or 4 or 5 or 6

      ELSE
         KF=0
         IF(FM.EQ.'F') KF=1
         IF(FM.EQ.'I') KF=2
         IF(FM.EQ.'A') KF=3
         IF(FM.EQ.'J') KF=4
         IF(FM.EQ.'K') KF=5
         IF(KF.NE.0) THEN
            IF(NR.LT.0) IER=IER+1
            N=N+1
            IFMT(N)=KF+10*MAX0(1,NR)
         ELSE
            IF(FM.NE.' ') IER=IER+1
         END IF
      END IF
      NR=-1
   10 CONTINUE
*     end of do-loop
      IF(LEV.NE.0) IER=IER+1
      IF(IER.NE.0) THEN
C        WRITE(6,101) FMT
         IEFMT=-1
         GOTO  30
      END IF
*
   20 CONTINUE
   30 NFMT=N
      IF(IEFMT.NE.0) NFMT=0
  100 RETURN
  101 FORMAT('0FORMAT ERROR'/
     +       14X,'CALL TRFMT(',A,')')
      END
