      INTEGER FUNCTION ALSTGQRY( FILENM )
C-------------------------------------------------------------
CKEY ALREAD STAGE QUERY / USER
C! Queries whether a file on an Aleph tape has been staged
C   Author   :- M. Cattaneo    VAX       20-MAR-1995
C               R. Hagelberg   UNIX
C               F. Ranjard
C==============================================================
C
C   Returned value  : 1 if staged file exists, 0 if it does not
C   Inputs    : C* FILENM      i.e.  ab1234.1.sl
C   Outputs   : None.
C
C=============================================================
      INTEGER SYSTEM
c      EXTERNAL SYSTEM
      CHARACTER*(*) FILENM
      CHARACTER     CHTAPE*16, CHOPT*80, MSG*120, LABEL*3
C - internal commom block to the package KERNLIB M432
      COMMON /SLATE/ ND432,NE432,NF432,NG432,DUM432(36)
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      alstgqry = 0
      LF  = LNBLNK(FILENM)
      JTAPS  = ICNEXT (FILENM,1,LF)
      CHTAPE = FILENM(JTAPS:NE432-1)
C      CALL CUTOL (CHTAPE)
      LABEL = '.al'
      IF (CHTAPE(1:2).EQ.'as') LABEL='.al'
      LT = LNBLNK(CHTAPE)
      I1P = INDEX(CHTAPE,'.')
      IF (I1P.EQ.0) THEN
         CHTAPE = CHTAPE(1:LT)//'.1'//LABEL
         I1P = LT+1
         LT = LNBLNK(CHTAPE)
      ENDIF
      I2P = INDEX(CHTAPE(I1P+1:LT),'.') +I1P
      IF (I2P.EQ.I1P) THEN
         CHTAPE = CHTAPE(1:LT)//LABEL
         I2P = LT+1
         LT = LT+3
      ENDIF
C
C      CHOPT = 'stageqry -V '// CHTAPE(1:I1P-1) //' -q '//
C     &        CHTAPE(I1P+1:I2P-1) //' | wc -l'
      CHOPT = 'stageqry -M '// CHTAPE //'| wc -l '
      LOPT = LNBLNK(CHOPT)
      MSG = 'x=`'//CHOPT(1:LOPT)//'` ; exit $x'
      IER = SYSTEM (MSG) / 256
C      IF (IER.EQ.0) THEN
C         CHTAPE(I1P:I1P)='_'
C         MSG = 'jojoalws:al$data:'//CHTAPE(1:LT)
C         LMSG = LNBLNK(MSG)
C         CHOPT = 'rfstat '''//MSG(1:LMSG)//''' | wc -l'
C         LOPT = LNBLNK(CHOPT)
C         MSG = 'x=`'//CHOPT(1:LOPT)//'` ; exit $x'
C         IER = SYSTEM (MSG) / 256
C      ENDIF
      IF (IER.NE.0) ALSTGQRY = 1

  999 RETURN
      END
