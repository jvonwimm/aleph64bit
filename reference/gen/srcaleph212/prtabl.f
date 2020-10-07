      SUBROUTINE PRTABL (NAME,NUM)
C.......................................................................
C Author : S. Orteu       24-NOV-1986
C
C!Prints tabular banks in a pretty format.
C  the routine assumes that the banks has a miniheader of 2 words:
C  the # of columns, the # of rows.
C  if the # of columns is > 1000 , the bank is assumed not to be of
C  tabular format. In this case the routine prints a message and
C  returns.
C  in the other case , it uses a modified version of BPRNT (BOS77)
C  to print 1 row at a time.
C
C - structure : SUBROUTINE subprogram
C               User Entry Names: PRTABL
C               External References: BPRTAB
C                                    NAMIND, NLINK (BOS77)
C               Comdecks referenced: BCS, BMACRO
C
C - usage   : CALL PRTABL (NAME,NUM)
C - input   : NAME  = bank name (up to 4 char.)
C             NUM    =bank # (if = 0 all banks are printed)
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      CHARACTER NAME*(*)
      LOGICAL FTABL
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      FTABL(K) =LCOLS(K).LT.256 .AND. LROWS(K).LT.50000 .AND.
     &          IW(K).EQ.LMHLEN+LCOLS(K)*LROWS(K)
C-----------------------------------------------------------------------
C - get the name-index and the bank index
      JNAMI = NAMIND (NAME)
      IF (NUM .EQ. 0) THEN
         IND = IW(JNAMI)
      ELSE
         IND = NLINK (NAME,NUM)
      ENDIF
C - test index
      IF (IND .EQ. 0) THEN
         WRITE(IW(6),*)  ' +++PRTABL+++ ',NAME,' NR=',NUM,
     &                   ' Bank doesn''t exist'
         GOTO 999
      ENDIF
C
C - print
 10   CONTINUE
      IF (IND.NE.0) THEN
         NR = IW(IND-2)
         IF (FTABL(IND)) THEN
            WRITE(IW(6),1000) NAME,NR,LCOLS(IND),LROWS(IND),IW(IND)
            CALL BPRTAB(JNAMI,IND,1,LROWS(IND))
         ELSE
            WRITE(IW(6),*) ' +++PRTABL+++ ',NAME,' NR=',NR,
     &                     ' is not tabular '
            JPR = NPRNT (NAME,NR)
         END IF
         IND = IW(IND-1)
         IF (NUM.EQ.0) GOTO 10
      ENDIF
 999  RETURN
 1000 FORMAT(/1X,'+++PRTABL+++ Bank ',A4,1X,I6,' has ',I3,
     &  ' columns and ',I6,' rows and the total length is ',I6)
      END
