C*DK LKPRROW
C*DF UNIX
      SUBROUTINE LKPRROW (NAME,NUM,STTROW,ENDROW,CHNR)
C -----------------------------------------------------------------
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
C  If your bank has no format defined, use PRWORK instead
C
C - structure : SUBROUTINE subprogram
C               External References: BPRTAB
C                                    NAMIND, NLINK (BOS77)
C               Comdecks referenced: BCS, BMACRO
C
C - input   : NAME  = bank name (up to 4 char.)
C             NUM    =bank # (if = 0 all banks are printed)
C ---------------------------------------------------------------
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C
      CHARACTER*4 CHAINT,NAME2,CHNR*1
      INTEGER STTROW,ENDROW,FROM,LENTH,TOLOWER
      DATA TOLOWER/32/
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      CHARACTER NAME*(*)
      LOGICAL FTABL,FTABL2, FALL
C
C*CA BMACRO
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
C*CC BMACRO
      FTABL2(K) = (IW(K).EQ.(LCOLS(K)*LROWS(K)+2))
      FTABL(K) = (LCOLS(K).LT.256 .AND. LROWS(K).LT.50000)
C-----------------------------------------------------------------------
      JNAMI=NAMIND(NAME)
      JNAMI2=NAMIND(CHAINT(TOLOWER+INTCHA(NAME)))
      IF ((CHNR.EQ.' ').OR.(CHNR.EQ.'*')) THEN
          FALL = .TRUE.
C       take the bank with the lowest bank number
          IND=IW(JNAMI)
          IF (IND.EQ.0) THEN
C             look for compressed
              NAME2=CHAINT(TOLOWER+INTCHA(NAME))
              IND=IW(JNAMI2)
              IF (IND.EQ.0) GOTO 2
              NAME=NAME2
              JNAMI=JNAMI2
          ENDIF
    2     CONTINUE
      ELSE
          FALL = .FALSE.
          IND = NLINK (NAME,NUM)
C - test index
          IF (IND.EQ.0) THEN
C - if bank doesn't exist try to find compressed bank
              NAME=CHAINT(TOLOWER+INTCHA(NAME))
              JNAMI=JNAMI2
              IND = NLINK (NAME,NUM)
          ENDIF
      ENDIF
      IF (IND .EQ. 0) THEN
         WRITE(IW(6),*) '[Bank doesn''t exist]'
         GOTO 999
      ENDIF
C
C - print
 10   CONTINUE
      IF (IND.NE.0) THEN
         NU = IW(IND-2)
         IF (.NOT.FTABL(IND)) THEN
            WRITE(IW(6),*) '[Non-tabular bank]'
            CALL NPRNT(NAME,NU)
         ELSE IF (.NOT.FTABL2(IND)) THEN
            WRITE(IW(6),*) '[Non-tabular bank]'
            CALL NPRNT(NAME,NU)
         ELSE
            IF (STTROW.LE.1) THEN
               WRITE(IW(6),1000) NAME,NU,LCOLS(IND),LROWS(IND),IW(IND)
            ELSE
               WRITE(IW(6),1001) NAME,NU,LCOLS(IND),LROWS(IND),IW(IND),
     &               STTROW
            ENDIF
            ISR = STTROW
            IF (ISR .LT. 1) ISR = 1
            IER = ENDROW
            IF (IER.LT.1 .OR. IER.GT.LROWS(IND)) IER = LROWS(IND)
            IF (IER.GE.ISR) CALL BPRTAB(JNAMI,IND,ISR,IER)
         END IF
         IND = IW(IND-1)
         IF (FALL) GOTO 10
      ENDIF
 999  RETURN
 1000 FORMAT(/1X,'+++LKPRROW+++ Bank ',A4,1X,I6,' has ',I3,
     &  ' columns and ',I6,' rows and the total length is ',I6)
 1001 FORMAT(/1X,'+++LKPRROW+++ Bank ',A4,1X,I6,' has ',I3,
     &  ' columns and ',I6,' rows and the total length is ',I6,
     &  /1X,'Start printing from row ',I6)
      END
