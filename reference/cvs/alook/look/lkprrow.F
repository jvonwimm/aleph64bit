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
#ifndef DOC
#include "bcs.h"
C
      CHARACTER*4 CHAINT,NAME2,CHNR*1
      INTEGER STTROW,ENDROW,FROM,LENTH,TOLOWER
      EXTERNAL NAMIND,NLINK,NPRNT
      INTEGER  NAMIND,NLINK,NPRNT
      INTEGER IRET
      CHARACTER NAME*(*)
      LOGICAL FTABL,FTABL2, FALL
      DATA TOLOWER/32/
C
#include "bmacro.h"
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
            IRET = NPRNT(NAME,NU)
         ELSE IF (.NOT.FTABL2(IND)) THEN
            WRITE(IW(6),*) '[Non-tabular bank]'
            IRET = NPRNT(NAME,NU)
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
#endif
