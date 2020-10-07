      SUBROUTINE CONTENT(STRING,MODE)
C ------------------------------------------------------------------
C     go through file DOC:SBANK.LBF and find bank requested by string
C     if no wildcard used just print it
C     otherwise print also where booked and referenced
C ------------------------------------------------------------------
#ifndef DOC
#include "sbkpar.h"
#include "paging.h"
#include "fildat.h"
#include "sbkcom.h"
#if defined(UNIX)
#include "sbkloc.h"
#endif
      CHARACTER*4 BANK,STRING
      CHARACTER*80 INLINEM1,INLINEM2
C --------------------------------------------------------------------
#if defined(UNIX)
      OPEN (UNIT=LINP,FILE=ALDOCC(:ILDOCC)//'/sbank.lbf',STATUS='OLD')
#else
      OPEN (UNIT=LINP,FILE='DOC:SBANK.LBF',STATUS='OLD',READONLY)
#endif
      I=0
      IADD=-1
    5 I=I+1
      BANK=NAMARR(I)
      IF (LGT(STRING,NAMARR(I)).AND.I.LT.IADARR(0)) GOTO 5
      IEST=MODE-1
      IF(MODE.EQ.0)IEST=4
      BANK=NAMARR(I)
      IF(MODE.EQ.1)GOTO 21
      IF(BANK(1:IEST).NE.STRING(1:IEST))GOTO 17
   21 DO 10 J=1,IADARR(I)-3
   10 READ (LINP,*)
      READ (LINP,'(A)') INLINEM2
      READ (LINP,'(A)') INLINEM1
      READ (LINP,'(A)') INPLIN
      IF(XBOOKG.OR.XREFER)THEN
        WRITE(LOUT,2)
    2   FORMAT(
     &    ' ......................................................')
        CALL PAGE
      ENDIF
C     bank is found - print two lines before and current line
      WRITE(LOUT,'(A)')INLINEM2
      CALL PAGE
      WRITE(LOUT,'(A)')INLINEM1
      CALL PAGE
      WRITE(LOUT,'(A)')INPLIN
      CALL PAGE
C     go on until end of this bank doc. is found
   15 READ(LINP,'(A)',END=17)INPLIN
      IF(INPLIN(2:14).EQ.'ADAMO Toolset')GOTO 17
      IF(INPLIN(3:8).EQ.'======'.AND.MODE.EQ.0)GOTO 17
      IF(INPLIN(3:8).EQ.'======')THEN
        IF(XBOOKG)CALL GTBOOK(BANK)
        IF(XREFER)CALL GTREFER(BANK)
        GOTO 5
      ENDIF
C     get rid of blanks at end of line
      LEN=LENOCC(INPLIN)
      IF(LEN.LT.2)GOTO 15
      WRITE(LOUT,'(A)')INPLIN(1:LEN)
      CALL PAGE
      GOTO 15
   17 CONTINUE
      CLOSE (LINP)
      END
#endif
