      SUBROUTINE UNDOC(STRING,MODE)
C ----------------------------------------------------------
C
C     find and print banks that are used in the programs
C      but which are undocumented
C ----------------------------------------------------------
#ifndef DOC
#include "paging.h"
#include "sbkpar.h"
#include "fildat.h"
#if defined(UNIX)
#include "sbkloc.h"
#endif
      CHARACTER*4 BANK,BBANK,DBANK(NBNK),RBANK(NBNK),STRING
C -----------------------------------------------------------
      WRITE(LOUT,2)
    2 FORMAT(' ......................................................')
      CALL PAGE
      WRITE(LOUT,3)STRING
    3 FORMAT(' Banks not documented:',A)
      CALL PAGE
      WRITE(LOUT,4)
    4 FORMAT(' =========================')
      CALL PAGE
C
C       first collect all the names of documented banks
C
#if defined(UNIX)
      OPEN (UNIT=LINP,FILE=ALDOCC(:ILDOCC)//'/sbank.lbf',STATUS='OLD')
#else
      OPEN (UNIT=LINP,FILE='DOC:SBANK.LBF',STATUS='OLD',READONLY)
#endif
   10 READ(LINP,'(A)',END=17)INPLIN
      IF(INPLIN(2:2).EQ.'|')THEN
      BANK=INPLIN(4:7)
      NDBANK=NDBANK+1
      DBANK(NDBANK)=BANK
      ENDIF
      GOTO 10
   17 CONTINUE
C
C         now we look for used banks
C
      NRBANK=0
#if defined(UNIX)
      OPEN (UNIT=LINP2,FILE=ALDOCC(:ILDOCC)//'/all.assign',STATUS='OLD')
#else
      OPEN (UNIT=LINP2,FILE='DOC:ALL.ASSIGN',STATUS='OLD',READONLY)
#endif
  300 READ(LINP2,'(A)',END=500)INPLIN
      IF(INDEX(INPLIN,'CALLA').EQ.0.AND.
     +       INDEX(INPLIN,'NBANK(').EQ.0.AND.
     +       INDEX(INPLIN,'MBANK(').EQ.0 )GOTO 300
      I=INDEX(INPLIN,'''')
      IF(I.EQ.0)GOTO 300
      J=INDEX(INPLIN(I+5:I+5),'''')
      IF(J.EQ.0)GOTO 300
      BBANK=INPLIN(I+1:I+4)
      IF(MODE.EQ.1)GOTO 302
      IEST=MODE-1
      IF(MODE.EQ.0)IEST=4
      IF(BBANK(1:IEST).NE.STRING(1:IEST))GOTO 300
  302 CONTINUE
C
C         do not do the same bank twice
C
      DO 305 J=1,NRBANK
        IF(RBANK(J).EQ.BBANK)GOTO 300
  305 CONTINUE
      NRBANK=NRBANK+1
      RBANK(NRBANK)=BBANK
C
C          check if bank is documented
C
      DO 310 J=1,NDBANK
        IF(DBANK(J).EQ.BBANK) GOTO 300
  310 CONTINUE
C
C        it is not documented
C
      WRITE(LOUT,311)BBANK,INPLIN(1:25)
  311 FORMAT(' Bank ',A,' Booked in ',A,'  is not documented!!!')
      CALL PAGE
      GOTO 300
  500 CONTINUE
      CLOSE(LINP)
      CLOSE(LINP2)
      END
#endif
