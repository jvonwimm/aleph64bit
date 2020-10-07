      PROGRAM HACPAR
C --------------------------------------------------------------
C! create DOC:BOSCOMCOM
CKEY HAC PARAMETER
C     Reads file DOC:SBANK.LBF and writes file DOC:BOS.COMCOM
C     which contains the HAC parameter description
C     F.Jacot  16/03/92 Modified for ADAMO 3.2
C --------------------------------------------------------------
#ifndef DOC
#include "sbkloc.h"
#include "sbkpar.h"
      CHARACTER*4   BANK
      CHARACTER*6   CDNAME
      CHARACTER*30  NAME,LACOM
      CHARACTER*2   REP
      CHARACTER*50  COMMNT
      CHARACTER*80  INPUTLINE,INLIN2,SAVLIN,BLANK
      CHARACTER*200 COMLIN,OUTLINE
      DATA BLANK /'                             '/
      DATA LACOM/'The number of columns in bank '/
C ---------------------------------------------------------------
C - Open input file
#if defined(UNIX)
#include "gtaldoc.h"
      OPEN (UNIT=LINP,FILE=ALDOCC(:ILDOCC)//'/sbank.lbf',STATUS='OLD')
#else
      OPEN (UNIT=LINP,FILE='DOC:SBANK.LBF',STATUS='OLD',READONLY)
#endif
C - Open output file
#if defined(UNIX)
      OPEN (UNIT=LINP2,FILE=ALDOCC(:ILDOCC)//'/bos.comcom',STATUS='NEW')
#else
      OPEN (UNIT=LINP2,FILE='DOC:BOS.COMCOM',STATUS='NEW')
#endif
C
C- set ADAMO offset depending on ADAMO version # 32
C
      JJ=12
      NC1=2
      NC2=10
      NC3=13
      NC4=18
      NBL=6
      NC5=7
      NC6=10
C
C - read input file
C
 10   READ(LINP,'(A)',END=17)INPUTLINE
      IF(INPUTLINE(2:2).NE.'|') GOTO 10
      BANK=INPUTLINE(4:7)
      IDCOUNT=0
      SAVLIN=' '
C
 11   READ(LINP,'(A)',END=17)INPUTLINE
      IF (IDCOUNT.LE.0.AND.INPUTLINE(4:7).EQ.BANK) GOTO 12
      IF (IDCOUNT.GE.0.AND.INPUTLINE(2:2).EQ.'.') THEN
          IDCOUNT=IDCOUNT+1
          IF (IDCOUNT.GE.2) GOTO 12
      ENDIF
      IF (INPUTLINE(2:5).EQ.'====') GOTO 10
      GOTO 11
C
 12   CDNAME=BANK//'JJ'
      IF (IDCOUNT.LE.0) THEN
          NAME='J'//BANK(1:3)//'@@'//INPUTLINE(10:17)
          COMLIN=INPUTLINE(18:80)
          CALL ADJLIN(COMLIN)
          GOTO 18
      ENDIF
      READ(LINP,'(A)',END=17)INPUTLINE
C
 13   IF (INPUTLINE(2:2).EQ.'.') GOTO 16
      REP=INPUTLINE(JJ:JJ+1)
      NAME='J'//BANK(1:3)//REP//' = '//INPUTLINE(NC1:NC2)
      COMLIN=' '
      SAVLIN=INPUTLINE

 15   READ(LINP,'(A)',END=17)INLIN2
      IF (INLIN2(1:NC3).NE.' ') THEN
          IF (COMLIN.EQ.' ') COMLIN=INPUTLINE(NC4:80)
          INPUTLINE=INLIN2
      ELSE
          COMLIN=COMLIN(1:120)//INLIN2
      ENDIF
      CALL ADJLIN(COMLIN)
      IF (INPUTLINE.NE.INLIN2) GOTO 15

 18   COMMNT=COMLIN(1:50)
      I=LNBLNK(COMMNT)
      OUTLINE=' '//NAME//CDNAME//' =>'//COMMNT
      WRITE (UNIT=LINP2,FMT='(A)') OUTLINE(1:40+I)
      IF (IDCOUNT.GT.0) THEN
          GOTO 13
      ELSE
          IDCOUNT=-1
          GOTO 11
      ENDIF

 16   NAME='L'//BANK//'A ='//BLANK(1:NBL)//SAVLIN(NC5:NC6)
      CDNAME=BANK//'JJ'
      OUTLINE=' '//NAME//CDNAME//' =>'//LACOM//BANK
      I=LNBLNK(OUTLINE)
      WRITE (UNIT=LINP2,FMT='(A)') OUTLINE(1:I)
      GOTO 10

 17   CONTINUE
      CLOSE (LINP)
      CLOSE (LINP2)
      END
#endif
