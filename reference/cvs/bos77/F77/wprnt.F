      SUBROUTINE WPRNT(JW,ID)

*     print bank (work bank or named bank)

      EXTERNAL MLINK
#include "boscom.h"
#include "param.h"
      CHARACTER*4 NAME,CHAINT, STP*13
      INTEGER JW(*),ID(1)
      INTEGER LW(30)
      EQUIVALENCE (LW(1),INTA(1))
      DATA STP/' '/
C     ...
      IF(JW(ICK).NE.12345) THEN
         CALL BBNOT(JW,'WPRNT')
*        FOLLOWING STATEMENT TO MAKE COMPILER HAPPY (NEVER EXECUTED)
         IND=MLINK(JW,NAME,0)
      END IF

      NDIM=MAX(1,JW(1))
      JW(1)=0
      JW(2)=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
      DO 20 I=1,NDIM
      IF(ID(I).LE.0) GOTO 20
      IF(ID(I).LT.JW(IWK)) THEN

*        named bank


         IND= MLINX(JW,JW(ID(I)-3),JW(ID(I)-2))
    5    IF(ID(I).NE.IND) GOTO 20
         ND=JW(ID(I))
         CALL PNVI(JW(ID(I)-2),STP(2:13),JSP)
         NAME=CHAINT(JW(ID(I)-3))
         WRITE(JW(LUP),1001) NAME       ,STP,ND,ID(I),
     1                      JW(ID(I)-1)
         NAMI=NAMIND(NAME)
         IFMT=IW(IDFMT+NAMI-NSYST)
         IF(IFMT.EQ.0) GOTO 10
C
C        FORMAT FOR THIS BANK IS DEFINED
C
         IF(IW(IFMT+3).EQ.0) THEN
C
C           FORMAT CODES
C
            DO 30 L = 4,IW(IFMT)
            MODA=MOD(IW(IFMT+L),10)
            IF(MODA.NE.3.AND.MODA.NE.0) GOTO 40
   30       CONTINUE
*           only text - use text format
            WRITE(JW(LUP),1007) (CHAINT(JW(ID(I)+L)),L=1,JW(ID(I)))
            GOTO 50
*           use format description
   40       CALL FPRNN(JW(ID(I)+1),JW(ID(I)),IW(IFMT+1),IW(IFMT))
   50       CONTINUE
         ELSE
C
C           FORMAT IS B32 OR B16 OR B8
C
            IFMT=IW(IFMT+3)
            N1=1
            N3=10*IFMT
            IF(IFMT.EQ.3) IFMT=4
            NWRD=IFMT*JW(ID(I))
            IF(NWRD.EQ.0) GOTO 15
    2       N3=MIN(N3,NWRD-N1+1)
               NP=MOD(N1+N3-1,1000)
            IF(IFMT.EQ.1) THEN
               CALL BLO32W(JW(ID(I)+1),2*(N1-1)+1,LW,1,N3)
               WRITE(JW(LUP),1003) N1,NP,(LW(L),L=1,N3)
            ELSE IF(IFMT.EQ.2) THEN
               CALL BLO16W(JW(ID(I)+1),N1,LW,1,N3)
               WRITE(JW(LUP),1004) N1,NP,(LW(L),L=1,N3)
            ELSE
               CALL BLO8W (JW(ID(I)+1),N1,LW,1,N3)
               WRITE(JW(LUP),1005) N1,NP,(LW(L),L=1,N3)
            END IF
            N1=N1+N3
            IF(N1.LT.NWRD) GOTO 2
         END IF
         GOTO 15
      ELSE IF(ID(I).LE.JW(NJW)) THEN
C
C        WORK BANK
C
         ND=JW(ID(I))
         WRITE(JW(LUP),1002) CHAINT(JW(ID(I)-3)),ND,ID(I),JW(ID(I)-2)
         GOTO 10
      END IF
      GOTO 20
   10 CALL BBUFM(JW(ID(I)+1),1,ND,JW(LUP))
   15 IF(JW(LUB).EQ.1) WRITE(JW(LUP),1006)
      JW(LUB)=JW(LUB)-1
   20 CONTINUE
C
  100 RETURN
 1001 FORMAT('0M/NPRNT----  NAMED BANK ',A4,A13,'WITH',I6,
     1   ' DATA WORDS AT INDEX',I6,' (INDEX TO NEXT BANK =',I6,')'/)
 1002 FORMAT('0WPRNT------  WORK BANK ',A4,10X,' WITH',I6,' DATA WORDS',
     1   ' AT INDEX',I6,' (TOTAL LENGTH       =',I6,')'/)
 1003 FORMAT(1X,I5,'-',I3,1X,10I12)
 1004 FORMAT(1X,I5,'-',I3,1X,20I 6)
 1005 FORMAT(1X,I5,'-',I3,1X,30I 4)
 1006 FORMAT('0----------- PRINT LIMIT FOR BANK PRINTOUT REACHED')
 1007 FORMAT(30X,18A4)
      END
