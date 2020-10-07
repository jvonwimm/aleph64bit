      SUBROUTINE BDADIR(LUN,NAME,IPOS)
C -------------------------------------------------------------
C - F.Ranjard - 900501  extracted from BDAPR
C               920629  replace NA by IW(INA) in the last call
C                       to INLIST because NA is local and not saved
*     Returns the position in the directory of bank
*     with NAME name .
*     input arguments  : LUN  / I  : D.A. logical unit number
*                        NAME / A  : bank name
*     output argument  : IPOS / I  : >0 bank NAME exists
*                                    =0 no bank with such name
*
      CHARACTER*4 NAME,CHAINT
#include "dacom.h"
#include "param.h"
      COMMON/BCS/IW(1000)
      INTEGER NJR(100)
      DATA IFI /0/
C ---------------------------------------------------------------
      IF(IW(LUP).LE.0) GOTO 100
      IUB=NLINK('+BUF',LUN)
      IF(IUB.EQ.0) GOTO 100
      LUNDA=LUN
      NWRDA=IW(IUB+11)
C
C - 1st entry: build table of all names
C
      IF(IFI.EQ.1) GOTO 50
      IFI = 1
C
      CALL WBANK(IW,INA,100,*100)
      NA=0
      NALA=INTCHA(NAME)
      DO 40 J=1,IW(IUB+15)
      IREC=IW(IUB+12)-J+1
   10 CALL DAFRD(IREC,0)
      NTOT=IW(IDR)
      DO 30 L=8,NTOT,5
      IF(IW(IDR+L-3).EQ.NALA) GOTO 30
      NACU=IW(IDR+L-3)
      NALA=NACU
      I=INLIST(NACU,IW(INA+1),NA)
      IF(I.LT.0) THEN
         I=-I
         DO 20 K=NA,I,-1
   20    IW(INA+K+1)=IW(INA+K)
         IW(INA+I)=NACU
         NA=NA+1
         IF(NA.EQ.IW(INA)) THEN
            CALL WBANK(IW,INA,NA+100,*100)
         END IF
      END IF
   30 CONTINUE
      IREC=IW(IDR+3)
      IF(IREC.NE.0) GOTO 10
   40 CONTINUE
      CALL WBANK(IW,INA,NA,*100)
C
*      WRITE(IW(LUP),101)
*      WRITE(IW(LUP),102) (CHAINT(IW(INA+I)),I=1,NA)
*  101 FORMAT('0SORTED LIST OF ALL NAMES'/)
*  102 FORMAT(1X,20A6)
C
   50 CONTINUE
C
C - LOOK FOR A BANK
C
      NACU = INTCHA (NAME)
      IPOS = INLIST (NACU,IW(INA+1),IW(INA))
C
 100  CONTINUE
      END
