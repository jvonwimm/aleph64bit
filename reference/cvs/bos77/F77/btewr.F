      SUBROUTINE BTEWR(JW,LUN,LIST)

*     WRITE SET OF BANKS USING FORTRAN WRITE

      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      COMMON/STRSTR/JS,IAST,IBST,LUNS
      EXTERNAL STRPR
      CHARACTER*256 TXT
      CHARACTER*(*) LIST
      CHARACTER*4   NAME,CHAINT
C     ...
      JW1=JW(1)
      NEVW=0
      NBLK=0
      IBF=NLINK('+BUF',LUN)
      IF(IBF.NE.0) THEN

*     stop, if wrong unit

#include "unpackio.h"
         IF(IOMD.NE.2.OR.IOST.EQ.1) THEN
            IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) LUN
            CALL BABEND('BTEWR')
         END IF
         IF(IOST.EQ.2) GOTO 15
         IF(IW(IBF+2).NE.0) NBLK=IW(IBF+2)
      ELSE
         IBF=NBANK('+BUF',LUN,11)
      END IF
      IBF= NBANK('+BUF',LUN,11)
      IF(IBF.EQ.0) GOTO 201
      DO 12 M=1,10
   12 IW(IBF+M)=0
#include "unpackio.h"
      IOMD=2
      IOST=2
#include "packio.h"
      IF(NBLK.LE.0) NBLK=130
      IW(IBF+2)=NBLK

*     CHECK ZERO LIST

   15 NBLK=IW(IBF+2)
      IF(LEN(LIST).EQ.1) THEN
         IF(LIST.EQ.'0'.OR.LIST.EQ.'O') THEN
            GOTO 100
         END IF
      END IF

*     start line

      LUNS=LUN
      CALL CNVL(2,NBLK)
      CALL CNVT('* START OF TEXT-RECORD',STRPR)
      CALL STRPR
C
C     WRITE FORMAT BANKS
C
      IF(IW(IBF+9).NE.IEFMT) THEN
         IF(IEFMT.LT.0) THEN
            IF(JW(LUP).GT.0) WRITE(JW(LUP),1003)
            CALL BABEND('BTEWR')
         END IF
*        WRITE FORMAT BANKS
         IW(IBF+9)=IEFMT
      END IF
C
C     INIT LOOP ON BANKS
C
      IC=0
      ISTART=0
      ICOUNT=0
      IBH=0
#include "listnn.h"
      IF(NAMI.EQ.0) THEN
         IF(ICOUNT.EQ.0) THEN
            GOTO 100
         ELSE
            GOTO 80
         END IF
      END IF
C
C     IO-STATISTIC FOR THIS NAME
C
      IOA=NAMI-NSYST
      IF(JW(IOS).EQ.0.OR.JW(JW(IOS)).LT.2*IOA) THEN
C     IM NAECHSTEN STATEMENT OC4
         CALL WBANC(JW,JW(IOS),2*IOA+10,*201)
      END IF
      IF(JW(NAMI).NE.0) JW(JW(IOS)+2*IOA)=JW(JW(IOS)+2*IOA)+1
*     format for this name
      ID=IW(IDFMT+NAMI-NSYST)
      IF(ID.NE.0) THEN
         CALL TRFMTR(TXT,LFM,IW(ID+1),IW(ID))
         CALL CNVT(CHAINT(IW(IDNAM+NAMI-NSYST)),STRPR)
         CALL CNVT(' $FMT ',STRPR)
         CALL CNVT(TXT(1:LFM),STRPR)
         CALL STRPR
      END IF
C
C     INIT LOOP FOR THIS NAME
C
      IND=NAMI+1
   20 IND=JW(IND-1)
      IF(IND.EQ.0) GOTO 3
C
C      WRITE
C
      NW=JW(IND)
      ICOUNT=ICOUNT+NW+4
      NAME=CHAINT(JW(IND-3))
      CALL BTEWS(JW,-LUN,NAME,JW(IND-2))
      GOTO 20
C
C
*     write ENDQ
   80 CALL CNVT('ENDQ',STRPR)
      CALL STRPR
*     eventually drop the banks just written
      IF(JW1.EQ.1) THEN
         CALL BDROP(JW,LIST)
      END IF
      IW(IBF+7)=MAX0(IW(IBF+7),NEVW)
C
  100 JW(1)=0
      JW(2)=0
      RETURN
  201 CALL BBSPC(IW,'BTEWR')
      GOTO 100
 1001 FORMAT('0BOS--BTEWR-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
 1003 FORMAT('0BOS--BTEWR-  ERROR IN BANK FORMAT DEFINITION - STOP')
      END
