      SUBROUTINE BDAWR(JW,LUN,NAME,NR)
*     write bank to direct access data set
*     ignored if bank not existing
*             if data set not opened (BDABF)
*             if password wrong
*
*     ABEND IF SPACE ON DATA SET INSUFFICIENT
#include "dacom.h"
#include "param.h"
      COMMON/BCS/IW(1000)
      INTEGER JW(*)
      CHARACTER*4 NAME
      IDAW=NLINK('+DAW',LUN)
      IF(IDAW.NE.0) THEN
*        store BDAWR-request in bank '+DAW'
         NDAW=IW(IDAW+1)
         IW(IDAW+5*NDAW+2)=JW(13)-IW(13)+1
         IW(IDAW+5*NDAW+3)=INTCHA(NAME)
         IW(IDAW+5*NDAW+4)=NR
         IW(IDAW+5*NDAW+5)=0
         IW(IDAW+5*NDAW+6)=0
         IW(IDAW+1)=IW(IDAW+1)+1
         IF(IW(IDAW+1).GE.100) THEN
*           now write to da file
            CALL BDAWE(-LUN)
         END IF
         GOTO 100
      END IF
*     immediate write (no '+DAW' bank)
      INDA=MLINK(JW,NAME,NR)
      IF(INDA.EQ.0) GOTO 100
*     ignore if data set not opened
      IUB=NLINK('+BUF',LUN)
      IF(IUB.EQ.0) GOTO 100
*     password check
      IF(IW(IUB+18).EQ.0) GOTO 100
      LUNDA=LUN
      NWRDA=IW(IUB+11)
      NADA=INTCHA(NAME)
      NRDA=NR
      NDDA=JW(INDA)
*     reserve unit
      CALL DARES
*     ... write
      CALL BDAWI(JW)
      CALL DAFWR(0,1)
      CALL DAFWR(0,0)
*     free data set
      CALL DADES
  100 RETURN
      END
