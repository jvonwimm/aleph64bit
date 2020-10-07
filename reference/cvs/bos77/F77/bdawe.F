      SUBROUTINE BDAWE(ILUN)
*
*     da write of bank names/nr in bank +DAW
*
#include "dacom.h"
#include "param.h"
      COMMON/BCS/IW(1000)
      CHARACTER*4 CHAINT
      LUN=IABS(ILUN)
      IDAW=NLINK('+DAW',LUN)
      IDAW=NPRNT('+DAW',LUN)
      IF(IDAW.EQ.0) GOTO 100
      NDAW=IW(IDAW+1)
      IF(NDAW.EQ.0) GOTO 90
*     ignore if data set not opened
      IUB=NLINK('+BUF',LUN)
      IF(IUB.EQ.0) GOTO  90
*     password check
      IF(IW(IUB+18).EQ.0) GOTO  90
*     Loop over the banks
      DO 10 I=1,NDAW
      JWND=IW(IDAW+5*I+2-5)
      NADA=IW(IDAW+5*I+3-5)
      NRDA=IW(IDAW+5*I+4-5)
      INDA=MLINK(IW(JWND),CHAINT(NADA),NRDA)
      IW(IDAW+5*I+5-5)=INDA
      IF(INDA.EQ.0) GOTO 10
*     hash index for direct access
      NHDA=MOD(IABS(NADA),IW(IUB+15))
      IF(NRDA.GE.0) THEN
         NHDA=NHDA+NRDA/100
      ELSE
         NHDA=NHDA-(-NRDA-1)/100
         IF(NHDA.LT.0) NHDA=NHDA+(IW(IUB+15)*(1-NHDA/IW(IUB+15)))
      END IF
      IW(IDAW+5*I+6-5)=IW(IUB+12)-MOD(NHDA,IW(IUB+15))
   10 CONTINUE
      CALL BPRNT(IW,'+DAW')
*     sort according to dir-record number
      CALL SORTAI(IW(IDAW+2),5,NDAW,5)
      CALL BPRNT(IW,'+DAW')
*     reserve unit
      LUNDA=LUN
      CALL DARES
      NWRDA=IW(IUB+11)
*     loop to write
      DO 20 I=1,NDAW
      JWND=IW(IDAW+5*I+2-5)
      NADA=IW(IDAW+5*I+3-5)
      NRDA=IW(IDAW+5*I+4-5)
      INDA=IW(IDAW+5*I+5-5)
      IF(INDA.EQ.0) GOTO 20
      NDDA=IW(JWND+INDA-1)
      CALL BDAWI(IW(JWND))
   20 CONTINUE
      CALL DAFWR(0,1)
      CALL DAFWR(0,0)
*     free data set
      CALL DADES
*     drop '+DAW' bank
   90 IF(ILUN.GT.0) IND=NDROP('+DAW',LUN)
  100 RETURN
      END
