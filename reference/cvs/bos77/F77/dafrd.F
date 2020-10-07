      SUBROUTINE DAFRD(IREC,ITYPE)

*     FORTRAN READ FOR DIRECTORY RECORD (ITYPE=0) OR DATA RECORD

#include "dacom.h"
      COMMON/BCS/IW(1000)
#include "param.h"
      IUREC=LUNDA+100*IREC
*     IUREC = current-record identifier
*           = LUN + 100 * recordnumber
*     IDRR  = identifier of record in directory workbank
*           = 0 no record  > 0 record unmodified
*                          < 0 record modified
*     IDDR  = identifier of record in data workbank
*           = 0 no record  > 0 record unmodified
*                          < 0 record modified
      IF(ITYPE.EQ.0) THEN
*        DIRECTORY RECORD
*        CHECK WORK BANK
         IF(IDR.NE.0) THEN
            IF(IW(IDR-2)-4.GE.NWRDA) GOTO 10
         END IF
         CALL WBANK(IW,IDR,NWRDA,*99)

*        WORK BANK ALREADY CREATED IN OPENDA - USE COMMON WORD
*        INSTEAD OF IW(IDR+NWRDA) NOTE NWRDA MAY CHANGE
         IF(IDR.EQ.0) GOTO 99
         IW(IDR+NWRDA)=0
*        CHECK RECORD IN BUFFER
   10 CONTINUE
C        WRITE(6,'(''  READ DIR'',2I6)')  IDRR,IUREC
         IF(IUREC.EQ.IABS(IDRR)) GOTO 100
         IF(IDRR.LT.0) THEN
            JREC=-IDRR/100
            CALL DAFWR(JREC,ITYPE)
         END IF
*        READ (UNIT=LUNDA,REC=IREC,ERR=90) (IW(IDR+I-1),I=1,NWRDA)
         CALL DAFRDS(LUNDA,IREC,IW(IDR),NWRDA,*90)
C        WRITE(6,'(''  READ DIR'',I6)')  IUREC
         IW(IUB+3)=IW(IUB+3)+1
#include "unlock.h"
         IDRR=IUREC
      ELSE
*        DATA RECORD
*        CHECK WORK BANK
         IF(IDD.NE.0) THEN
            IF(IW(IDD-2)-4.GE.NWRDA) GOTO 20
         END IF
         CALL WBANK(IW,IDD,NWRDA,*99)
         IF(IDD.EQ.0) GOTO 99
         IW(IDD+NWRDA)=0
*        CHECK RECORD IN BUFFER
   20 CONTINUE
C        WRITE(6,'(''  READ DAT'',2I6)')  IDDR,IUREC
         IF(IUREC.EQ.IABS(IDDR)) GOTO 100
         IF(IDDR.LT.0) THEN
            JREC=-IDDR/100
            CALL DAFWR(JREC,ITYPE)
         END IF
*        READ (UNIT=LUNDA,REC=IREC,ERR=90) (IW(IDD+I-1),I=1,NWRDA)
         CALL DAFRDS(LUNDA,IREC,IW(IDD),NWRDA,*90)
C        WRITE(6,'(''  READ DAT'',I6)')  IUREC
         IW(IUB+3)=IW(IUB+3)+1
#include "unlock.h"
         IDDR=IUREC
      END IF
      GOTO 100
   90 IF(IW(LUP).EQ.0) GOTO 99
      WRITE(IW(LUP),1001) LUNDA
   99 CALL BABEND('DAFRD')
 1001 FORMAT('0---------- DA-UNIT',I4,' READ ERROR - STOP')
  100 RETURN
      END
