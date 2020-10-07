      SUBROUTINE WDROP(JW,ID)
C
C     USER ENTRY
C     DROP WORK BANKS WITH INDICES ID(1)...ID(NDIM)
C
#include "param.h"
#include "mparam.h"
      INTEGER JW(*),ID(*)
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WDROP')
      NDIM=MAX(1,JW(1))
      JW(1)=0
      JW(2)=0
C
      LD=LOCF(ID(1))-JW(LJW)
      DO 10 I=1,NDIM
      IF(ID(I).EQ.0) GOTO 10
C
C        DROP WORK BANK, IF INDEX VALID
C
         IF(ID(I).GT.JW(IWK).AND.ID(I).LT.JW(NJW).
     1   AND.JW(ID(I)-1).EQ.LD+I) THEN
            JW( NDW)=JW( NDW)+JW(ID(I)-2)
            JW(IGW)=MAX(JW(IGW),ID(I)+JW(ID(I)-2)-3)
            JW(ID(I))=-JW(ID(I)-2)
            ID(I)=0
            GOTO 10
         END IF
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) ID(I)
         CALL BABEND('WDROP')
   10 CONTINUE
C
C     TRY TO REGAIN SPACE OF DROPPED BANKS
C
      JD=JW(IWK)+3
   20 IF(JW(JD).LT.0) THEN
         JW( IWK)=JW( IWK)-JW(JD)
         JW( NDW)=JW( NDW)+JW(JD)
         JD    =JD-JW(JD)
         GOTO 20
      END IF
      IF(JW(NDW).EQ.0) JW(IGW)=0
C
  100 RETURN
 1001 FORMAT('0BOS--WDROP-  INDEX ID INCORRECT - STOP'/
     1 14X,'ID= ',I12)
      END
