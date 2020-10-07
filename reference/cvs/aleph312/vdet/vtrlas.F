      LOGICAL FUNCTION VTRLAS (DUM)
C----------------------------------------------------------------------
C!  - .TRUE. If VDET Laser Trigger
CKEY VDET LASER TRIGGER / USER
C!   Author   :- E. Lancon             30-JUN-1993
C!   modified : A.Wagner and F.Ranjard 12-APR-1995
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "xtbnjj.h"
      CHARACTER*4 MNM1,MNM2,CHAINT
      INTEGER AGETDB, ALTRIG, ALFIND
      SAVE INUM
      DATA NAXTBN, IRLAST /2*0/
C
#include "bmacro.h"
C---------------------------------------------------------------
C
      VTRLAS = .FALSE.
      IF (NAXTBN.EQ.0) NAXTBN = NAMIND('XTBN')
      CALL ABRUEV (IRUNRC,IEV)
C
C - new run ====================================
C
      IF (IRUNRC .NE. IRLAST) THEN
C      reset INUM to wrong value
         INUM = -1
         IRLAST = IRUNRC
C      IF XTBN is not on the run header list  OR
C         is not there THEN get it from DB
         IXTBN = ALFIND('C','XTBN')
         KXTBN  = IW(NAXTBN)
         IF (IXTBN.EQ.0 .OR. KXTBN.EQ.0) THEN
            IDUMM = AGETDB('XTBN',IRUNRC)
            KXTBN = IW(NAXTBN)
         ENDIF
         IF (KXTBN.NE.0) THEN
            DO I=1,LROWS(KXTBN)
C
C            is this trigger bit enabled?
C            exclude bit # 31
C
               IDEF=ITABL(KXTBN,I,JXTBTN)
               INUM=ITABL(KXTBN,I,JXTBTB)
C
C            For some of the early runs XTBN was corrupted
C
               IF(INUM.LT.0.OR.INUM.GT.31) GOTO 888
C
C            get mnemonic
C
               IF(IDEF.NE.0)THEN
                 MNM1=CHAINT(ITABL(KXTBN,I,JXTBBM))
                 MNM2=CHAINT(ITABL(KXTBN,I,JXTBBM+1))
                 IF(MNM1(1:4).EQ.'VDET'.AND.MNM2(1:4).EQ.'_LSR')GOTO 100
               ENDIF
            ENDDO
            GOTO 888
         ENDIF
C
      ENDIF
C
C - same run ==========================================
C
  100 CONTINUE

C?    IF trigger bit number and trigger bit pattern are OK
C     THEN compare trigger bit pattern with trigger bit number
C
      IF( INUM .GT. -1) THEN
        IF (ALTRIG(JLVL1,JLVL2,JLVL3).GT.0) VTRLAS = BTEST(JLVL3,INUM)
      ENDIF
C
C =====================================================
C
  888 RETURN
      END
#endif
