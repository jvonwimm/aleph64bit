      SUBROUTINE RVDPRE(IRET)
C----------------------------------------------------------------------
C!  - Steer VDET track preselection
C!
C!   Author   :- M.Cattaneo,D.Casper             5-MAR-1997
C!               Code originally in RECEVT as modified by D.Casper
C!               for tracking upgrade.
C!
C!   V0 and nuclear interaction pre-processing switched off with VNSL card
C!   YNKF card disables kinks
C!   YNLV card disables long V0's
C!   YNSV card disables secondary vertices
C!
C!  Output   IRET = 0      Event OK
C!                <>0      Event rejected
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
#include "frftjj.h"
#include "fxtrjj.h"
#include "yknkjj.h"
#include "ynvhjj.h"
      LOGICAL INIT

      SAVE INIT,NFRFT,NFXTR,NVNSL,NYNKF,NYNVH,NYNSV,NYNLV,NYKNK
#include "bmacro.h"
      DATA INIT/.TRUE./
C--------------------------------------------------------------------
C
      IF (INIT) THEN
        INIT = .FALSE.
        NFRFT = NAMIND('FRFT')
        NFXTR = NAMIND('FXTR')
        NVNSL = NAMIND('VNSL')
        NYNKF = NAMIND('YNKF')
        NYNVH = NAMIND('YNVH')
        NYNSV = NAMIND('YNSV')
        NYNLV = NAMIND('YNLV')
        NYKNK = NAMIND('YKNK')
      ENDIF

      IRET = 0
C
C     Find primary vertex for VDET track pre-selection
C
      IF (FDETRF(JULYT)) THEN
        CALL YTOPOL
      ELSE IF (FDETRF(JULYR)) THEN
        CALL YFMAIN
      ENDIF
C
C     Create FXTR bank for VDET track preselection
C
      KFRFT = IW(NFRFT)
      IF(KFRFT.NE.0) THEN
C     Drop FXTR if it already exists
        IF(IW(NFXTR).NE.0) CALL BDROP(IW,'FXTR')
        NTRK = LROWS(KFRFT)
        NWFXTR = NTRK*LFXTRA + LMHLEN
        CALL AUBOS('FXTR',0,NWFXTR,KFXTR,IGARB)
        CALL BLIST(IW,'T+','FXTR')
        IF(IGARB.NE.2)THEN
          IW(KFXTR+LMHCOL) = LFXTRA
          IW(KFXTR+LMHROW) = NTRK
          KVNSL = IW(NVNSL)
          IF(KVNSL.EQ.0)THEN
C      Identify V0s for VDET track preselection
            IF(IW(NYNLV).EQ.0) CALL FLNGV0
C      Identify pairs of tracks which make up a kink
            IF (IW(NYNKF).EQ.0) THEN
              CALL YKSRCH(.FALSE.)
C      Flag outgoing tracks from kinks in FXTR
              KFXTR = IW(NFXTR)
              KYKNK = IW(NYKNK)
              IF (KYKNK.GT.0) THEN
                DO IROW = 1, LROWS(KYKNK)
                  IYTK = ITABL(KYKNK,IROW,JYKNOT)
                  IF (IYTK.GT.0) IW(KROW(KFXTR,IYTK)+JFXTKN) = 1
                ENDDO
              ENDIF
            ENDIF
C      Identify material interactions for VDET track preselection
            IF (IW(NYNSV).EQ.0) THEN
              CALL YSVRTX(IERYNC)
              IF(IERYNC.EQ.0)THEN
                KYNVH = IW(NYNVH)
                KFXTR = IW(NFXTR)
                IF (KYNVH.NE.0)THEN
                  DO IROW = 1, LROWS(KYNVH)
                    IYTK = ITABL(KYNVH,IROW,JYNVTN)
                    IYVX = ITABL(KYNVH,IROW,JYNVVN)
                    IF (IYTK.GT.0 .AND. IYVX.GT.0)
     &                  IW(KROW(KFXTR,IYTK)+JFXTNC) = 1
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
