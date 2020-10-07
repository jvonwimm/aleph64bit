*DK hcdigi
      SUBROUTINE HCDIGI
C-------------------------------------------------------
C
C! Implements the module CREATE HC DIGITISINGS
C!
C!     Author     : G.Zito     86/05/21
C!     Modified   : G.Catanesi 89/02/06
C!     Modified   : L.Silvestris 18/3/93
C!
C!     Input bank : HTHT  McHcStoreys
C!                  HWHT  McHcTubeSignal with induction
C!                  HPHT  McHcPlaneSignal
C!     Output bank :
C!                  HWDI McHcTubeDigitising
C!                  HTDI  McHcStackDigitising
C!                  HPDI  McHcPlaneDigitising
C!                  HWTR McHcPlaneTriggerSignal
C!                  HTTR McHcTowerTriggerSignal
C!
C!   -Called by : ASDIGI
C!   -Calls : HCTRGA, HCTRGD, HCFORA, HCFOPL, HCFORD from this .HLB
C!            BDROP,BLIST from BOS77.HLB
C!            ALBOS  from ALEPHLIB
C -------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "hcnamc.h"
#include "hwdijj.h"
#include "hwtrjj.h"
#include "htdijj.h"
#include "httrjj.h"
#include "hpdijj.h"
#include "hccoun.h"
#include "iocom.h"
#include "bmacro.h"
C
C          Check for old digitisings banks and drop them
C
      IDRP = IW(NAHWDI)
      IF (IDRP.NE.0) CALL BDROP (IW,'HTDIHWDIHTTRHWTRHPDI')
C
C       Book HxTR and HxDI banks and add them to 'E' list
C
      JHWHT = IW(NAHWHT)
      IF (JHWHT.EQ.0) RETURN
      NHWHT = LROWS (JHWHT)
      CALL ALBOS ('HWDI',0,NHWHT*LHWDIA+LMHLEN,JHWDI,IGARB)
      IW(JHWDI+1) = LHWDIA
      IW(JHWDI+2) = 0
      CALL ALBOS ('HWTR',0,NHWTR*LHWTRA+LMHLEN,JHWTR,IGARB)
      IW(JHWTR+1) = LHWTRA
      IW(JHWTR+2) = NHWTR
      CALL BLIST(IW,'E+','HWDIHWTR')
C
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) RETURN
      NHTHT = LROWS (JHTHT)
      CALL ALBOS ('HTDI',0,NHTHT*LHTDIA+LMHLEN,JHTDI,IGARB)
      IW(JHTDI+1) = LHTDIA
      IW(JHTDI+2) = 0
      CALL ALBOS ('HTTR',0,NHTTR*LHTTRA+LMHLEN,JHTTR,IGARB)
      IW(JHTTR+1) = LHTTRA
      IW(JHTTR+2) = NHTTR
      CALL BLIST(IW,'E+','HTDIHTTR')
C
C     Issue HC trigger signal
C
C     Form tower trigger signal: HTHT -> HTTR 'E' list
C
      CALL HCTRGA
C
C     Form tube trigger signal: HWHT -> HWTR 'E' list
C
      CALL HCTRGD
C
C
      JHPHT = IW(NAHPHT)
      IF (JHTHT.EQ.0) RETURN
      NHPHT = LROWS (JHPHT)
      CALL ALBOS ('HPDI',0,NHPHT*LHPDIA+LMHLEN,JHPDI,IGARB)
      IW(JHPDI+1) = LHPDIA
      IW(JHPDI+2) = 0
      CALL BLIST(IW,'E+','HPDI')
C
C       Format tower signal
C
      CALL HCFORA
C
C
C       Format plane signal
C
      CALL HCFOPL
C       Format tube signal
C
      CALL HCFORD
C
C - Compress output banks
C
      CALL AUBPRS ('HTDIHWDIHPDI')
C
      END
#endif
