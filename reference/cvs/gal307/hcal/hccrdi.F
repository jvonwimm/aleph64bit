*DK hccrdi
      SUBROUTINE HCCRDI
C --------------------------------------------
C
C!   Implements the module CREATE DIGITAL PATTERN
C!
C!         Author     : G.Zito 86/05/21
C!
C!         Input bank : JDHCHI  McHcHits
C!                      JDHCSE  McHcTubesSegment
C!         Output bank : HWHT  McHcTubeSignal
C!
C!   -Called by :HCASIG
C!   -Calls     :none
C-------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hchijj.h"
#include "hcsejj.h"
#include "hwhtjj.h"
#include "bmacro.h"
C
C      Loop on Hits
C
      NHCHI = LROWS (JDHCHI)
      IF (NHCHI.EQ.0) RETURN
      KHCHI = JDHCHI + LMHLEN
      JHWHT = IW(NAHWHT)
      KHWHT = JHWHT + LMHLEN
      IPREV = 0
      NHWHT = 0
C
      DO 20 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         KHCSE = KROW (JDHCSE,NSEG)
         IF (IW(KHCSE+5) .EQ. IPREV) GOTO 10
         NHWHT = NHWHT + 1
         IW(KHWHT+1)=IW(KHCSE+5)
         IPREV = IW(KHCSE+5)
         KHWHT = KHWHT + LHWHTA
   10    IW(KHCSE+JHCSFT) = NHWHT
         KHCHI = KHCHI + LHCHIA
   20 CONTINUE
      IW(JHWHT +2) = NHWHT
C
C
      IF(FHCDEB)THEN
         KHWHT = JHWHT + LMHLEN
         WRITE(LOUTIO,500) NHWHT
         WRITE (LOUTIO,510) (IW(KHWHT+I),I=1,NHWHT*LHWHTA,LHWHTA)
      ENDIF
C
      RETURN
  500 FORMAT (1X,'+++HCCRDI+++ HWHT  McHcTubeSignal',I5)
  510 FORMAT (1X,10I10)
      END
#endif
