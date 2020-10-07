*DK hctrgd
      SUBROUTINE HCTRGD
C --------------------------------------------
C
C! Implements the module FORM TUBE TRIGGER SIGNAL
C!
C!     Author     :G.Zito   86/05/21
C!      Modified by :G.Catanesi   88/01/20
C!                   F.Ranjard    89/02/10
C!
C!      Input bank  : HWHT  McHcTubeSignal
C!      Output bank : HWTR  McHcTubeTrigSignal
C!
C!
C!   -Called by :HCDIGI
C!   -Calls     :none
C -----------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcpara.h"
#include "hcnamc.h"
#include "hwhtjj.h"
#include "hwtrjj.h"
#include "hctrig.h"
#include "bmacro.h"
C ----------------------------------------------------------
      JHWHT = IW(NAHWHT)
      KHWHT = JHWHT + LMHLEN
      NHWHT = LROWS (JHWHT)
      IF (NHWHT.EQ.0) RETURN
      JHWTR = IW(NAHWTR)
      KHWTR = JHWTR + LMHLEN
C
C     Scan McHcTubeSignal
C
      IPREV = -1
      DO 10 I = 1, NHWHT
         IMOD = IW(KHWHT+1)/100000
         ILL = IW(KHWHT + 1)/1000
         ILL = MOD(ILL,100)
         IF(ILL.EQ.1)THEN
            IL = ILL
         ELSE
            IL = ILL/ 2+ 1
         ENDIF
         IF(IMOD .EQ. IPREV) THEN
            IF(IL .NE. ILPRE) THEN
               ILPRE = IL
               IW(KHWTR+1) = IW(KHWTR+1) + 1
            ENDIF
         ELSE
            IPREV = IMOD
            IM = MOD(IMOD,100)
            IP = IMOD/100
            IF(IP.EQ.LPBAR) IM = IM + LPHCTR/2
            IF(IP.EQ.LPECB) IM = IM + LPHCTF + LPHCTR/2
            KHWTR = JHWTR + LMHLEN + (IM-1) * LHWTRA
            IW(KHWTR+1) = 1
            ILPRE = IL
         ENDIF
         KHWHT= KHWHT + LHWHTA
   10 CONTINUE
C
      IF(FHCDB1)THEN
         WRITE(LOUTIO,500)
         KHWTR = JHWTR + LMHLEN
         WRITE (LOUTIO,510) (I,I=1,36)
         WRITE (LOUTIO,510) (IW(KHWTR+K),K=1,36)
      ENDIF
C
      RETURN
  500 FORMAT(/1X,'+++HCTRGD+++ HWTR McTubeTrigSignal (plane)')
  510 FORMAT (1X,36I3)
      END
#endif
