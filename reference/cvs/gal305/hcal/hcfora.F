*DK hcfora
      SUBROUTINE HCFORA
C--------------------------------------------------------------------
C
C!  Implements the module FORMAT TOWER SIGNAL
C!
C!          Author : G.Zito  86/05/21
C!          mod. by: G.Catanesi, F.Ranjard - 890522
C!                   1 stack or 2 stacks
C!          Input bank : HTHT  McHcStoreys
C!          Output bank: HTDI  McHcTowerDigitising
C!
C!
C!        -Called by : HCDIGI
C!        -Calls     : SORTMI,MVBITS from CERNLIB
C-------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hthtjj.h"
#include "htdijj.h"
#include "hcgega.h"
#include "bmacro.h"
C ------------------------------------------------------------
C
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) RETURN
      NHTHT = LROWS (JHTHT)
      IF (NHTHT.EQ.0) RETURN
C
C      Loop on Storeys
C
      KHTHT = JHTHT +LMHLEN
C
      JHTDI = IW(NAHTDI)
      KHTDI = JHTDI + LMHLEN
C
      NHTDI = 0
      ITH1 = 0
      IPH1 = 0
      DO 10 I = 1, NHTHT
         N = IW(KHTHT+1)
         IST = MOD(N,100)
         N = N / 100
         ITH = MOD(N,100)
         N = N / 100
         IPH = N
         NED = INT(RW(KHTHT+4)*1000.)
         IF(ITH.NE.ITH1.OR.IPH.NE.IPH1) THEN
            ITH1 = ITH
            IPH1 = IPH
            NHTDI = NHTDI + 1
            KHTDI = KROW(JHTDI,NHTDI)
            IPHM = IPH
            CALL MVBITS(IPHM,0,16,IW(KHTDI+1),0)
            ITHM = ITH
            CALL MVBITS(ITHM,0,8,IW(KHTDI+1),16)
            IRE = IHCREG(ITH)
            CALL MVBITS(IRE,0,8,IW(KHTDI+1),24)
         ENDIF
         IST = MIN(IST,LHTDIA-1)
         IW(KHTDI+1+IST) = IW(KHTDI+1+IST) + NED
         KHTHT = KHTHT + LHTHTA
   10 CONTINUE
C
      IW(JHTDI+LMHROW) = NHTDI
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) NHTDI
         KHTDI = JHTDI + LMHLEN
         DO 20 I = 1, NHTDI
            IPH = IBITS(IW(KHTDI+1),0,16)
            ITH = IBITS(IW(KHTDI+1),16,8)
            IRE = IBITS(IW(KHTDI+1),24,8)
            WRITE(LOUTIO,510)I,IPH,ITH,IRE,(IW(KHTDI+J),J=JHTDED,LHTDIA)
            KHTDI = KHTDI + LHTDIA
   20    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCFORA+++ HTDI McHcTowerDigitising ',I5/
     +'      row#       Phi     Theta    Region    Estack')
  510 FORMAT(10I10)
      END
#endif
