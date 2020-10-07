*DK hcfopl
      SUBROUTINE HCFOPL
C--------------------------------------------------------------------
C
C!  Implements the module FORMAT PLANE SIGNAL
C!
C!          Author : G.Catanesi 03/02/1989
C!
C!          Input bank : HPHT
C!          Output bank: HPDI
C!
C!
C!        -Called by : HCDIGI
C!        -Calls     : SORTIQ,MVBITS from CERNLIB
C ------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "iocom.h"
#include "hcnamc.h"
#include "hphtjj.h"
#include "hpdijj.h"
#include "hccoun.h"
#include "bmacro.h"
C ------------------------------------------------------------
C
      JHPHT = IW(NAHPHT)
      IF (JHPHT.EQ.0) RETURN
      NHPHT = LROWS (JHPHT)
      IF (NHPHT.EQ.0) RETURN
C
C      Loop on Planes
C
      KHPHT = JHPHT +LMHLEN
C
      CALL SORTIQ (IW(KHPHT+1),LHPHTA,NHPHT,1)
C
      JHPDI = IW(NAHPDI)
      KHPDI = JHPDI + LMHLEN
C
      NHPDI = 0
      IPL1 = 0
      IMO1 = 0
      DO 10 I = 1, NHPHT
         N = IW(KHPHT+1)
         IPL = MOD(N,100)
         N = N / 100
         IMO = MOD(N,100)
         N = N / 100
         IPO = N
         NED = INT(RW(KHPHT+2)*1000.)
         IF(IMO.NE.IMO1.OR.IPL.NE.IPL1) THEN
            IMO1 = IMO
            IPL1 = IPL
            NHPDI = NHPDI + 1
            KHPDI = KROW(JHPDI,NHPDI)
            IPLM = IPL
            CALL MVBITS(IPLM,0,8,IW(KHPDI+JHPDPA),0)
            IMOM = IMO
            CALL MVBITS(IMOM,0,8,IW(KHPDI+JHPDPA),8)
C
            CALL MVBITS(IPO,0,8,IW(KHPDI+JHPDPA),16)
         ENDIF
C
            IW(KHPDI+JHPDED) = NED
         KHPHT = KHPHT + LHPHTA
   10 CONTINUE
C
      IW(JHPDI+LMHROW) = NHPDI
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) NHPDI
         KHPDI = JHPDI + LMHLEN
         DO 20 I = 1, NHPDI
            IPL = IBITS(IW(KHPDI+JHPDPA),0,8)
            IMO = IBITS(IW(KHPDI+JHPDPA),8,8)
            IPO = IBITS(IW(KHPDI+JHPDPA),16,8)
            IEPLA = IW(KHPDI+JHPDED)
            WRITE(LOUTIO,510)I,IPL,IMO,IPO,IEPLA
            KHPDI = KHPDI + LHPDIA
   20    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCFOPL+++ HPDI McHcPlanesDigitising ',I5/
     +' Plan#  Pla  Mod  Por     Energy ')
  510 FORMAT(4I5,I10)
      END
#endif
