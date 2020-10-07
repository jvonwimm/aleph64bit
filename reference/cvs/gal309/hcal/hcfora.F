*DK hcfora
      SUBROUTINE HCFORA
C--------------------------------------------------------------------
C
C!  Implements the module FORMAT TOWER SIGNAL
C!
C!          Author : G.Zito  86/05/21
C!          mod. by: G.Catanesi, F.Ranjard - 890522
C!                   1 stack or 2 stacks
C!          mod. by: A.Sciaba`, A.Venturi -980702
C!                   thresholds and half towers calib
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
#include "hcalct.h"
#include "hctwth.h"
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
C    Loop on HTDI to check the energy of the half tower and to apply the
C    threshold
      ITHR=210
      KHTDI=JHTDI + LMHLEN
      DO 15  I=1,NHTDI
        IPH=IBITS(IW(KHTDI+1),0,16)
        ITH=IBITS(IW(KHTDI+1),16,8)
        IOV=0
        CALL HCMODN(ITH,IPH,ISUB,IMD1,IMD2)
        CALIB = HCALTO(1,IMD1)
        CALIE = HCALTO(2,IMD2)
        IF(ITH.GE.IFHFTH.AND.ITH.LE.ILHFTH) IOV=1
        IF(63-ITH.GE.IFHFTH.AND.63-ITH.LE.ILHFTH) IOV=2
        IF(IOV.EQ.0) THEN
          IF(ISUB.EQ.1) THEN
             IF(IW(KHTDI+2).LE.ITHR*CALIB)
     +          IW(KHTDI+2)=-MAX(ABS(IW(KHTDI+2)),1)
          ENDIF
          IF(ISUB.EQ.3) THEN
             IF(IW(KHTDI+2).LE.ITHR*CALIE)
     +          IW(KHTDI+2)=-MAX(ABS(IW(KHTDI+2)),1)
          ENDIF
        ELSEIF(IOV.NE.0) THEN
          IF(IOV.EQ.1) LTH=ITH-IFHFTH+1
          IF(IOV.EQ.2) LTH=63-ITH-IFHFTH+1
C
C   Consistency check
          ETOT=ENHFTW(LTH,IPH,IOV,1)+ENHFTW(LTH,IPH,IOV,2)
          IETOT=INT(ETOT*1000)
          IF((IETOT-IW(KHTDI+2)).GT.1) THEN 
            WRITE(LOUTIO,*) 'HCFORA: PROBLEMS WITH HALF TOWERS'
            WRITE(LOUTIO,*) ITH,IPH,IOV,LTH,ETOT,IETOT,IW(KHTDI+2),
     +                      (ENHFTW(LTH,IPH,IOV,JJ),JJ=1,2)
          ENDIF
C   Threshold and half tower miscalibration
          IF(1000*ENHFTW(LTH,IPH,IOV,1).LE.ITHR*CALIB
     +       .AND.1000*ENHFTW(LTH,IPH,IOV,2).LE.
     +       ITHR*CALIE) THEN
             IW(KHTDI+2)=-MAX(ABS(IW(KHTDI+2)),1)
          ELSEIF(1000*ENHFTW(LTH,IPH,IOV,1).LE.ITHR*
     +          CALIB) THEN
             IW(KHTDI+2)=INT(1000*ENHFTW(LTH,IPH,IOV,2)*
     +          CALIB/CALIE)
          ELSEIF(1000*ENHFTW(LTH,IPH,IOV,2).LE.ITHR*
     +          CALIE) THEN
             IW(KHTDI+2)=INT(1000*ENHFTW(LTH,IPH,IOV,1))
          ELSE
             IW(KHTDI+2)=INT(1000*(ENHFTW(LTH,IPH,IOV,1)+
     +          ENHFTW(LTH,IPH,IOV,2)*CALIB/CALIE))
          ENDIF
        ENDIF
C     Apply Monte Carlo calibration constants
        IF(ISUB.EQ.1.OR.ISUB.EQ.2) THEN
           IW(KHTDI+2) = INT(IW(KHTDI+2)/CALMCB(IMD1))
        ELSEIF(ISUB.EQ.3) THEN
           IW(KHTDI+2) = INT(IW(KHTDI+2)/CALMCE(IMD2))
        ENDIF          
        KHTDI=KHTDI+LHTDIA
 15   CONTINUE
C
C     Remove towers below the threshold
      CALL UBPACK('HTDI',0,JHTDED,0,-1,IER)
      IF(IER.EQ.1) 
     +   WRITE(LOUTIO,*) 'HCFORA: ERROR IN PACKING HTDI'
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
