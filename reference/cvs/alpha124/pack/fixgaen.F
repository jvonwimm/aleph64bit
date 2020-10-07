      SUBROUTINE FIXGAEN
C ---------------------------------------------------------------------
C - Author: M.N Minard   940121   modified 970805
C!    Fix Gampek photon
C     Called from QFILL
C-    For all processing JULIA before ALEPHLIB 156
C-    uses Gain correction either from ECMC or Bank file
C-    for processing of minis before alephlib 156 , db 187
C-    uses correction routine to photon energy  CORAOLD/CORANEW
C-    The correction is done on the ALPHA GAT section
C ---------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "rhahjj.h"
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      CHARACTER*4 NAMJUL
      CHARACTER*8 PRNAM
      COMMON / ECCMF / ECCM(36) ,NRMIN,NRMAX
      DATA ILVERMX / 157 /
      DATA IDBVERX / 187 /
      DATA ILVER2X / 203 /
      DATA IDBVE2X / 194 /
      DATA ALVERC, ICVERC /2*0/
#include "qmacro.h"
      JR(I) = KOQVEC + ILGAT * KCQVEC + I
      JQDET(I,N) = KOQDET + (KFFRD+I-1) * KCQDET + N
C ---------------------------------------------------------------------
C-    Check database , alephlib version
C
      KRHAH = IW(NARHAH)
      NRHAH=LROWS(KRHAH)
      IMINI = 0
      IJULI = 0
      DO 10 IRHAH = 1, NRHAH
        PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
        PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
        IF (PRNAM.EQ.'MINI ') THEN
          IMINI = 1
          IDVER = ITABL(KRHAH,IRHAH,JRHADV)
          ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
        ENDIF
        IF(PRNAM.EQ.'JULIA') THEN
          IJULI = 1
          IDVER = ITABL(KRHAH,IRHAH,JRHADV)
          ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
        ENDIF
 10   CONTINUE
      IF (IMINI.EQ.0) THEN
C
C-    REPG card only active on POT 
C
        IF(IW(NAMIND('REPG')).NE.0.OR.IW(NAMIND('REGA')).NE.0) THEN
          IF (ALVERC .EQ. 0) CALL ALVERS(ALVERC)
          IF (ICVERC .EQ. 0) CALL ADBVER(ICVERC,IDVERC)
          IDVER = ICVERC
          ILVER = NINT(10.*ALVERC)
       ENDIF
      ENDIF
      CALL ECGATI(KRUN)
C
C-   Determine if correction have to be applied
C
      IF(ILVER.GE.ILVER2X.OR.IDVER.GE.IDBVE2X) GO TO 999
C
C-    Correct each photon 
C
      DO ILGAT = KFGAT,KLGAT
C
C-    Loop on Gampex photon
C
      EN = QP(ILGAT)
      PH = QPH(ILGAT)
      TH = ACOS(QCT(ILGAT))
C
C-    Determine Module number
C
       IREG=2
       IF(COS(TH) .LT. -0.72) IREG=3
       IF(COS(TH) .GT.  0.72) IREG=1
C
       FIDEG = PH*180./PI
       IF(FIDEG .LT. 0.)FIDEG=FIDEG+360.
       IF(IREG .EQ. 1 .OR. IREG .EQ. 3) THEN
          FIGAC = FIDEG+16.875
          IF(FIGAC .GT. 360.) FIGAC=FIGAC-360.
          MODUX = AINT(FIGAC/30.)+1
       ENDIF
       IF(IREG .EQ. 2 ) THEN
          FIGAC=FIDEG+1.875
          IF(FIGAC .GT. 360.) FIGAC=FIGAC-360.
          MODUX = AINT(FIGAC/30.)+1
       ENDIF
       MODRE = MODUX+(IREG-1)*12
       IF(MODRE.GT.36) MODRE=36
C
C-     Calculate correction
C
       CORF = ECCM(MODRE)
       CORA =1.
       IF (ILVER.LT.ILVERMX.AND.IDVER.LT.IDBVERX) THEN
       CORA = CORANEW(EN,TH,PH,KRUN)*CORAOLD(EN,TH,KRUN)
       ENDIF
       IF (ILVER.LT.ILVER2X.AND.IDVER.LT.IDBVE2X) THEN
         IF (ILVER.GE.ILVERMX.AND.IDVER.GE.IDBVERX) THEN
         CORA = CORANEW(EN,TH,PH,KRUN)*CORAOL2(EN,TH,PH,KRUN)
         ENDIF
       ENDIF
       CORF = CORF*CORA
C
C-     Rescale Gat section px,py,pz,p,e
C
        PX = RW(JR(JQVEQX))
        PY = RW(JR(JQVEQY))
        PZ = RW(JR(JQVEQZ))
        PN = RW(JR(JQVEQP))
        RW(JR(JQVEQX)) = PX*CORF
        RW(JR(JQVEQY)) = PY*CORF
        RW(JR(JQVEQZ)) = PZ*CORF
        RW(JR(JQVEQP)) = PN* CORF
        RW(JR(JQVEQE)) = PN*CORF
       ENDDO
C
 999   RETURN
       END
#endif

