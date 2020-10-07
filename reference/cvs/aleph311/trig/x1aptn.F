      SUBROUTINE X1APTN
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C. - Modified                97/04/02  (Trigger : SNG_N_EL)
C.
C! - Apply the Level1 trigger
C.
C? - The different Level1 triggers are simulated using the information
C?   prepared in X1DISN
C.
C.
C. - Bank      : X1RG is filled (only row TPR
C --------------------------------------------------------------------
#ifndef DOC
      SAVE
C.
#include "bcs.h"
#include "x1tsto.h"
#include "xtrcom.h"
#include "x1namc.h"
#include "x1rgjj.h"
C.
C ----------------------------------------------------------------------
C
      LOGICAL BTEST
      LOGICAL LTA3,LTA4,LTB3,LTB4
C     MASKA= FFF      , MASKB= FFF000
C     MASKA=2**12 - 1 , MASKB=2**24 - 2**12
      DATA MASKA/4095/, MASKB/16773120/
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
C.
C -   Single Muon trigger (SNG_MUON)
C.
C -   Coincidence for each trigger segment of
C.                                            It-Tracks-Thresholds1
C.                                            Hc-Wires-Thresholds3
C.
C.
      NTRBIT = 0
      ITRG11 = IAND(NITBIT(2),NHWBIT(6))
      ITRG12 = IAND(NITBIT(1),NHWBIT(5))
      ITRI1 = IOR(ITRG11,ITRG12)
      IF (ITRI1.NE.0) NTRBIT=IBSET(NTRBIT,8)
      IF (ITRI1.NE.0) NPHYTR(1) = NPHYTR(1) + 1
C.
C.
C -   Single Charged El.-mag. energy trigger (SNG_C_EM)
C.
C -   Coincidence for each trigger segment of
C.                                            It-Tracks-Thresholds1
C.                                            Ec-Wires-Thresholds1
C.
C.
      ITRG21 = IAND(NITBIT(2),NEWBIT(2))
      ITRG22 = IAND(NITBIT(1),NEWBIT(1))
      ITRI2 = IOR(ITRG21,ITRG22)
      IF (ITRI2.NE.0) NTRBIT=IBSET(NTRBIT,9)
      IF (ITRI2.NE.0) NPHYTR(2) = NPHYTR(2) + 1
C.
C.
C -   Single Neutral El.-mag. energy         (SNG_N_EL)
C.
C.                                            Ec-Wires-Thresholds3
C.
C.
      ITRG31 = NEWBIT(6)
      ITRG32 = NEWBIT(5)
      ITRI3 = IOR(ITRG31,ITRG32)
      IF (ITRI3.NE.0) NTRBIT=IBSET(NTRBIT,2)
      IF (ITRI3.NE.0) NPHYTR(3) = NPHYTR(3) + 1
C.
C.
C -   Total energy ECW barrel
C.
C.                                        Ec-Wires-Total-Energy1 (Barrel)
C.
C.
      ITRG42 = 0
      ITRG41 = 0
      IF (BTEST(NTEBIT,2)) ITRG41=IBSET(ITRG41,0)
      IF (ITRG41.NE.0) NTRBIT=IBSET(NTRBIT,17)
      IF (ITRG41.NE.0) NPHYTR(4) = NPHYTR(4) + 1
C.
C.
C -   Total energy ECW Endcap_A
C.
C.                                        Ec-Wires-Total-Energy2 (Endcap_A)
C.
C.
      ITRG52 = 0
      ITRG51 = 0
      IF (BTEST(NTEBIT,4)) ITRG51=IBSET(ITRG51,0)
      IF (ITRG51.NE.0) NTRBIT=IBSET(NTRBIT,18)
      IF (ITRG51.NE.0) NPHYTR(5) = NPHYTR(5) + 1
C.
C -   Total energy ECW Endcap_B
C.
C.                                        Ec-Wires-Total-Energy2 (Endcap_B)
C.
C.
      ITRG62 = 0
      ITRG61 = 0
      IF (BTEST(NTEBIT,5)) ITRG61=IBSET(ITRG61,0)
      IF (ITRG61.NE.0) NTRBIT=IBSET(NTRBIT,19)
      IF (ITRG61.NE.0) NPHYTR(6) = NPHYTR(6) + 1
C.
C.
C -   Total energy ECW Endcap_A*Endcap_B
C.
C.                                        Ec-Wires-Total-Energy1 (EC_A*EC_B)
C.
C.
      ITRG72 = 0
      ITRG71 = 0
      IF (BTEST(NTEBIT,0).AND.BTEST(NTEBIT,1)) ITRG71=IBSET(ITRG71,0)
      IF (ITRG71.NE.0) NTRBIT=IBSET(NTRBIT,20)
      IF (ITRG71.NE.0) NPHYTR(7) = NPHYTR(7) + 1
C.
C.
C.
C -   LCAL Wires A*B*SUM (Bhabha)
C.
C.
      ITRG81 = 0
      ITRG82 = 0
      IF (BTEST(NLWBIT,1).AND.BTEST(NLWBIT,5).AND.BTEST(NLWBIT,3))
     *   ITRG81=IBSET(ITRG81,0)
      IF (ITRG81.NE.0) NTRBIT=IBSET(NTRBIT,6)
      IF (ITRG81.NE.0) NPHYTR(8) = NPHYTR(8) + 1
C.
C.
C -   Bhabha trigger  SICAL
C.
C -   Grand or of
C -         SideA Verylow threshold            Threshold 1   (SI_A_VLO)
C -         SideB Verylow threshold            Threshold 1   (SI_B_VLO)
C -         SideA Veryhigh threshold           Threshold 4   (SI_A_VHI)
C -         SideB Veryhigh threshold           Threshold 4   (SI_B_VHI)
C -         SideA low  .AND.       SideB high  Threshold 2&3 (SI_LO_HI)
C -         SideA high .AND.       SideB low   Threshold 2&3 (SI_HI_LO)
      ITRG91 = 0
      ITRG92 = 0
      MT2 = 0
      KSIX2 = IW(NASIX2)
      IF ( KSIX2.GT.0) THEN
         ITWORD = IW ( KSIX2+LMHLEN+1)
         IF ( ITWORD.NE.0) THEN
C  A VERY LOW    OR   B VERY LOW
            IF ((IBITS(ITWORD,5,1).NE.0).OR.(IBITS(ITWORD,4,1).NE.0))
     $    MT2 = IBSET(MT2,0)
C  A HIGH B LOW  OR   A LOW  B HIGH
            IF ((IBITS(ITWORD,0,1).NE.0).OR.(IBITS(ITWORD,1,1).NE.0))
     $    MT2 = IBSET(MT2,1)
C  A VERY HIGH   OR   B VERY HIGH
            IF ((IBITS(ITWORD,2,1).NE.0).OR.(IBITS(ITWORD,3,1).NE.0))
     $    MT2 = IBSET(MT2,2)
         ENDIF
      ENDIF
      ITRG91 = ITWORD
      IF (MT2.NE.0) NTRBIT=IBSET(NTRBIT,4 )
      IF (MT2.NE.0) NPHYTR(9) = NPHYTR(9) + 1
C.
C.
C -  Fill Level1  Trigger Bit Word in bank X1RG
C
C
      KX1RG = IW(NAX1RG)+LMHLEN
      IW(KX1RG+JX1RNA) = INTCHA('TPR ')
      IW(KX1RG+JX1RCO)   = NTRBIT
      IW(KX1RG+JX1RCO+1) = NTRBIT
      IW(KX1RG+JX1RCO+2) = NTRBIT
C
C
      IF (NTRBIT.NE.0) NACCL1 = NACCL1 + 1
      IF (NTRBIT.NE.0) NACCTR = NACCTR + 1
C.
      RETURN
      END
#endif

