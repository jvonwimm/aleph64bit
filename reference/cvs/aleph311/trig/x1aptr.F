      SUBROUTINE X1APTR
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C. - Modified : C. Geweniger - 88/10/11  for GALEPH 20.1
C. - Modified : E. Blucher - 89/15/2 for ALEPHLIB
C. - Modified : C.Geweniger - 890900 for ALEPHLIB 9.9
C. - Modified : B. Bloch-Devaux - 92/12/10 for Sical in 92
C.
C! - Apply the level1 trigger
C.
C? - The different level1 triggers are simulated using the information
C?   prepared in XT1SUM.
C.
C.
C. - Called by      X1TRIG                        from this .HLB
C. - Banks     : XTEB is filled
C.
#ifndef DOC
      SAVE
C.
#include "bcs.h"
#include "x1tcom.h"
#include "xtrcom.h"
#include "x1namc.h"
#include "xtebjj.h"
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
C -   Apply single muon trigger (SNG_MUON)
C.
C -   Coincidence for each trigger segment of
C.                                            It-Tracks-Thresholds1
C.                                            Hc-Wires-Thresholds3
C.                                            Hc-Segments-Thresholds1
C.
C.
      NTRBIT = 0
      ITRG11 = IAND(IAND(NITBIT(2),NHWBIT(6)),NHTBIT(2))
      ITRG12 = IAND(IAND(NITBIT(1),NHWBIT(5)),NHTBIT(1))
      ITRI1 = IOR(ITRG11,ITRG12)
      IF (ITRI1.NE.0) NTRBIT=IBSET(NTRBIT,0)
      IF (ITRI1.NE.0) NPHYTR(1) = NPHYTR(1) + 1
C.
C.
C -   Apply single charged el.-mag. energy trigger (SNG_C_EM)
C.
C -   Coincidence for each trigger segment of
C.                                            It-Tracks-Thresholds1
C.                                            Ec-Wires-Thresholds2
C.                                            Ec-Segments-Thresholds3
C.
C.
      ITRG21 = IAND(IAND(NITBIT(2),NEWBIT(4)),NETBIT(6))
      ITRG22 = IAND(IAND(NITBIT(1),NEWBIT(3)),NETBIT(5))
      ITRI2 = IOR(ITRG21,ITRG22)
      IF (ITRI2.NE.0) NTRBIT=IBSET(NTRBIT,1)
      IF (ITRI2.NE.0) NPHYTR(2) = NPHYTR(2) + 1
C.
C.
C -   Apply single neutral el.-mag. energy trigger (SNG_N_EM)
C.
C -   Coincidence for each trigger segment of
C.                                            Ec-Wires-Thresholds4
C.                                            Ec-Segments-Thresholds4
C.
C.
      ITRG31 = IAND(NEWBIT(8),NETBIT(8))
      ITRG32 = IAND(NEWBIT(7),NETBIT(7))
      ITRI3 = IOR(ITRG31,ITRG32)
      IF (ITRI3.NE.0) NTRBIT=IBSET(NTRBIT,2)
      IF (ITRI3.NE.0) NPHYTR(3) = NPHYTR(3) + 1
C.
C.
C -   Apply single photon trigger
C     (Not implemented in the present trigger system.)
C.
C -   Coincidence for each trigger segment of
C.                                            Ec-Wires-Thresholds1
C.                                            Ec-Segments-Thresholds1
C.
C.
      ITRG41 = IAND(NEWBIT(2),NETBIT(2))
      ITRG42 = IAND(NEWBIT(1),NETBIT(1))
      ITRI4 = IOR(ITRG41,ITRG42)
      IF (ITRI4.EQ.0) GO TO 333
C.
C.
C. - Now apply the VETO conditions for this trigger
C.
C. -
C.                                            It-Track-Thresholds1
C.                                            Lc-Segments-Thresholds1
C.
      IF (NLTBIT(1).NE.0.OR.NLTBIT(2).NE.0) GO TO 333
      IF (NITBIT(1).NE.0.OR.NITBIT(2).NE.0) GO TO 333
CCC      NTRBIT=IBSET(NTRBIT,3)
CCC      NPHYTR(4) = NPHYTR(4) + 1
      GO TO 334
 333  CONTINUE
      ITRG41 = 0
      ITRG42 = 0
      ITRI4 = 0
 334  CONTINUE
C.
C.
C -   Apply single charged hadr. energy trigger (SNG_C_HA)
C.
C -   Coincidence for each trigger segment of
C.                                            It-Tracks-Thresholds1
C.                                            Hc-Wires-Thresholds1
C.                                            Hc-Segments-Thresholds2
C.
C.
      ITRG51 = IAND(IAND(NITBIT(2),NHWBIT(2)),NHTBIT(4))
      ITRG52 = IAND(IAND(NITBIT(1),NHWBIT(1)),NHTBIT(3))
      ITRI5 = IOR(ITRG51,ITRG52)
      IF (ITRI5.NE.0) NTRBIT=IBSET(NTRBIT,4)
      IF (ITRI5.NE.0) NPHYTR(5) = NPHYTR(5) + 1
C.
C.
C -   Apply single neutral hadr. energy trigger (SNG_N_HA)
C.
C -   Coincidence for each trigger segment of
C.                                            Hc-Wires-Thresholds2
C.                                            Hc-Segments-Thresholds3
C.
C.
      ITRG61 = IAND(NHWBIT(4),NHTBIT(6))
      ITRG62 = IAND(NHWBIT(3),NHTBIT(5))
      ITRI6 = IOR(ITRG61,ITRG62)
      IF (ITRI6.NE.0) NTRBIT=IBSET(NTRBIT,5)
      IF (ITRI6.NE.0) NPHYTR(6) = NPHYTR(6) + 1
C.
C.
C -   Apply total energy trigger
C.
C -   Or of
C.    Ec-Wires-Total-Energy, Barrel,              Thresholds1 (ETT_EWBA)
C.    Ec-Wires-Total-Energy, EndcapA.and.EndcapB, Thresholds1 (ETT_EWE*)
C.    Ec-Wires-Total-Energy, EndcapA,             Thresholds2 (ETT_EWEA)
C.    Ec-Wires-Total-Energy, EndcapB,             Thresholds2 (ETT_EWEB)
C.
C.
      ITRG72 = 0
      ITRG71 = 0
      IF (BTEST(NTEBIT,10).AND.BTEST(NTEBIT,14)) ITRG71=IBSET(ITRG71,17)
      IF (BTEST(NTEBIT, 8).AND.BTEST(NTEBIT,12) .AND.
     &    BTEST(NTEBIT, 9).AND.BTEST(NTEBIT,13)) ITRG71=IBSET(ITRG71,20)
      IF (BTEST(NTEBIT,24).AND.BTEST(NTEBIT,28)) ITRG71=IBSET(ITRG71,24)
      IF (BTEST(NTEBIT,25).AND.BTEST(NTEBIT,29)) ITRG71=IBSET(ITRG71,25)
      IF (ITRG71.NE.0) NTRBIT=IBSET(NTRBIT,6)
      IF (ITRG71.NE.0) NPHYTR(7) = NPHYTR(7) + 1
C.
C.
C -   Apply Bhabha trigger  LCAL
C.
C -   Grand or of
C -   or of all supersegments                   Thresholds2   (LC_VHIGH)
C -   or of SideA low  .AND. or of SideB low    Thresholds3   (LC_LO_LO)
C -   or of SideA low threshold                 Thresholds3   (LC_A_LOW)
C -   or of SideB low threshold                 Thresholds3   (LC_B_LOW)
C -   or of SideA high threshold                Thresholds4   (LC_A_HIG)
C -   or of SideB high threshold                Thresholds4   (LC_B_HIG)
C -   or of SideA low  .AND. or of SideB high   Thresholds3&4 (LC_LO_HI)
C -   or of SideA high .AND. or of SideB low    Thresholds3&4 (LC_HI_LO)
C.
C.
      ITRG81 = 0
      ITRG82 = 0
      LTA3 = IAND(MASKA,NLTBIT(3)) .NE. 0
      LTA4 = IAND(MASKA,NLTBIT(4)) .NE. 0
      LTB3 = IAND(MASKB,NLTBIT(3)) .NE. 0
      LTB4 = IAND(MASKB,NLTBIT(4)) .NE. 0
      IF (NLTBIT(2).NE.0)  ITRG81=IBSET(ITRG81, 0)
      IF (LTA3 .AND. LTB3) ITRG81=IBSET(ITRG81, 3)
      IF (LTA3)            ITRG81=IBSET(ITRG81, 4)
      IF (LTB3)            ITRG81=IBSET(ITRG81, 5)
      IF (LTA4)            ITRG81=IBSET(ITRG81, 6)
      IF (LTB4)            ITRG81=IBSET(ITRG81, 7)
      IF (LTA3 .AND. LTB4) ITRG81=IBSET(ITRG81,14)
      IF (LTA4 .AND. LTB3) ITRG81=IBSET(ITRG81,15)
      IF (ITRG81.NE.0) NTRBIT=IBSET(NTRBIT,7)
      IF (ITRG81.NE.0) NPHYTR(8) = NPHYTR(8) + 1
C.
C.
C -   Apply Bhabha trigger  SICAL
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
      IF (MT2.NE.0) NTRBIT=IBSET(NTRBIT,8 )
      IF (MT2.NE.0) NPHYTR(9) = NPHYTR(9) + 1
C.
C.
C -  Fill Level 1 Trigger Bit Word in bank XTEB
C    This should be updated for the different year setups !!!!
C
      KXTEB=IW(NAXTEB)+LMHLEN
      MT1=IOR(ITRG71,ITRG81)
      CALL MVBITS(NTRBIT,2,1,MT1,2)
      CALL MVBITS(NTRBIT,0,2,MT1,8)
      CALL MVBITS(NTRBIT,4,2,MT1,11)
      IW(KXTEB+JXTET1)=MT1
C
C
      IF (NTRBIT.NE.0) NACCL1 = NACCL1 + 1
      IF (NTRBIT.NE.0) NACCTR = NACCTR + 1
C.
      RETURN
      END
#endif
