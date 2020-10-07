CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZOM
CH
      SUBROUTINE DQZOM(TMOD,TA,AS,HH,VV,BMODE,NYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                   16-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     TMOD(MODE) MOD1DZ MOD2DZ
C      RS   Rubber band Select view before with cursor
C      RB   Execute Rubber band
C      NS   1     0      0    Normal rectangle (Macintosh) or square
C      NR   2     0      0    Normal rectangle (Macintosh) or square
C      US   3     1      0    Uncentered square
C      CS   4     1      1    Centered   square
C      UR   5     2      0    Uncentered rectangle free aspect ratio
C      CR   6     2      1    Centered rectangle with fixed aspect ratio
C      HP   7     3      0    Horizontal parallelogram
C      CP   8     3      1    Centered   horizontal parallelogram ??
C      UV   9     4      0    Vertical   parallelogram
C      CV  10     4      1    Centered   vertical parallelogram ??
C                  C      BR  Rotate base
C                  C      BI  Base inversed
C Rubber Band   Undo
C     RB         RU
C  Box is Cleared, redrawn, Fixed, New box is redrawn
C            BC      BX       BF      BN
C      COMMON/XZOOM/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
C      LOGICAL FSTRDZ,FDUM,FB
      LOGICAL FDUM
      DIMENSION H(6),V(6),HH(4),VV(4)
      DIMENSION HLU(5),VLU(5),HLD(5),VLD(5)
      CHARACTER *2 TMOD(10),TA
      INTEGER BUTSTA
      COMMON /ASTCM1/ BUTSTA
      NYES=0
      CALL DO_STR('RB"CB"FB')
      CALL DO_STR_LIST(10,TMOD,'rubber modes')
      IF(TA.EQ.'RB') GO TO 10
      DO K=1,10
         IF(TA.EQ.TMOD(K)) THEN
            BMODE=K
            NYES=2
            IZOMDO=1
            RETURN
         END IF
      END DO
      IF(TA.EQ.'CB') THEN
         CALL DGZOOM(0,0,0,0)
         NYES=1
         RETURN
      END IF
C      IF(TA.EQ.'LB') THEN
C         CALL DGZOOM(4,0,0,0)
C         NYES=1
C         RETURN
C      END IF
      IF(TA.EQ.'FB') THEN
         CALL DGZOOM(5,0,0,0)
         NYES=1
         RETURN
      END IF
C      IF(TA.EQ.'UR') THEN
C         CALL UCOPY(HLU,HH,4)
C         CALL UCOPY(VLU,VV,4)
C         NYES=2
C         RETURN
C      END IF
      NYES=0
      RETURN
   10 MOD1DZ=BMODE
      CALL UCOPY(HH,HLU,4)
      CALL UCOPY(VV,VLU,4)
      NWINDZ=-99
      FSTRDZ=.TRUE.
      IF(AS.EQ.0.) THEN
         QASRDZ=0.5
      ELSE
         QASRDZ=0.5/AS
      END IF
      CALL DGZOOM(1,0,H,V)
      IF(BUTSTA.LT.0) THEN
C  Rubberband request terminated before completion.
         BUTSTA=0
         NYES=1
         RETURN
      ENDIF
C      IF(NWINDZ.LT.0) THEN
C         NYES=-1
C         RETURN
C      END IF
      RD=(H(3)-H(1))**2+(V(3)-V(1))
      IF(RD.LE.99.) THEN
         RD=(H(4)-H(2))**2+(V(4)-V(2))
         IF(RD.LE.99.) THEN
            NYES=-1
            RETURN
         END IF
      END IF
      HLD(1)=H(1)
      HLD(2)=H(2)
      HLD(3)=H(3)
      HLD(4)=H(4)
      VLD(1)=V(1)
      VLD(2)=V(2)
      VLD(3)=V(3)
      VLD(4)=V(4)
      IF(IPICDO.NE.0.AND.ISTODS(5,NWINDZ,IWUSDO).NE.IPICDO) RETURN
      CALL DQINV(NWINDZ,H(1),V(1),HH(1),VV(1))
      CALL DQINV(NWINDZ,H(2),V(2),HH(2),VV(2))
      CALL DQINV(NWINDZ,H(3),V(3),HH(3),VV(3))
      CALL DQINV(NWINDZ,H(4),V(4),HH(4),VV(4))
      NYES=3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQZBN
CH
      ENTRY DQZBN(HH,VV)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(ISTODS(5,IAREDO,IWUSDO).NE.IPICDO) THEN
 1000    CALL DWRT('Select right window.')
      ELSE
         CALL DQSET(IAREDO,0.,0.)
         DO K=1,4
            CALL DQPOC(HH(K),VV(K),H(K),V(K),FDUM)
         END DO
         H(5)=H(1)
         V(5)=V(1)
         CALL DGZOOM(3,IAREDO,H,V)
      END IF
      END
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZRB
CCH
C      SUBROUTINE DQZRB(H0,V0,H2,V2,PH,PV,FSTOP)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC   CALLED BY DALI_X
CC ---------------------------------------------------------------------
CC      NR   1     0      0    Normal rectangle (Macintosh) or square
CC      NR   2     0      0    Normal rectangle (Macintosh) or square
CC      US   3     1      0    Unrotated square
CC      RS   4     1      1    Rotated   square
CC      UR   5     2      0    Unrotated rectangle free aspect ratio
CC      RR   6     2      1    Rotated   rectangle with fixed aspect ratio
CC      HP   7     3      0    Horizontal parallelogram
CC      VP   8     4      0    Vertical   parallelogram
CC
C      INCLUDE 'DALI_CF.INC'
CC      COMMON/XZOOM/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
C      DIMENSION PH(5),PV(5)
CC      LOGICAL FSTRDZ,FSTOP
C      LOGICAL FSTOP
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TPARDA=
C     &  'J_PBM'
C      CALL DPARAM(11
C     &  ,J_PBM)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      IF(FSTRDZ) THEN
C         IF(NWINDZ.LT.0) THEN
C            HM=0.5*(H0+H2)
C            VM=0.5*(V0+V2)
C            CALL DPOAR(HM,VM,NWINDZ)
C         END IF
C         IF(NWINDZ.LT.0) GO TO 996
C         IF(ISTODS(5,NWINDZ,IWUSDO).NE.IPICDO) GO TO 997
C         IF(ISTODS(6,NWINDZ,IWUSDO).NE.0.AND.
C     &     (PSTODS(1,J_PBM,NWINDZ,IWUSDO).GT.4..AND.
C     &     MOD1DZ.LT.5)) GO TO 998
C         CALL DQSET(NWINDZ,0.,0.)
C         CALL DQPOC(0.,0.,HCNT,VCNT,FO2)
C         VPOS=VCNT+ABS(V0-VCNT)
C         VNEG=VCNT-ABS(V0-VCNT)
C         IF(MOD1DZ.EQ.5) THEN
C            H0C=H0-HCNT
C            V0C=V0-VCNT
C            R5=V0C**2+H0C**2
C            IF(R5.EQ.0.) GO TO 999
C            QH5=H0C/R5
C            QV5=V0C/R5
C         ELSE IF(MOD1DZ.EQ.7) THEN
C            IF(V0.EQ.VCNT) GO TO 999
C            Q7=(H0-HCNT)/(V0-VCNT)
C            PV(3)=V0
C            PV(4)=V0
C         END IF
C         FSTRDZ=.FALSE.
C      END IF
C      GO TO (10,10,30,40,50,60,70,80),MOD1DZ
C      GO TO 999
CC                                              UR  1   Rectangle (Macintosh)
C   10 PH(1)=H0
C      PH(2)=H2
C      PH(3)=H2
C      PH(4)=H0
C      PV(1)=V0
C      PV(2)=V0
C      PV(3)=V2
C      PV(4)=V2
C      GO TO 9
CC                                                       Unrotated square
C   30 H1=H0
C      V1=V0
C      DH=H2-H1
C      DV=V2-V1
C      R12=SQRT(DH**2+DV**2)
C      IF(R12.LE.1.) THEN
C         DO K=1,5
C            PH(K)=H0
C            PV(K)=V0
C         END DO
C         RETURN
C      END IF
C      GO TO 45
CC                                                       Rotated square
C   40 R01=SQRT((H0-HCNT)**2+(V0-VCNT)**2)
C      R02=SQRT((H2-HCNT)**2+(V2-VCNT)**2)
C      IF(R02.LE.R01) THEN
C         DO K=1,5
C            PH(K)=H0
C            PV(K)=V0
C         END DO
C         RETURN
C      END IF
C      Q=R01/R02
C      H1=HCNT+Q*(H2-HCNT)
C      V1=VCNT+Q*(V2-VCNT)
CC      DH=H2-H1
CC      DV=V2-V1
CC      R12=SQRT(DH**2+DV**2)
C  45  HM=0.5*(H1+H2)
C      VM=0.5*(V1+V2)
C      S=0.5*MAX(ABS(H2-H1),ABS(V2-V1))
C      PH(1)=HM-S
C      PH(2)=HM+S
C      PH(3)=PH(2)
C      PH(4)=PH(1)
C      PV(1)=VM-S
C      PV(2)=PV(1)
C      PV(3)=VM+S
C      PV(4)=PV(3)
C      GO TO 9
CC                                               Unrotated rectangle
CC   50 GO TO 999
CC   50 HM=0.5*(H0+H2)
CC      VM=0.5*(V0+V2)
CC      DH=HM-HCNT
CC      DV=VM-VCNT
CC      RR=DH**2+DV**2
CC      IF(RR.NE.0.) QR=1./RR
CC      PH(1)=H0
CC      PV(1)=V0
CC      PH(3)=H2
CC      PV(3)=V2
CC      PH(4)=QR*((V2-V0)*DH*DV+H0*DV**2+H2*DH**2)
CC      IF(DH.NE.0.) THEN
CC        PV(4)=V0+(PH(4)-H0)*DV/DH
CC      ELSE
CC        PV(4)=V2
CC      END IF
CC      PH(2)=2.*HM-PH(4)
CC      PV(2)=2.*VM-PV(4)
CC                                               Unrotated rectangle
C   50 H2C=H2-HCNT
C      V2C=V2-VCNT
C      HV=H2C*H0C+V2C*V0C
C      HM=QH5*HV+HCNT
C      VM=QV5*HV+VCNT
C      DH=H2-HM
C      DV=VM-V2
C      PH(1)=HM-DH
C      PH(2)=HM+DH
C      PH(3)=H0+DH
C      PH(4)=H0-DH
C      PV(1)=VM+DV
C      PV(2)=VM-DV
C      PV(3)=V0-DV
C      PV(4)=V0+DV
C      GO TO 9
CC                                                 Rotated rectangle
C   60 R01=SQRT((H0-HCNT)**2+(V0-VCNT)**2)
C      R02=SQRT((H2-HCNT)**2+(V2-VCNT)**2)
C      IF(R02.LE.R01) THEN
C         DO K=1,5
C            PH(K)=H0
C            PV(K)=V0
C         END DO
C         RETURN
C      END IF
C      Q=R01/R02
C      H1=HCNT+Q*(H2-HCNT)
C      V1=VCNT+Q*(V2-VCNT)
C      DH=H2-H1
C      DV=V2-V1
C      R12=SQRT(DH**2+DV**2)
C      B=R12*QASRDZ
C      A=ATAN2(DV,DH)
C      SA=SIN(A)*B
C      CA=COS(A)*B
C      PH(1)=H1-SA
C      PV(1)=V1+CA
C      PH(2)=H1+SA
C      PV(2)=V1-CA
C      PH(3)=H2+SA
C      PV(3)=V2-CA
C      PH(4)=H2-SA
C      PV(4)=V2+CA
C      GO TO 9
CC                                     Horizontal parallelogram
CC  130 IF(V2.GT.VCNT) THEN
CC        V1=VPOS
CC        IF(V2.LT.V1) V2=V1
CC      ELSE
CC        V1=VNEG
CC        IF(V2.GT.V1) V2=V1
CC      END IF
CC      IF(V2.NE.VCNT) THEN
CC        HV=(H2-HCNT)/(V2-VCNT)
CC        H1=HCNT+(V1-VCNT)*(H2-HCNT)/(V2-VCNT)
CC      ELSE
CC        H1=H2
CC      END IF
CC      B=QAS*SQRT((H2-H1)**2+(V2-V1)**2)
CCC      IF(V1.NE.V2) THEN
CCC        DV=V2-V1
CCC        B=QAS*ABS(DV+(H2-H1)**2/DV)
CCC      ELSE
CCC        B=0.
CCC      END IF
CC      PH(1)=H1-B
CC      PH(2)=H1+B
CC      PH(3)=H2+B
CC      PH(4)=H2-B
CC                                                    Horizontal parallelogram
CC  70  PV(1)=V0
CC      PV(2)=V0
CC      PV(3)=V2
CC      PV(4)=V2
CC      PH(1)=H0
CC      PH(3)=H2
CC      HM=0.5*(H0+H2)-HCNT
CC      VM=0.5*(V0+V2)-VCNT
CC      IF(VM.NE.0.) THEN
CC        DH=(V2-V0)*HM/VM
CC      ELSE
CC        DH=0.
CC      END IF
CC      PH(2)=H2-DH
CC      PH(4)=H0+DH
CC                                                    Horizontal parallelogram
C   70 V2C=V2-VCNT
C      PV(1)=V2
C      PV(2)=V2
C      HM=Q7*V2C+HCNT
CC      HM=Q7*V2C+VCNT
C      DH=ABS(H2-HM)
C      PH(1)=HM-DH
C      PH(2)=HM+DH
C      PH(3)=H0+DH
C      PH(4)=H0-DH
C      GO TO 9
CC                                                    Vertical parallelogram
C  80  PV(1)=V1+B
C      PV(2)=V1-B
C      PV(3)=V2-B
C      PV(4)=V2+B
C      PH(1)=H1
C      PH(2)=H1
C      PH(3)=H2
C      PH(4)=H2
C    9 PH(5)=PH(1)
C      PV(5)=PV(1)
C      RETURN
C  996 CALL DWRT('There is no full picture on the window!')
C      GO TO 999
C  997 CALL DWRT('Window / projection mismatch! Select right one.')
C      GO TO 999
C  998 CALL DWRT('Do not use SQ-mode on a rotated projection!')
C  999 FSTOP=.TRUE.
C      NWINDZ=0
C      END
