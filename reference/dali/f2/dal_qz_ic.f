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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DZ
      COMMON/DZOOMC/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
      LOGICAL FSTRDZ
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
