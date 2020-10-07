*DK DGRB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGRB
CH
      SUBROUTINE DGRB(TANSW,FRET)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'FR'  (DGRB)
C AND     BY TANSW.EQ.'FZ'  (DGRB)
C AND     BY TANSW.EQ.'RZ'  (DGRB)
C AND     BY TANSW.EQ.'RO'  (DGRB)
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EXTERNAL DGRBL
      CHARACTER *2 TANSW
      CHARACTER *3 TDG
      DIMENSION H(5),V(5),IH(5),IV(5),NDSEG(2,2)
      DIMENSION HRB(*),VRB(*)
      DIMENSION HRD(5),VRD(5)
      DIMENSION KP(3)
      DATA ANG/0./
      LOGICAL FEMTY,FRET
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PBP,J_PB1,J_PB2,J_PB3'
      CALL DPARAM(11
     &  ,J_PBP,J_PB1,J_PB2,J_PB3)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FRET=.TRUE.
      CALL DO_STR('ZB: 3 point rubber band')
      IF(TANSW.EQ.'ZB') THEN
C       ................................................................... B3
        CALL DO_BAR_ANSWER_PLATFORM_TEXT('zb')
        CALL DGRBL0
        CALL DGLINM(IHC,IVC,H,V,IH,IV,IAR,DGRBL,
     &    .FALSE.,NDSEG,NUMPL)
        IF(NDSEG(2,1).EQ.5) THEN
          KP(1)=J_PB1
          KP(2)=J_PB2
          KP(3)=J_PB3
          DO N=1,3
            K=KP(N)
            CALL DQINV(IAREDO,H(N),V(N),PARADA(2,K),PARADA(4,K))
          END DO
          PARADA(2,J_PBP)=PICNDO
        END IF
        RETURN
      END IF
      CALL DO_STR('ZS: Store user range for 3 p.rubber band')
      IF(TANSW.EQ.'ZS') THEN
        KP(1)=J_PB1
        KP(2)=J_PB2
        KP(3)=J_PB3
        DO N=1,3
          K=KP(N)
          PARADA(2,K)=HUSRDQ(N,IAREDO)
          PARADA(4,K)=VUSRDQ(N,IAREDO)
        END DO
        PARADA(2,J_PBP)=PICNDO
        RETURN
      END IF
      CALL DO_STR('ZR: rotate picture.')
      IF(TANSW.EQ.'ZR') THEN
   10   CALL DWRT('Rotate by how much [degrees]? <cr>=go back=o,k.')
        CALL DGETLN(TDG,LDG,3)
        IF(LDG.GT.0) THEN
          READ(TDG(1:LDG),1010,ERR=19) IDG
C 1010     FORMAT(I)
 1010     FORMAT(BN,I3)
          ANG=IDG
          SA=SIND(ANG)
          CA=COSD(ANG)
          KP(1)=J_PB1
          KP(2)=J_PB2
          KP(3)=J_PB3
          GO to 20
        END IF
        RETURN
   19   CALL DWRT('Degrees are only accepted in integers.#')
        GO TO 10
      END IF
      CALL DO_STR('ZA: rotate further by same angle.')
      IF(TANSW.EQ.'ZA'.AND.ANG.NE.0.) GO TO 20
C     ................ IF(PARADA(2,J_PBP).NE.FLOAT(IPICDO)) RETURN
      CALL DO_STR('Z9: rotate by 90 degrees.')
      IF(TANSW.EQ.'Z9') THEN
C       ................................................................. N3
        DO I=2,4,2
          P4=PARADA(I,J_PB1)+PARADA(I,J_PB3)-PARADA(I,J_PB2)
          PARADA(I,J_PB3)=PARADA(I,J_PB2)
          PARADA(I,J_PB2)=PARADA(I,J_PB1)
          PARADA(I,J_PB1)=P4
        END DO
        RETURN
      END IF
      CALL DO_STR('ZM: get mirror image')
      IF(TANSW.EQ.'ZM') THEN
C       ................................................................. M3
        DO I=2,4,2
          PARADA(I,J_PB1)=PARADA(I,J_PB1)
     &      +PARADA(I,J_PB3)-PARADA(I,J_PB2)
          P3=PARADA(I,J_PB3)
          PARADA(I,J_PB3)=PARADA(I,J_PB2)
          PARADA(I,J_PB2)=P3
        END DO
        RETURN
      END IF
      CALL DO_STR('ZF: fix 3 point ruber band outline.')
      IF(TANSW.EQ.'ZF') THEN
C         ................................................................. F3
        IF(PARADA(2,J_PBP).NE.PICNDO) RETURN
        CALL DQSET(IAREDO,0.,0.)
        CALL DQLEVL(ICRZDD)
        DLINDD=PDCODD(2,LIGLDD)
        KP(1)=J_PB1
        KP(2)=J_PB2
        KP(3)=J_PB3
        DO N=1,3
          HRD(N)=PARADA(2,KP(N))
          VRD(N)=PARADA(4,KP(N))
        END DO
        HRD(4)=HRD(1)+HRD(3)-HRD(2)
        VRD(4)=VRD(1)+VRD(3)-VRD(2)
        HRD(5)=HRD(1)
        VRD(5)=VRD(1)
        DO N=1,4
          CALL DQLIE(HRD(N),VRD(N))
        END DO
        DLINDD=PDCODD(2,LITRDD)
        RETURN
      END IF
      CALL DO_STR('ZQ: quit from 3 point rubber band.')
      IF(TANSW.EQ.'ZQ') THEN
C       ................................................................. Q3
        PARADA(2,J_PBP)=0.
        RETURN
      END IF
      FRET=.FALSE.
      RETURN
   20 HM=0.5*(HUSRDQ(1,IAREDO)+HUSRDQ(3,IAREDO))
      VM=0.5*(VUSRDQ(1,IAREDO)+VUSRDQ(3,IAREDO))
      DO N=1,3
        K=KP(N)
        HU=HUSRDQ(N,IAREDO)-HM
        VU=VUSRDQ(N,IAREDO)-VM
        PARADA(2,K)=HM+CA*HU+SA*VU
        PARADA(4,K)=VM-SA*HU+CA*VU
      END DO
      PARADA(2,J_PBP)=PICNDO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DGRBST
CH
      ENTRY DGRBST(HRB,VRB,FEMTY)
CH
CH --------------------------------------------------------------------
CH
      FEMTY=.TRUE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PBP,J_PB1,J_PB2,J_PB3'
      CALL DPARAM(11
     &  ,J_PBP,J_PB1,J_PB2,J_PB3)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_PBP).NE.PICNDO) RETURN
      IF(PARADA(2,J_PB1).EQ.PARADA(2,J_PB2).AND.
     &   PARADA(2,J_PB1).EQ.PARADA(2,J_PB3)) RETURN
      IF(PARADA(4,J_PB1).EQ.PARADA(4,J_PB2).AND.
     &   PARADA(4,J_PB1).EQ.PARADA(4,J_PB3)) RETURN
      FEMTY=.FALSE.
      KP(1)=J_PB1
      KP(2)=J_PB2
      KP(3)=J_PB3
      DO N=1,3
        HRB(N)=PARADA(2,KP(N))
        VRB(N)=PARADA(4,KP(N))
      END DO
      HRB(4)=HRB(1)+HRB(3)-HRB(2)
      VRB(4)=VRB(1)+VRB(3)-VRB(2)
      END
*DK DGRBSC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGRBSC
CH
      SUBROUTINE DGRBSC(HRB,VRB,SCA,HMID,VMID,AS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     ..................... CALCULATE SCALE, CENTER POSITION AND ASPECT RATIO
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(*),VRB(*),SCA(*)
      DIMENSION HH(3),VV(3)
      LOGICAL FIN
      HMID=0.5*(HRB(1)+HRB(3))
      VMID=0.5*(VRB(1)+VRB(3))
      CALL DPARGI(84,'OZS',MODSC)
      IF(MODSC.EQ.1) THEN
        CALL DQPOC(0.,0.,H0,V0,FIN)
C       ..... 1) Total scale difference must be : D=SQRT(DH*DH+DV*DV)
C       ..... 2) User point 0,0 must have scale 0,0 :
C       .....    transform user 0,0 to display coordinates, get perpendicular
C       .....    display position on lower (HH(1),VV(1)) and left axis
C       .....    (HH(3),VV(3)) ; then calculate via straight line:
C       .....    H=HRB(1)+T*DH, V=VRB(2)+T*DV left (low) and right (up) scale.
        CALL DQINV(IAREDO,H0,VLOWDG(IAREDO),HH(1),VV(1))
        CALL DQINV(IAREDO,HLOWDG(IAREDO),V0,HH(3),VV(3))
        DO K=1,3,2
          N=K+1
          DH=HRB(N)-HRB(1)
          DV=VRB(N)-VRB(1)
          IF(ABS(DH).GT.ABS(DV)) THEN
            T=(HH(K)-HRB(1))/DH
          ELSE
            T=(VV(K)-VRB(1))/DV
          END IF
          D=SQRT(DH*DH+DV*DV)
          SCA(K)=-T*D
          SCA(N)=(1.-T)*D
        END DO
      ELSE
        DH3=SQRT( (HRB(2)-HRB(1))**2+(VRB(2)-VRB(1))**2)
        DV3=SQRT( (HRB(4)-HRB(1))**2+(VRB(4)-VRB(1))**2)
        IF(HMID.EQ.0..AND.VMID.EQ.0.) THEN
          DH3=0.5*DH3
          DV3=0.5*DV3
          SCA(1)=-DH3
          SCA(2)= DH3
          SCA(3)=-DV3
          SCA(4)= DV3
        ELSE
          SCA(1)=0.
          SCA(2)=DH3
          SCA(3)=0.
          SCA(4)=DV3
        END IF
      END IF
      IF(DH3.GT.0..AND.DV3.GT.0.) AS=DV3/DH3
      END
*DK DGRBTY
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGRBTY
CH
      SUBROUTINE DGRBTY
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CALL DPARGV(11,'PBP',2,PBP)
      IF(PBP.EQ.PICNDO) THEN
        CALL DWRT('Zooming via normal rub.band cursors may be wrong.')
        CALL DWRT('3 point rubberband in use! Type ZQ to quit from it.')
      END IF
      END
*DK DGRBL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGRBL
CH
      SUBROUTINE DGRBL(IHC,IVC,H,V,NDSEG,NUMPL,FBUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(5),V(5),NDSEG(2,2)
      DATA D/10./
C     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C     QUESTION TO Bjoern WHAT IS TKBD???
C     CHARACTER*(*) TKBD
C     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      LOGICAL FBUT(4),F1
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      IF(FBUT(1)) THEN
        IF(F1) THEN
C         .......................................................1) move cross
          HC0=IHC
          VC0=IVC
          H(1)=HC0-D
          H(2)=HC0+D
          H(3)=HC0
          H(4)=HC0
          V(1)=VC0
          V(2)=VC0
          V(3)=VC0-D
          V(4)=VC0+D
          NUMPL=2
          NDSEG(1,1)=1
          NDSEG(1,2)=3
          NDSEG(2,1)=2
          NDSEG(2,2)=2
        ELSE IF(.NOT.FBUT(2)) THEN
          AH=H(2)-H(1)
          AV=V(2)-V(1)
          Q=AH*AH+AV*AV
          IF(Q.GT.0.) THEN
            H(3)=IHC
            V(3)=IVC
            TP=( AH*(H(3)-H(1)) +AV*(V(3)-V(1)) )/Q
            H(2)=AH*TP+H(1)
            V(2)=AV*TP+V(1)
            H(4)=H(1)+H(3)-H(2)
            V(4)=V(1)+V(3)-V(2)
          END IF
        ELSE
C         ...................................... 4) button up: move position 3
          H(3)=IHC
          V(3)=IVC
          H(4)=H(1)+H(3)-H(2)
          V(4)=V(1)+V(3)-V(2)
          NDSEG(2,1)=5
C         .................................................. 6) stop with <CR>
        END IF
      ELSE IF(.NOT.FBUT(1)) THEN
        IF(F1) THEN
C         ................................... 2) button down: store position 1
          F1=.FALSE.
          H(1)=IHC
          V(1)=IVC
          H(5)=H(1)
          V(5)=V(1)
          NUMPL=1
          NDSEG(2,1)=1
        ELSE
C         .................................... 3) button down: move position 2
          H(2)=IHC
          V(2)=IVC
          NUMPL=1
          NDSEG(2,1)=2
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DGRBL0
CH
      ENTRY DGRBL0
CH
CH --------------------------------------------------------------------
CH
      F1=.TRUE.
      CALL DWRT('Toggle left button to draw lines.')
      CALL DWRT_END(0)
      END
