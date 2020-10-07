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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
*DK DGT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGT
CH
      SUBROUTINE DGT
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
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TLAST/' '/
      CHARACTER *2 TA,TCW
      DATA TCW/'cw'/
      CHARACTER *3  THLP,TBACK,TCR
      CHARACTER *18 TIST
      DATA TIST/' '/
      CHARACTER *49 T
      CHARACTER *1 T0789(-4:-1),TPRG
      DATA T0789/'0','7','8','9'/,TPRG/'\'/
      LOGICAL NYES,F0,FPRG,FHEAD
      DATA FPRG/.FALSE./
      FHEAD=.FALSE.
      THLP='GT '
      IARE=IAREDO
      IF(TPICDO.NE.'GG'.AND.TPICDO.NE.'GT') THEN
        T='GT:W1: Non-existent processor. Type a correct one.'
        TPICDO='GT'
      END IF
    1 IF(TPICDO.NE.'GG') THEN
C          123456789 123456789 123456789
        T='GT:W1:     Select a processor.'
        TPICDO='GT'
        CALL DWRT('.................................................')
        CALL DWR_HL_AR(T)
        FHEAD=.TRUE.
    2   CALL DGZOOM(6,IAREDO,0,0)
        THLP(3:3)=TPRGDH
        TPRGDH=' '
        CALL DQHL_MENU(THLP)
      END IF
      CALL DWRT_END(2)
  936 CALL DOP2LT(TLAST,NTYPE,NEXEC,TA,F)

C     ................... 5.6.97 intrinsic macros give "  " at the end ??
      IF(TA.EQ.'  ') GO TO 936
      IF(TA.EQ.'**') GO TO 936
      IF(TA.EQ.'T ') THEN
        TPICDO='GT'
        GO TO 1
      END IF
      IF(     TA(1:1).EQ.TPRG) THEN
        THLP='GT'//TA(2:2)
        CALL DQHL_MENU(THLP)
        TA=' '
        IF(.NOT.FHEAD) THEN
          T='GT:W1:     Select a processor.'
          TPICDO='GT'
          CALL DWRT('.................................................')
          CALL DWR_HL_AR(T)
          FHEAD=.TRUE.
        END IF
        GO TO 936
      END IF
      IF(FPRG) THEN
        FPRG=.FALSE.
      ELSE
        TPRGDH=' '
      END IF
      IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?')
     &  CALL DO_RESET_COMMAND_LIST(TA)
C                                               DO
      CALL DO_STR('DO: get selected window')
      IF(NEXEC.EQ.2) THEN
        NARE=IAREDO
        GO TO 3
      END IF
C                                               RETURN
      IF(NTYPE.EQ.5) GO TO 8
      IF(NTYPE.EQ.2) THEN
C                                               P2
c        CALL DO_STR('P2: go to page 2')
c        IF(TA.EQ.'P2') THEN
c          THLP='G2 '
c          TPICDO='GT'
c          GO TO 2
c        END IF
C                                               GT,GO
        CALL DO_STR('GT: no action')
        IF(TA.EQ.'GT') THEN
          THLP='GT '
          TPICDO='GT'
          GO TO 2
C                                               GG
        ELSE IF(TA.EQ.'GG: no action') THEN
          GO TO 2
        END IF
C                                               GB
        CALL DO_STR('GB: go back')
        IF(TA.EQ.'GB') GO TO 8
C       ....................................... TI
        CALL DO_STR('TI')
        IF(TA.EQ.'TI') THEN
          CALL DATE(TIST( 1: 9))
          CALL TIME(TIST(11:18))
          CALL DWRT(TIST)
          GO TO 2
        END IF
C                                               W1 ...
        CALL DO_STR('W?: change window')
        IF(TA(1:1).EQ.'W'.OR.TA(1:1).EQ.'w') THEN
          CALL DAREA('W',TA,0,12,IAREDO,NYES)
          IF(NYES) THEN
            CALL DQHL_W
            CALL DGZOOM(6,IAREDO,0,0)
            IF(TA(1:1).EQ.'w') GO TO 936
            GO TO 2
          END IF
        END IF
        CALL DO_STR('WA: select window set 1')
        IF(TA.EQ.'WA') THEN
          WISUDW=1.
          CALL DQWSU
          IF(TPICDO.EQ.'GG') GO TO 8
          GO TO 2
        END IF
        CALL DO_STR('WB: select window set 2')
        IF(TA.EQ.'WB') THEN
          WISUDW=0.
          CALL DQWSU
          IF(TPICDO.EQ.'GG') GO TO 8
          GO TO 2
        END IF
        CALL DO_STR('G?: get data of selected window')
        IF(TA(1:1).EQ.'G') THEN
C                                               G1 ...
          CALL DAREA('G',TA,0,12,NARE,NYES)
          IF(NYES) GO TO 3
          DO LARE=-4,-1
            IF(TA(2:2).EQ.T0789(LARE)) THEN
              CALL DPCGVI(LARE,F0,IZOMDO)
              IF(.NOT.F0) THEN
                FGETDO=.FALSE.
                RETURN
              END IF
            END IF
          END DO
        END IF
C                                               GW
        CALL DO_STR('GC: get data of current window')
        IF(TA.EQ.'GC') THEN
          CALL DGCURG(HW,VW)
          CALL DPOAR(HW,VW,NARE)
          IF(NARE.LT.0) GO TO 4
    3     CALL DPCGAR(NARE,F0,IZOMDO)
          IF(.NOT.F0) THEN
             FGETDO=.FALSE.
             GO TO 9
          END IF
    4     CALL DWRT('No picture defined in this window?')
          GO TO 2
        END IF
        IF(TA.EQ.'QW: only used for wizzard else no action') THEN
          FWIZDW=.FALSE.
          CALL DQTIT(IFULDB)
          CALL DQHL_R
          GO TO 1
        END IF
        IF(TA.EQ.TCW) THEN
          FCONDW=.TRUE.
          GO TO 1
        END IF
        IF(TA.EQ.'ba') THEN
          CALL DQHL_LAST_PAGE(TBACK,TCR)
          IF(TBACK.NE.TCR) THEN
            IF(TBACK(1:2).EQ.TCR(1:2)) THEN
              THLP=TBACK
              TBACK=' '
              CALL DQHL_MENU(THLP)
              CALL DWRT_END(2)
              GO TO 936
            ELSE
              CALL DOPER_HELP_BACK(TBACK)
              TA=TBACK(1:2)
            END IF
          END IF
        END IF
        IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN
          DO K=1,MAXPDP
            IF(TPICDP(K).NE.'  ') CALL DO_STR(TPICDP(K))
          END DO
          CALL DO_TY_COMMAND_LIST('DGT')
          GO TO 936
        END IF
        TPICDO=TA
        GO TO 9
      ELSE
        IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN
          DO K=1,MAXPDP
            IF(TPICDP(K).NE.'  ') CALL DO_STR(TPICDP(K))
          END DO
          CALL DO_TY_COMMAND_LIST('DGT')
        ELSE
          T='GT:W1: Invalid command. Select a processor.'
        END IF
      END IF
      IF(TPICDO.EQ.'GG') THEN
        CALL DWRT('.................................................')
        CALL DWR_HL_AR(T)
        TPICDO='GT'
      ELSE
        T(1:6)=' '
        CALL DWRT(T)
      END IF
      CALL DQHLP(THLP)
      GO TO 2
    8 IAREDO=IARE
      TPICDO='GB'
      FGETDO=.FALSE.
    9 CALL DGZOOM(6,-1,0,0)
      END
