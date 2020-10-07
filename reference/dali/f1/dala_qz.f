CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQ_ZOOM
CH
      SUBROUTINE DQ_ZOOM(TMOD,TANSW,JH0,JV0,HZ,HRB,VRB,MODEB,NYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
C     ....... CROSS HAIR   RB
      INCLUDE 'DALI_CF.INC'
C     NYES = 0 wrong command, 1 go to 936 , 2 go to 930, 3 update cone
C     mode
C     1     NS unrotated rectangle
C     2     SQ square
C     3     NS rotating rectangle
C     4     NS parallelogram with horizontal line
      CHARACTER *2 TANSW,TMOD(4)
      LOGICAL FDUM,FRB,FMOD
C                                             start position =| |= # of p.line
C                                                             V V
      DIMENSION HRB(4),VRB(4),H(13),V(13),IH(13),IV(13),NDSEG(2,5)
      DATA NDSEG/1,2,   !  1,5
     &           3,2,   !  6,2
     &           8,2,
     &          10,2,
     &          12,2/
      EXTERNAL DQZO_MM

      CALL DPARGV(11,'PBP',2,PBP)

      FMOD=.FALSE.
      NYES=2
      DO K=1,4
C       ....................................... change mode from outside
        IF(TANSW.EQ.TMOD(K)) THEN
          IF(PBP.EQ.PICNDO) GO TO 9
          IZOMDO=1
          MODEB=K
          RETURN
        END IF
        IF(TMOD(K).NE.'**') FMOD=.TRUE.
      END DO
      IF(JH0.GT.0.AND.TANSW.EQ.'SC') THEN
        IF(PBP.EQ.PICNDO) GO TO 9
        CALL DO_BAR_WRT(1.,4,
     &    'measure rubber band center: Move mouse <CR>=return') 
        CALL DMCU(HD0,VD0)
        CALL DQINV(IAREDO,HD0,VD0,PARADA(2,JH0),PARADA(2,JV0))
        PARADA(4,JH0)=1.
        RETURN
      END IF
      IF(     TANSW.EQ.'RB') THEN
        IF(PBP.EQ.PICNDO) GO TO 9
        IF(IDSTO(4,IAREDO).LE.0.OR.IDSTO(5,IAREDO).NE.IPICDO) THEN
          CALL DWRT('Wrong projection on selected window.#')
        ELSE
          H0=0.
          V0=0.
          IF(JH0.GT.0) THEN
            IF(PARADA(4,JH0).GT.0.) THEN
              H0=PARADA(2,JH0)
              V0=PARADA(2,JV0)
            ELSE
C             ........ For RZ: HZ#0 = Z OF VERTEX
              H0=HZ
            END IF
          END IF
c          CALL DO_BAR_WRT(1.,4,
c     &    'Zoom with rubber band: Move mouse <CR>=return') 
          CALL DO_BAR_ANSWER_PLATFORM_TEXT('rb')

          CALL DQSET(IAREDO,0.,0.)
          MODE=MODEB
C         ..................... setup ihc,ivc,h,v dependent on mode
          CALL DQZO_MODE_IN(TMOD,MODE,H0,V0,IHC,IVC,H,V)
C         ....................................... NDSEG(1:2, # of polyline)
C         ................................   1=start position, 2=# of points
C         ndseg(1,1)=1       ............... data statement
          NDSEG(2,1)=2
          NDSEG(1,2)=3
C         ndseg(2,2)=2       ............... data statement
          NUMPL=2
          CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DQZO_MM,.FALSE.,
     &      NDSEG,NUMPL)
          CALL DQZO_MODE_END(MODE,FRB)
          IF(FMOD) MODEB=MODE
          IF(FRB) THEN
            DO K=1,4
              CALL DQINV(IAREDO,H(K),V(K),HRB(K),VRB(K))
            END DO
            NYES=3
          END IF
        END IF
      ELSE
        NYES=0
      END IF
      RETURN

    9 CALL DWRT('Wrong cammand if general linear transformation on#')
      NYES=1

      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZO_MM
CH
      SUBROUTINE DQZO_MM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TKBD
      DIMENSION H(13),V(13),NDSEG(2,3)
      DIMENSION HP(2),VP(2)
      DATA D1/6./,DD/9./,D4/8./
      LOGICAL FBUT(5),FPUSH(4),FCLIK(4)
C     ............................. Dali NBUT2=2 NBUT3=3 backward compatible
C     ......................... Atlantis NBUT2=3 NBUT3=2 for 2 buuton mouse
C     DATA NBUT2/3/,NBUT3/2/
      DATA NTB20/9/,Q0/0.99/
      LOGICAL FHAIR,FP2
      CALL DQZO_BUT(FBUT,FPUSH,FCLIK)
      IF(FCLIK(4)) THEN
        IF(     TKBD.EQ.'L') THEN
C         ............................ draw stick pointer lines
          NC4=-NC4
        ELSE IF(TKBD.EQ.'R') THEN
          MODSQ=-MODSQ
        END IF
        RETURN
      END IF
C        IF(FC4) THEN
C          FC4=.FALSE.
C          NDSEG(1,1)=NS11
C          NDSEG(2,1)=NS21
C          NDSEG(1,2)=NS12
C          NDSEG(2,2)=NS22
C          NUMPL=NUPL
C        ELSE
C          FC4=.TRUE.
C          NS11=NDSEG(1,1)
C          NS21=NDSEG(2,1)
C          NS12=NDSEG(1,2)
C          NS22=NDSEG(2,2)
C          NUPL=NUMPL
C          NDSEG(1,1)=1
C          NDSEG(2,1)=2
C          NDSEG(1,2)=3
C          NDSEG(2,2)=2
C          NUMPL=2
C          IF(NDEB.EQ.9) THEN
C            IHC=HP(NPNT)
C            IVC=VP(NPNT)
C          END IF
C        END IF
C      END IF
      IF(NC4.GT.0) THEN
        NDSEG(1,1)=1
        NDSEG(2,1)=2
        NDSEG(1,2)=3
        NDSEG(2,2)=2
        NUMPL=2
        H(1)=IHC-D4
        H(2)=IHC+D4
        V(1)=IVC
        V(2)=IVC
        H(3)=IHC
        H(4)=IHC
        V(3)=IVC-D4
        V(4)=IVC+D4
        WRITE(TXTADW,4321) IHC,IVC
 4321   FORMAT(2I7)
        CALL DWR_OVER_PRINT(49)
        RETURN
      END IF
C     ............................... at start :
C     ....................................... FHAIR=.TRUE.
C     ....................................... FP2=.FALSE.
C     ....................................... therefore we go to RB start. 
C     ....................................... FP2 = TRUE if button 1 pushed
C     ....................................... FHAIR = TRUE at initial cross
C     ....................................... hair or if button 1 is pushed
C     ....................................... again.

C      IF(FP2.AND.(.NOT.FPUSH(NBUT2))) THEN
CC       ..................................... Once button 2 was pressed, the
CC       ..................................... rectangle or ... is drawn, and
CC       ..................................... the process finishes, when
CC       ..................................... button 2 is released. This is
CC       ..................................... the fast ALEPH R.B..
C        FBUT(5)=.TRUE.
C        RETURN
C      END IF
      IF(     FCLIK(1)) THEN
        IF(FHAIR) THEN
C         ......................... if crosshair is visble step to next mode
          CALL DQZO_MODE_STEP
        ELSE
C         ......................... else go back to cross hair
          FHAIR=.TRUE.
        END IF
      ELSE IF(FCLIK(2)) THEN
        NTB2=0
        IF(FHAIR) THEN
C         ......................... start drawing RB
          FHAIR=.FALSE.
          HP(1)=IHC
          VP(1)=IVC
          HP(2)=IHC+D1
          VP(2)=IVC+D1
          NPNT=2
        ELSE
C         .......................... switch to other point
          NPNT=MOD(NPNT,2)+1
          IHC=HP(NPNT)
          IVC=VP(NPNT)
        END IF

      ELSE IF(FPUSH(2).AND.(.NOT.FHAIR)) THEN
        NTB2=NTB2+1

      ELSE IF(.NOT.FPUSH(2).AND.(.NOT.FHAIR)) THEN

        IF(NTB2.GT.NTB20) THEN
          FBUT(5)=.TRUE.
          RETURN
        END IF

C      ELSE IF(FPUSH(NBUT2).AND.FHAIR) THEN
CC       ............................ start drawing RB, but no way back
C        FP2=.TRUE.
C        FHAIR=.FALSE.
C        HP(1)=IHC
C        VP(1)=IVC
C        HP(2)=IHC+D1
C        VP(2)=IVC+D1
C        NPNT=2
C        IHC=HP(NPNT)
C        IVC=VP(NPNT)
C      ELSE IF(FCLIK(NBUT2).AND.(.NOT.FHAIR)) THEN
CC       ........................... stop when button 2 is released.
C        FBUT(5)=.TRUE.
C        RETURN
      END IF
C     ........................... RB start: at start of RB we get to here.
      IF(FHAIR) THEN
C       ......................... draw cross hair
        CALL DQZO_MODE_GET_HAIR(IHC,IVC,H,V)
        NDSEG(2,1)=2
        NDSEG(1,2)=3
        NUMPL=2
      ELSE
C       ......................... draw RB
        HP(NPNT)=IHC
        VP(NPNT)=IVC
        CALL DQZO_MODE_GET_RB(HP,VP,H,V,NPNT,NUMPL)
        H(6)=HP(NPNT)-DD
        V(6)=VP(NPNT)-DD
        H(7)=HP(NPNT)+DD
        V(7)=VP(NPNT)+DD
        H(8)=HP(NPNT)-DD
        V(8)=VP(NPNT)+DD
        H(9)=HP(NPNT)+DD
        V(9)=VP(NPNT)-DD
        NDSEG(2,1)=5
        NDSEG(1,2)=6
        IF(NUMPL.EQ.4) THEN
          IF(MODSQ.EQ.1) THEN
            H(10)=HP(1)
            V(10)=VP(1)
            H(11)=HP(2)
            V(11)=VP(2)
          ELSE
            H(10)=HP(1)
            V(10)=VP(1)
            DHA=ABS(HP(2)-HP(1))
            DVA=ABS(VP(2)-VP(1))
            IF(DHA.GT.0..AND.DVA.GT.0.) THEN
              IF(     DVA.LT.DHA) THEN
                Q=DVA/DHA
                IF(Q.LT.Q0) THEN
                  H(11)=HP(2)
                  V(11)=VP(1)
                  H(12)=HP(1)
                  V(12)=VP(2)
                  NUMPL=5
                ELSE
                  NUMPL=3
                END IF
              ELSE IF(DHA.LT.DVA) THEN
                Q=DHA/DVA
                IF(Q.LT.Q0) THEN
                  H(11)=HP(1)
                  V(11)=VP(2)
                  H(12)=HP(2)
                  V(12)=VP(1)
                  NUMPL=5
                ELSE
                  NUMPL=3
                END IF
              ELSE
                NUMPL=3
              END IF
            ELSE
              NUMPL=2
            END IF
            H(13)=HP(2)
            V(13)=VP(2)
          END IF
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
CH -------------------------------------------------------
CH
      ENTRY DQZO_MM_0
CH --------------------------------------------------------------------
CH
      FHAIR=.TRUE.
      FP2=.FALSE.
      NC4=-1
      MODSQ=1
      NTB2=0
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZO_MODE_IN
CH
      SUBROUTINE DQZO_MODE_IN(TMOD,MODE,HIN,VIN,IHC,IVC,HD,VD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INCLUDE 'DALI_CF.INC'
      DIMENSION HD(*),VD(*),HDP(*),VDP(*)
      DIMENSION H(4) ,V(4) ,HP(2) ,VP(2) ,HR(2),VR(2)
      DIMENSION HU(3),VU(3)
      CHARACTER *2 TMOD(4),TM(4)
      CHARACTER *4 DT4
      CHARACTER *5 DT5
      CHARACTER *6 DT6
      DATA DC/1300./,GC/50./,DS/10./,DT/30./,C/0.8/,LOP/50/,DL3/10./
      DIMENSION DVHR(2)
      DATA DVHR/50.,1300./
      LOGICAL FM,FDUM,FRB,FRBO,FSTEP
      H0=HIN
      V0=VIN
      CALL DQZO_MM_0
      M=MODE
      IZOM=IDSTO(6,IAREDO)
      FSTEP=.FALSE.
      DO K=1,4
        TM(K)=TMOD(K)
        IF(TMOD(K).NE.'**') FSTEP=.TRUE.
      END DO
      HC=(1.-C)*HLOWDG(IAREDO)+C*HHGHDG(IAREDO)
      VC=(1.-C)*VLOWDG(IAREDO)+C*VHGHDG(IAREDO)
      IHC=HC
      IVC=VC
      CALL DWRT(' ')
      FRB=.FALSE.
      HHGH=HHGHDG(0)
      GO TO 10
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------
CH
      ENTRY DQZO_MODE_STEP
CH --------------------------------------------------------------------
CH
    1 IF(FSTEP) THEN
        M=MOD(M,4)+1
        IF(TM(M).EQ.'**') GO TO 1
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------
CH
      ENTRY DQZO_MODE_GET_HAIR(IHC,IVC,HD,VD)
CH --------------------------------------------------------------------
CH
C     mode = M
C     1     NS unrotated rectangle
C     2     SQ square
C     3     NS rotating rectangle
C     4     NS parallelogram with horizontal line
      HCD=IHC
      VCD=IVC
      CALL DQINV(IAREDO,HCD,VCD,HC,VC)
   10 IF(    M.LE.2) THEN
C       ....................... DC=1300 large vertical/horizontal cross
        H(1)=HC-DC
        H(2)=HC+DC
        H(3)=HC
        H(4)=HC
        V(1)=VC
        V(2)=VC
        V(3)=VC-DVHR(M)
        V(4)=VC+DVHR(M)
        IF(M.EQ.1) THEN
C                   123456789 123456789 123456789 123456789 123456789
          TXTADW=  '                    H=123456 V=123456  <CR>=stop'
        ELSE
          IF(ABS(HC-H0).LE.ABS(VC-V0)) THEN
            TXTADW='horiz. pos. fixed:  H=123456 V=123456  <CR>=stop'
          ELSE
            TXTADW='verti. pos. fixed:  H=123456 V=123456  <CR>=stop'
          END IF
        END IF
        TXTADW(23:28)=DT6(HC)
        TXTADW(32:37)=DT6(VC)
      ELSE 
        DH=(HC-H0)
        DV=(VC-V0)
        H(1)=H0
        V(1)=V0
        H(2)=HC+DH
        V(2)=VC+DV
        IF(M.EQ.3) THEN
          H(3)=HC+DV
          V(3)=VC-DH
          H(4)=HC-DV
          V(4)=VC+DH
        ELSE
          H(3)=HC-DC
          V(3)=VC
          H(4)=HC+DC
          V(4)=VC
        END IF
        AN=DATN2D(VC,HC)
        RO=SQRT((HC-H0)**2+(VC-V0)**2)
        TXTADW='Angle=123456  Rho=123456               <CR>=stop  '
        TXTADW( 7:12)=DT6(AN)
        TXTADW(19:24)=DT6(RO)
      END IF
      CALL DWR_OVER_PRINT(LOP)
      DO K=1,4
        CALL DQPOC(H(K),V(K),HD(K),VD(K),FDUM)
      END DO
      IF(M.GT.2) THEN
        DHD=HD(3)-HCD
        DVD=VD(3)-VCD
        RRD=SQRT(DHD**2+DVD**2)
        IF(RRD.GT.0.) THEN
          QRD=DL3/RRD
          HD(3)=HCD+QRD*DHD
          VD(3)=VCD+QRD*DVD
          HD(4)=HCD-QRD*DHD
          VD(4)=VCD-QRD*DVD
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
CH -------------------------------------------------------
CH
      ENTRY DQZO_MODE_GET_RB(HDP,VDP,HD,VD,NPNT,NUMPL)
CH --------------------------------------------------------------------
CH
C     mode
C     1     NS unrotated rectangle
C     2     SQ square
C     3     NS rotating rectangle
C     4     NS parallelogram with horizontal line

      CALL DQINV(IAREDO,HDP(1),VDP(1),HP(1),VP(1))
      CALL DQINV(IAREDO,HDP(2),VDP(2),HP(2),VP(2))
      IF(M.EQ.1) THEN
C       .................................................... rectangle
        H(1)=HP(1)
        V(1)=VP(1)
        H(3)=HP(2)
        V(3)=VP(2)
        CALL DQ24(H,V)
C               123456789 123456789 123456789 123456789 123456789
        TXTADW='H1=12345 V1=12345   H2=12345 V2=12345   <CR>=stop'
        TXTADW( 4: 8)=DT5(HP(1))
        TXTADW(13:17)=DT5(VP(1))
        TXTADW(24:28)=DT5(HP(2))
        TXTADW(33:37)=DT5(VP(2))
        NUMPL=3
      ELSE
        HR(1)=HP(1)-H0
        VR(1)=VP(1)-V0
        HR(2)=HP(2)-H0
        VR(2)=VP(2)-V0
        IF(     M.EQ.2) THEN
C         ...................................................... square
          IF(NPNT.EQ.2) THEN
C           .................................... point 2 is modified first
            IF(ABS(VR(1)).GT.ABS(HR(1))) THEN
              HR(2)=HR(1)*VR(2)/VR(1)
              HP(2)=H0+HR(2)
            ELSE
              VR(2)=VR(1)*HR(2)/HR(1)
              VP(2)=V0+VR(2)
            END IF
            RS2=SQRT(HR(2)**2+VR(2)**2)
            IF(HR(1)*HR(2)+VR(1)*VR(2).LT.0.) RS2=-RS2
          ELSE
            RS1=SQRT(HR(1)**2+VR(1)**2)
            IF(RS1.EQ.0.) RETURN
            Q=RS2/RS1
            HR(2)=HR(1)*Q
            VR(2)=VR(1)*Q
            HP(2)=H0+HR(2)
            VP(2)=V0+VR(2)
          END IF
          HM=H0+0.5*(HR(1)+HR(2))
          VM=V0+0.5*(VR(1)+VR(2))
          DH=0.5*ABS(HR(1)-HR(2))
          DV=0.5*ABS(VR(1)-VR(2))
          DD=MAX(DH,DV)
          H(1)=HM-DD
          V(1)=VM-DD
          H(3)=HM+DD
          V(3)=VM+DD
          CALL DQ24(H,V)
C                 123456789 123456789 123456789 123456789 123456789
          TXTADW='H=12345 V=12345 D=12345                 <CR>=stop'
          TXTADW( 3: 7)=DT5(H(3))
          TXTADW(11:15)=DT5(V(3))
          TXTADW(19:23)=DT5(DD)
C                 123456789 123456789 123456789 123456789 123456789
        ELSE IF(M.EQ.3) THEN
C         ................................................ rotating rectangle
          IF(NPNT.EQ.2) THEN
            A1=ATAN2(VR(1),HR(1))
            A2=ATAN2(VR(2),HR(2))
            R2=SQRT(HR(2)*HR(2)+VR(2)*VR(2))
          ELSE
            DA=ATAN2(VR(1),HR(1))-A1
            AM=A2+DA
            HR(2)=R2*COS(AM)
            VR(2)=R2*SIN(AM)
            HP(2)=H0+HR(2)
            VP(2)=V0+VR(2)
          END IF
          RR=(HR(1)*HR(1)+VR(1)*VR(1))
          IF(RR.EQ.0.) RETURN
          T=(HR(1)*HR(2)+VR(1)*VR(2))/RR
          HL=HR(1)*T
          VL=VR(1)*T
          DH=HL-HR(2)
          DV=VL-VR(2)
          H(1)=HP(2)
          V(1)=VP(2)
C         ........................... H(1)=H0+HL-DH=HP(2)
C         ........................... V(1)=V0+VL-DV=VP(2)
          H(2)=H0+HR(1)-DH
          V(2)=V0+VR(1)-DV
          H(3)=H0+HR(1)+DH
          V(3)=V0+VR(1)+DV
          H(4)=H0+HL+DH
          V(4)=V0+VL+DV
          DP=SQRT((H(3)-H(2))**2+(V(3)-V(2))**2)
          IF(DP.GT.0.) THEN
            DR=SQRT((H(2)-H(1))**2+(V(2)-V(1))**2)
            AS=DR/DP
C                   123456789 123456789 123456789 123456789 123456789
            TXTADW='H=12345 D=12345 V=12345 D=12345 AS=1234 <CR>=stop'
            TXTADW( 3: 7)=DT5(HR(1))
            TXTADW(11:15)=DT5(HR(1)-HR(2))
            TXTADW(19:23)=DT5(VR(2))
            TXTADW(27:31)=DT5(VR(1)-VR(2))
            TXTADW(36:39)=DT4(AS)
          END IF
          NUMPL=3
        ELSE
          IF(VR(1).NE.0.) THEN
            IF(NPNT.EQ.2) THEN
C             ....................... When point 2 is moved point 1 is fix.
              DH=HR(2)-HR(1)*VR(2)/VR(1)
              DP=HP(2)-HP(1)
            ELSE
C             ....................... When point 1 is moved point 2 moves also.
              HP(2)=H0+HR(1)*VR(2)/VR(1)+DH
            END IF
            H(2)=HP(2)
            V(2)=VP(2)
            H(1)=HP(2)-2.*DH
            V(1)=VP(2)
            H(3)=HP(1)+DH
            V(3)=VP(1)
            H(4)=HP(1)-DH
            V(4)=VP(1)
          END IF
          NUMPL=3
        END IF
      END IF
      DO K=1,4
        CALL DQPOC(H(K),V(K),HD(K),VD(K),FDUM)
      END DO
      HD(5)=HD(1)
      VD(5)=VD(1)
      IF(M.EQ.2) NUMPL=4
C     ....................................... only HP(2),VP(2) can be modified
      CALL DQPOC(HP(2),VP(2),HDP(2),VDP(2),FDUM)
      CALL DWR_OVER_PRINT(LOP)
      FRB=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------- DQZO_MODE_END
CH
      ENTRY DQZO_MODE_END(MODE_OUT,FRBO)
CH --------------------------------------------------------------------
CH
      MODE_OUT=M
      FRBO=FRB
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQZO_REC
CH
      SUBROUTINE DQZO_BUT(FBUT,FPUSH,FCLIK)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     If button k is NOT pushed, FBUT(K) is TRUE. This non logical
C     situation is inverted by DQZO_BUT.
C     If a button is pushed FPUSH is true as long as the button is hold
C     down.
C     When a button is pushed FCLIK is true only once until button is 
C     released and pushed again.

      LOGICAL FBUT(4),FPUSH(4),FCLIK(4),FSTAT(4)
      DO K=1,4
        IF(FBUT(K)) THEN
          FPUSH(K)=.FALSE.
          FCLIK(K)=.FALSE.
          FSTAT(K)=.TRUE.
        ELSE
          FPUSH(K)=.TRUE.
          IF(FSTAT(K)) THEN
            FCLIK(K)=.TRUE.
          ELSE
            FCLIK(K)=.FALSE.
          END IF
          FSTAT(K)=.FALSE.
        END IF
      END DO
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQZO_DISPLAY
CH
      SUBROUTINE DQZO_DISPLAY(TANSW,HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INCLUDE 'DALI_CF.INC'
      EXTERNAL DQZO_DUMMY
      CHARACTER *2 TANSW
      DIMENSION HRB(4),VRB(4),H(5),V(5),IH(5),IV(5),NDSEG(2,1)
      DATA NDSEG/1,2/,NUMPL/1/
      LOGICAL FDUM
      IF(IDSTO(5,IAREDO).NE.IPICDO) THEN
 1000    CALL DWRT('Select right window.')
      ELSE
         CALL DQSET(IAREDO,0.,0.)
         DO K=1,4
            CALL DQPOC(HRB(K),VRB(K),H(K),V(K),FDUM)
         END DO
         H(5)=H(1)
         V(5)=V(1)
         IF(TANSW.EQ.'DB') THEN
           CALL DGZOOM(3,IAREDO,H,V)
         ELSE IF(TANSW.EQ.'CB') THEN
           CALL DGZOOM(0,0,0,0)
         ELSE
           CALL DPARGV(96,'SRB',2,DLINDD)
           CALL DPAR_SET_CO(93,'CRB')
           CALL DGZOOM(0,0,0,0)
           CALL DGDRAW(5,H,V)
         END IF
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZO_DUMMY
CH
      SUBROUTINE DQZO_DUMMY(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION H(9),V(9),NDSEG(2,3)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4)
      END
*DK DQZ_OFFSET
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQZ_OFFSET
CH
      SUBROUTINE DQZ_OFFSET(S,JH0,JV0,HZ,HR1,VR1,HR2,VR2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     ......... correct hrb1,vrb1 to hrb2,vrb2 by S*offset
      INCLUDE 'DALI_CF.INC'
      DIMENSION HR1(4),VR1(4),HR2(4),VR2(4)
      H0=0.
      V0=0.
      IF(PARADA(4,JH0).GT.0.) THEN
        H0=S*PARADA(2,JH0)
        V0=S*PARADA(2,JV0)
      ELSE
C       ........ For RZ: HZ#0 = Z OF VERTEX
        H0=S*HZ
      END IF
      DO K=1,4
        HR2(K)=H0+HR1(K)
        VR2(K)=V0+VR1(K)
      END DO
      END

      SUBROUTINE DQZRB(H0,V0,H2,V2,PH,PV,FSTOP)
      END
