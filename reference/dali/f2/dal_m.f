*DK DM_FIX_MARKERS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------- DM_FIX_MARKERS
CH
      SUBROUTINE DM_FIX_MARKERS(NUMPL,NDSEG,H,V)
CH
C     called by dmps
CH --------------------------------------------------------------------
      DIMENSION H(*),V(*),NDSEG(2,*)
      IF(NUMPL.GT.0) THEN
        CALL DPAR_SET_WIDTH(89,'MWI',0,' ')
        CALL DPAR_SET_CO(89,'MCO')
        DO K=1,NUMPL
          CALL DGDRAW(NDSEG(2,K),H(NDSEG(1,K)),V(NDSEG(1,K)))
        END DO
        CALL DPAR_SET_TR_WIDTH
      END IF
      END
*DK DMCU
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMCU
CH
      SUBROUTINE DMCU(H0,V0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Draw cursor line
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(2),V(2),IH(2),IV(2),NDSEG(1,2)
      DATA NUMPL/1/,NDSEG/1,2/
      EXTERNAL DMCUM
      CALL DO_BAR_ANSWER_PLATFORM_TEXT('qq')
      CALL DGCURG(HC,VC)
      IHC=HC
      IVC=VC
      IF(IHC.EQ.0.OR.IVC.EQ.0) THEN
        IHC=300
        IVC=300
      END IF
      CALL DMCUM0(H(1),V(1))
      H(2)=IHC
      V(2)=IVC
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DMCUM,.FALSE.,NDSEG,NUMPL)
      H0=H(2)
      V0=V(2)
      END
*DK DMCUM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMCUM
CH
      SUBROUTINE DMCUM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    MOVE CROSS OR MARKERS
C    Called by : DGTLNM <- DM3D
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),NDSEG(2,*)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(*),FB1,FB2,FB3
      DATA FB1/.TRUE./,FB2/.TRUE./,FB3/.TRUE./,J/1/,JC/4/
      DIMENSION HC(4),VC(4)
C     ......................... Button 1 pressed = move corner
      IF(.NOT.FBUT(1)) THEN
        IF(FB1) THEN
          FB1=.FALSE.
          JC=MOD(JC,4)+1
          IHC=H(J)
          IVC=V(J)
          H(1)=HC(JC)
          V(1)=VC(JC)
          H(2)=IHC
          V(2)=IVC
          J=2
        END IF
        RETURN
      END IF
C     ......................... Button 2 pressed = move other point
      IF(.NOT.FBUT(2)) THEN
        IF(FB2) THEN
          FB2=.FALSE.
          J=3-J
          IHC=H(J)
          IVC=V(J)
        END IF
        RETURN
      END IF
C     ......................... Button 3 pressed = output
      IF(.NOT.FBUT(3)) THEN
        IF(FB3) THEN
          FB3=.FALSE.
          IH1=H(1)
          IH2=H(2)
          IV1=V(1)
          IV2=V(2)
          IDH=IH2-IH1
          IDV=IV2-IV1
          WRITE(TXTADW,1000) 'H: ',IH1,IDH,IH2,'   V: ',IV1,IDV,IV2
 1000     FORMAT(2(A,I4,' + ',I4,' = ',I4))
          CALL DWRC
        END IF
        RETURN
      END IF
C     ....................................... no button pressed
      FB1=.TRUE.
      FB2=.TRUE.
      FB3=.TRUE.
      H(J)=IHC
      V(J)=IVC
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DMCUM0
CH
      ENTRY DMCUM0(HH,VV)
CH
C     called by dmps
CH --------------------------------------------------------------------
      HC(1)=1.
      HC(3)=HHGHDG(13)-1.
      VC(1)=1.
      VC(3)=VHGHDG(13)-1.
      HC(2)=HC(3)
      HC(4)=HC(1)
      VC(2)=VC(1)
      VC(4)=VC(3)
      HH=HC(JC)
      VV=VC(JC)
      J=2
      END
*DK DM3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM3D
CH
      SUBROUTINE DM3D
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  no interaction via DOPER. Returns from where it was called.
C    Draw 3D flying 3D spot on all projections
C    Called by : DBR1
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (NLIN=28)
      DIMENSION H(2*NLIN),V(2*NLIN),IH(2*NLIN),IV(2*NLIN),NDSEG(2,NLIN)
      DATA N8/8/,S1/1./
      EXTERNAL DM3DM
      CALL DQHLP('M3 ')
      KK=1
      DO K=1,NLIN
        NDSEG(1,K)=KK
        NDSEG(2,K)=2
        KK=KK+2
      END DO
      CALL DM3D0(H,V,HC,VC)
      IHC=HC
      IVC=VC
      NUMPL=2
      CALL DO_BAR_ANSWER_PLATFORM_TEXT('3m')
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DM3DM,.FALSE.,NDSEG,NUMPL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM3DF
CH
      ENTRY DM3DF
CH
C     called by dmps
CH --------------------------------------------------------------------
      DLIN=DLINDD
      DLINDD=S1
      CALL DGLEVL(N8)
      DO K=1,NUMPL
        CALL DGDRAW(NDSEG(2,K),H(NDSEG(1,K)),V(NDSEG(1,K)))
      END DO
      DLINDD=DLIN
      END
*DK DMFIX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMFIX
CH
      SUBROUTINE DMFIX(H,V,NDSEG,NUMPL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),NDSEG(2,*)
      DATA S1/1./,N8/8/
      DLIN=DLINDD
      DLINDD=S1
      CALL DGLEVL(N8)
      DO K=1,NUMPL
        CALL DGDRAW(NDSEG(2,K),H(NDSEG(1,K)),V(NDSEG(1,K)))
      END DO
      DLINDD=DLIN
      END
*DK DM3DM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM3DM
CH
      SUBROUTINE DM3DM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    MOVE CROSS OR MARKERS
C    Called by : DGTLNM <- DM3D
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),NDSEG(2,*)
      CHARACTER *(*) TKBD
      CHARACTER *9 TP(7),TAL(-3:3),TCH
      DATA TP/'rho-phi  ',          ! YX 1
     &        'theta    ',          ! YZ 2
     &        'rho-theta',          ! RZ 3
     &        'phi-theta',          ! FT 4
     &        'phi-theta',          ! FZ 5
     &        'rho-phi  ',          ! FR 6
     &        '         '/          ! RO 7
      DATA TAL/'theta-phi',         ! AL TF -3
     &         'phi-theta',         ! AL FT -2
     &         '         ',         !       -1
     &         '         ',         ! AL RO  0
     &         '         ',         ! AL AC  1
     &         'U-theta  ',         ! AL UT  2
     &         'theta_U  '/         ! AL TU  3
      DIMENSION H0(4),V0(4)
      DATA H0/-8.,8.,-8.,8./,V0/-8.,8.,8.,-8./
      DATA ROMIN/0.1/,IPIC/0/
      CHARACTER *2 TPIC1,TPIC2,TH(2),TV(2)
      DATA TH/'=H','=V'/,TV/'=V','=H'/
      LOGICAL FBUT(4),F1,F2,F4
      DATA F1,F2/2*.TRUE./,F4/.FALSE./
      DATA RO,FI,TE/100.,45.,45./
      CHARACTER *1 TM(4),TT(2)
      DATA TM/'X','J','M','N'/,TT/'C','R'/
      DIMENSION NFB1(4),NFB2(4)
      DATA NFB1/3,3,2,3/,NFB2/4,4,4,3/,NFB3/1/,MORZ/2/
      IF(FBUT(1).AND.FBUT(2).AND.FBUT(3).AND.FBUT(4)) THEN
C       ............................................... NO BUTTON PRESSED
        F1=.TRUE.
        F2=.TRUE.
        F4=.TRUE.
        HD=IHC
        VD=IVC
        IF(MODE.LT.3.OR.TCH(1:1).EQ.' ') THEN
C         ...................................................... single cross
C         .............. CROSS ONLY (MODE=1) OR JUMP (MODE=2) OR NO PROJECTION
          DO IW=0,12
            IF(ISTODS(4,IW,IWUSDO).GT.0) THEN
              IF(HMINDG(IW).LE.HD.AND.HD.LT.HHGHDG(IW).AND.
     &           VMINDG(IW).LE.VD.AND.VD.LT.VHGHDG(IW)) THEN
                CALL DQINV(IW,HD,VD,HU,VU)
                IPIC=ISTODS(5,IW,IWUSDO)
                TPIC1=TPICDP(IPIC)
                IHV=1
                IF(TPIC1.EQ.'AP')  THEN
                  NALP1=PSTODS(1,J_APR,IW,IWUSDO)
                  IF(NALP1.GT.3) GO TO 898
                  TCH=TAL(NALP1)
                  IALP1=IABS(NALP1)
                  IF(IALP1.EQ.3) IHV=2
                ELSE
                  IF(IPIC.GT.7) GO TO 898
                  TCH=TP(IPIC)
                END IF
                WRITE(TXTADW,1010) IHC,IVC,TAREDO(IW),TPICDP(IPIC),
     &            HU,TH(IHV),VU,TV(IHV),TCH
 1010           FORMAT(I5,I5,1X,A,':',A,':',
     &            F9.2,A,F9.2,A,2X,A,'>')
                CALL DWR_OVER_PRINT(51)
                IDIF=2
                GO TO 900
              END IF
            END IF
          END DO
          IF(VD.GE.VMINDG(13)) IW=13
  898     TCH=' '
          WRITE(TXTADW,1011) IHC,IVC,TCH
          IDIF=1
 1011     FORMAT(I5,I5,1X,30X,A,'>')
          CALL DWR_OVER_PRINT(51)
  900     IHL=IHC
          IVL=IVC
        ELSE
C         ................................... after pushing the left button,
C         ............................ the window of 1 cross is used.
C         .......................................... CALCULTE FI,RO,TE
          IF(IW.LT.13) THEN
            CALL DQINV(IW,HD,VD,HU,VU)
            IF(     TPIC1.EQ.'FT') THEN                             ! FT
              FI=MOD(VU+3600.,360.)
              TE=MAX(1.,MIN(179.,-HU))
            ELSE IF(TPIC1.EQ.'YX') THEN                             ! XY
              FI=DATN2D(VU,HU)
              RO=MAX(ROMIN,SQRT(VU*VU+HU*HU))
            ELSE IF(TPIC1.EQ.'RZ') THEN                             ! RZ
              RO=MAX(ROMIN,ABS(VU))
              TE=DATN2D(RO,HU)
            ELSE IF(TPIC1.EQ.'FR') THEN                             ! FR
              FI=MOD(VU+3600.,360.)
              RO=MAX(ROMIN,HU)
            ELSE IF(TPIC1.EQ.'FZ') THEN                             ! FZ
              FI=MOD(VU+3600.,360.)
              TE=DATN2D(RO,HU)
            ELSE IF(TPIC1.EQ.'YZ') THEN                             ! YZ
              TE=DATN2D(RO,HU)
            ELSE IF(TPIC1.EQ.'AP') THEN                             ! AP
              IF(IALP1.GE.2) THEN
                TE=MAX(1.,MIN(179.,-HU))
                IF(ISTODS(6,IW,IWUSDO).EQ.0) THEN
                  FIMID=PSTODS(1,J_AFM,IW,IWUSDO)
                  IF(FIMID.GE.360.) FIMID=FIMID-360.
                  FT=VU-180.
                  IF(NALP1.GT.0) THEN
                    ST=SIND(TE)
                    FT=FT/ST
                  END IF
                  FI=FT+FIMID
                ELSE
                  FI=VU
                END IF
              END IF
            END IF
          ELSE
            FI=400.*VD/VHGHDG(13)
            FI=MOD(FI+3600.,360.)
            TE=180.-180.*HD/HHGHDG(13)
            TE=MAX(1.,MIN(179.,TE))
          END IF
C         .......................................... RECALCULATE ALL MARKERS
          NUMP=0
          NHV=1
          DO NW=1,12
            IF(ISTODS(4,NW,IWUSDO).GT.0) THEN
              NPIC=ISTODS(5,NW,IWUSDO)
              TPIC2=TPICDP(NPIC)
              CALL DQSET(NW,0.,0.)
              IF(     TPIC2.EQ.'FT') THEN                             ! FT
                IF(PSTODS(2,J_PRF,NW,IWUSDO).LT.0..AND.
     &             PSTODS(2,J_PRT,NW,IWUSDO).LT.0.) THEN
                  CALL DM3DFI(-TE,FI,NW,H,V,NHV,NUMP)
                ELSE
                  CALL DM3DHV(-TE,FI,NW,H,V,NHV,NUMP)
                END IF
              ELSE IF(TPIC2.EQ.'YX') THEN                             ! XY
                CF=COSD(FI)
                SF=SIND(FI)
                IF(FPRSDQ) THEN
                  HLOWDG(NW)=HMINDG(NW)
                  VLOWDG(NW)=VMINDG(NW)
                END IF
                CALL DM3DRP(SF,CF,RO,NW,H,V,NHV,NUMP)
              ELSE IF(TPIC2.EQ.'RZ') THEN                             ! RZ
                FPSQDQ=.TRUE.
                ST=SIND(TE)
                CT=COSD(TE)
                ZZ=RO*CT/ST
                IF(MORZ.EQ.1.OR.FPRSDQ) THEN
                  HLOWDG(NW)=HMINDG(NW)
                  VLOWDG(NW)=VMINDG(NW)
C                 ... As angles are not conserved in the rectangular fishey,
c                 ... the cross hair is used for RZ-fish-eye.
                  CALL DM3DHV(ZZ,RO,NW,H,V,NHV,NUMP)
                  IF(PSTODS(2,J_PTE,NW,IWUSDO).NE.0.)
     &              CALL DM3DHL(ZZ,-RO,NW,H,V,NHV,NUMP)
                ELSE
                  R=SQRT(RO*RO+ZZ*ZZ)
                  CALL DM3DRP(ST,CT,R,NW,H,V,NHV,NUMP)
                  IF(PSTODS(2,J_PTE,NW,IWUSDO).NE.0.)
     &              CALL DM3DRP(-ST,CT,R,NW,H,V,NHV,NUMP)
                END IF
                FPSQDQ=.FALSE.
              ELSE IF(TPIC2.EQ.'FR') THEN                             ! FR
                CALL DM3DFI(RO,FI,NW,H,V,NHV,NUMP)
              ELSE IF(TPIC2.EQ.'FZ') THEN                             ! FZ
                ZZ=RO*COSD(TE)/SIND(TE)
                CALL DM3DFI(ZZ,FI,NW,H,V,NHV,NUMP)
              ELSE IF(TPIC2.EQ.'YZ') THEN                             ! YZ
                FIYZ=PSTODS(1,J_PFY,NW,IWUSDO)
                SF=SIND(FIYZ)
                CF=COSD(FIYZ)
                XX=RO*COSD(FI)
                YY=RO*SIND(FI)
                ZZ=RO*COSD(TE)/SIND(TE)
                YX=-SF*XX+CF*YY
                IF(MORZ.EQ.1) THEN
                  CALL DM3DHV(ZZ,YX,NW,H,V,NHV,NUMP)
                ELSE
                  R=SQRT(YX*YX+ZZ*ZZ)
                  IF(R.GT.0.) THEN
                    SA=YX/R
                    CA=ZZ/R
                    CALL DM3DRP(SA,CA,R,NW,H,V,NHV,NUMP)
                  END IF
                END IF
              ELSE IF(TPIC2.EQ.'RO') THEN                             ! RO
                FIRO=    PSTODS(1,J_PFI,NW,IWUSDO)
                SF=SIND(FIRO)
                CF=COSD(FIRO)
C
                TERO=90.-PSTODS(1,J_PTE,NW,IWUSDO)
                ST=SIND(TERO)
                CT=COSD(TERO)
C
                GARO=    PSTODS(1,J_RAL,NW,IWUSDO)
                SG=SIND(GARO)
                CG=COSD(GARO)
C
                X1=RO*COSD(FI)
                Y1=RO*SIND(FI)
                Z1=RO*COSD(TE)/SIND(TE)
C
                X2= CF*X1+SF*Y1
                Y2=-SF*X1+CF*Y1
                X3= CT*X2+ST*Z1
                Z3=-ST*X2+CT*Z1
                Y3= CG*Y2+SG*Z3
                IF(MORZ.EQ.1) THEN
                  CALL DM3DHV(X3,Y3,NW,H,V,NHV,NUMP)
                ELSE
                  R=SQRT(X3*X3+Y3*Y3)
                  IF(R.GT.0.) THEN
                    SA=Y3/R
                    CA=X3/R
                    CALL DM3DRP(SA,CA,R,NW,H,V,NHV,NUMP)
                  END IF
                END IF
              ELSE IF(TPIC2.EQ.'AP') THEN                             ! AP
C               .......................... NALPR: 2=UT,  3=TU, 0=RO, 1=AC,
C               ..........................       -2=FT, -3=TF
                NALP2=PSTODS(1,J_APR,NW,IWUSDO)
                IALP2=IABS(NALP2)
                IF(IALP2.GE.2) THEN
                  IF(ISTODS(6,NW,IWUSDO).EQ.0) THEN
                    FIMID=PSTODS(1,J_AFM,NW,IWUSDO)
                    IF(FIMID.GE.360.) FIMID=FIMID-360.
                    FT=FI-FIMID
                    IF(     FT.LT.-180.) THEN
                      FT=FT+360.
                    ELSE IF(FT.GE. 180.) THEN
                      FT=FT-360.
                    END IF
                    IF(NALP2.GT.0) THEN
                      ST=SIND(TE)
                      FT=FT*ST
                    END IF
                    FT=FT+180.
                    IF(FT.GT.360.) FT=FT-360.
                  ELSE
                    FT=FI
                  END IF
                  CALL DM3DHV(-TE,FT,NW,H,V,NHV,NUMP)
                END IF
              END IF
            END IF
          END DO
          WRITE(TXTADW,1020) RO,FI,TE,TAREDO(IW),TCH
 1020     FORMAT(F9.2,'=RO',F7.1,'=FI',F7.1,'=TE',5X,A,': ',A,'>')
          CALL DWR_OVER_PRINT(51)
          IDIF=3
        END IF
        IF(MODE.LE.3) THEN
C         ....................................................... DRAW CROSS
          IF(MODE.EQ.1) THEN
            NHV=1
            NUMP=0
          END IF
          I=NHV
          DO K=1,4
            H(I)=HD+H0(K)
            V(I)=VD+V0(K)
            I=I+1
          END DO
          NUMPL=NUMP+2
        ELSE
          NUMPL=NUMP
        END IF
        RETURN
      ELSE
C       ...................................................... HANDLE BUTTONS
        IF(.NOT.FBUT(1)) THEN
          IF(F1) MODE=NFB1(MODE)
          F1=.FALSE.
          F2=.TRUE.
          F4=.TRUE.
          RETURN
        END IF
        IF(.NOT.FBUT(2)) THEN
          IF(F2) MODE=NFB2(MODE)
          F1=.TRUE.
          F2=.FALSE.
          F4=.TRUE.
          RETURN
        END IF
        IF(.NOT.FBUT(3)) THEN
          F1=.TRUE.
          F2=.TRUE.
          F4=.TRUE.
          MODE=NFB3
          RETURN
        END IF
        IF(.NOT.FBUT(4)) THEN
          F1=.TRUE.
          F2=.TRUE.
          IF(TKBD.EQ.'<') THEN
            IF(F4) CALL DQHLP('<<')
            RETURN
          END IF
          IF(TKBD.EQ.'H') THEN
            IF(F4) CALL DQHLP('<+')
            RETURN
          END IF
          IF(     TKBD.EQ.'F') THEN
            IF(F4) THEN
C             CALL DWRT_SETUP('TERMINAL=OFF')
              CALL DWRT(TXTADW(1:50))
C             CALL DWRT_SETUP('TERMINAL=ON')
C             CALL DWRT_SETUP('LOGFILE=OFF')
C             CALL DWRT(' ')
C             CALL DWRT_SETUP('LOGFILE=ON')
              NDIF=IDIF
              IF(IDIF.LT.3) THEN
                IHLOL=IHL
                IVLOL=IVL
                IF(IDIF.EQ.2) THEN
                  HUOLD=HU
                  VUOLD=VU
                END IF
              ELSE
                ROOLD=RO
                FIOLD=FI
                TEOLD=TE
              END IF
              F4=.FALSE.
            END IF
            RETURN
          ELSE IF(TKBD.EQ.'D') THEN
            IF(F4) THEN
              IF(NDIF.EQ.IDIF) THEN
                IF(     NDIF.EQ.1) THEN
                  IDH=IHLOL-IHL
                  IDV=IVLOL-IVL
                  WRITE(TXTADW,2010) IDH,IDV
                  CALL DWRC
                ELSE IF(NDIF.EQ.2) THEN
                  IDH=IHLOL-IHL
                  IDV=IVLOL-IVL
                  DHU=HUOLD-HU
                  DVU=VUOLD-VU
                  WRITE(TXTADW,2010) IDH,IDV,DHU,DVU
 2010             FORMAT(I5,I5,7X,F9.2,F11.2)
                  CALL DWRC
                ELSE IF(NDIF.EQ.3) THEN
                  DRO=ROOLD-RO
                  DFI=FIOLD-FI
                  IF(DFI.GT. 180.) DFI=DFI-360.
                  IF(DFI.LE.-180.) DFI=DFI+360.
                  DTE=TEOLD-TE
                  WRITE(TXTADW,2020) DRO,DFI,DTE
 2020             FORMAT(F9.2,2F10.1)
                  CALL DWRC
                END IF
                CALL DWRT_SETUP('LOGFILE=OFF')
                CALL DWRT(' ')
                CALL DWRT_SETUP('LOGFILE=ON')
              ELSE IF(NDIF*IDIF.EQ.2) THEN
C               ................ : ONE = 1 AND THE OTHER = 2
                IDH=IHLOL-IHL
                IDV=IVLOL-IVL
                WRITE(TXTADW,2010) IDH,IDV
                CALL DWRC
                CALL DWRT_SETUP('LOGFILE=OFF')
                CALL DWRT(' ')
                CALL DWRT_SETUP('LOGFILE=ON')
              END IF
              F4=.FALSE.
            END IF
            RETURN
          END IF
          DO M=1,4
            IF(TKBD.EQ.TM(M)) THEN
              MODE=M
              RETURN
            END IF
          END DO
          DO M=1,2
            IF(TKBD.EQ.TT(M)) THEN
              MORZ=M
              RETURN
            END IF
          END DO
          RETURN
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
CH -------------------------------------------------------------------- DM3D0
CH
      ENTRY DM3D0(H,V,HC,VC)
CH
CH --------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFY,J_PFI,J_PTE,J_PRF,J_PRT,J_RAL'
      CALL DPARAM(10
     &  ,J_PFY,J_PFI,J_PTE,J_PRF,J_PRT,J_RAL)
      TPARDA=
     &  'J_AFM,J_APR'
      CALL DPARAM(39
     &  ,J_AFM,J_APR)
      TPARDA=
     &  'J_OM3'
      CALL DPARAM(84
     &  ,J_OM3)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_OM3).GE.0.) THEN
        MORZ=PARADA(2,J_OM3)
        PARADA(4,J_OM3)=-PARADA(4,J_OM3)
      END IF
      IW=-1
      HC=0.5*(HMINDG(IAREDO)+HHGHDG(IAREDO))
      VC=0.5*(VMINDG(IAREDO)+VHGHDG(IAREDO))
      DO K=1,4
        H(K)=HC+H0(K)
        V(K)=VC+V0(K)
      END DO
      MODE=1
      NDIF=0
      NHV=1
      NUMP=0
      F1=.TRUE.
      F2=.TRUE.
      F4=.FALSE.
      CALL DWRT('Move mouse')
      END
*DK DM3DFI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM3DFI
CH
      SUBROUTINE DM3DFI(HU,VU,NW,H,V,NHV,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :HU,VU = CROSS HAIR POSITIONS
C    Outputs   :H,V CROSS HAIR COORDINATES FOR VERTICAL AND HORIZONTAL LINES
C               LINE AT FI=FI+360.
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FIN
      DIMENSION H(*),V(*)
      VP=VU+360.
      CALL DQPOC(HU,VP,HD1,VD1,FIN)
C     ....................................................... HL+360
      IF(VMINDG(NW).LT.VD1.AND.VD1.LT.VHGHDG(NW)) THEN
        NUM=NUM+1
        H(NHV)=HMINDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
        H(NHV)=HHGHDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
      END IF
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM3DHV
CH
      ENTRY DM3DHV(HU,VU,NW,H,V,NHV,NUM)
CH
C    Outputs   :H,V CROSS HAIR COORDINATES FOR VERTICAL AND HORIZONTAL LINE
C    NO LINE AT FI=FI+360.
CH --------------------------------------------------------------------
      CALL DQPOC(HU,VU,HD1,VD1,FIN)
C     ....................................................... HL
      IF(VMINDG(NW).LT.VD1.AND.VD1.LT.VHGHDG(NW)) THEN
        NUM=NUM+1
        H(NHV)=HMINDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
        H(NHV)=HHGHDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
      END IF
C     ....................................................... VL
      IF(HMINDG(NW).LT.HD1.AND.HD1.LT.HHGHDG(NW)) THEN
        NUM=NUM+1
        V(NHV)=VMINDG(NW)
        H(NHV)=HD1
        NHV=NHV+1
        V(NHV)=VHGHDG(NW)
        H(NHV)=HD1
        NHV=NHV+1
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM3DHV
CH
      ENTRY DM3DHL(HU,VU,NW,H,V,NHV,NUM)
CH
C    Outputs   :H,V CROSS HAIR COORDINATES FOR A HORIZONTAL LINE
CH --------------------------------------------------------------------
      CALL DQPOC(HU,VU,HD1,VD1,FIN)
C     ....................................................... HL
      IF(VMINDG(NW).LT.VD1.AND.VD1.LT.VHGHDG(NW)) THEN
        NUM=NUM+1
        H(NHV)=HMINDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
        H(NHV)=HHGHDG(NW)
        V(NHV)=VD1
        NHV=NHV+1
      END IF
      END
*DK DM3DRP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM3DRP
CH
      SUBROUTINE DM3DRP(SA,CA,RO,NW,H,V,NHV,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :HU,VU = CROSS HAIR POSITIONS
C    Outputs   :H,V CROSS HAIR COORDINATES FOR VERTICAL AND HORIZONTAL LINES
C               LINE AT FI=FI+360.
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*)
      DATA DP/8./,RE/65536./
      LOGICAL FIN,FIP
      XE=RE*CA
      YE=RE*SA
      XR=RO*CA
      YR=RO*SA
      CALL DQPOC(0.,0.,HD1,VD1,FIN)
      CALL DQPOC(XE,YE,HD3,VD3,FIN)
      CALL DQPOC(XR,YR,HD2,VD2,FIN)
C     .................................................. RL
      IHV=NHV
      H(NHV)=HD1
      V(NHV)=VD1
      NHV=NHV+1
      H(NHV)=HD3
      V(NHV)=VD3
      CALL DQCL0(HLOWDG(NW),VLOWDG(NW),HHGHDG(NW),VHGHDG(NW),1.)
      CALL DQCLP(H(IHV),V(IHV),H(NHV),V(NHV),FIP)
      IF(FIP) THEN
        NUM=NUM+1
        NHV=NHV+1
      ELSE
        NHV=IHV
      END IF
C     .................................................. PL
      IF(FIN) THEN
        NUM=NUM+1
        DPC=DP*CA
        DPS=DP*SA
        H(NHV)=HD2+DPS
        V(NHV)=VD2-DPC
        NHV=NHV+1
        H(NHV)=HD2-DPS
        V(NHV)=VD2+DPC
        NHV=NHV+1
      END IF
      END
*DK DM2D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM2D
CH
      SUBROUTINE DM2D(TANSW,FYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Draw phi cursors on XY,FR,FT,FZ for RZ
C    Called by : DKRZ
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TMM(5),TANSW,TPIC
      DATA TMM/'MM','XY','YX','TF','FT'/
      PARAMETER (NLIN=18)
      DIMENSION H(2*NLIN),V(2*NLIN),IH(2*NLIN),IV(2*NLIN),NDSEG(2,NLIN)
      DATA N8/8/,S1/1./,K3/3/
      EXTERNAL DM2DM
      LOGICAL FYES,FIN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DO M=1,5
        IF(TANSW.EQ.TMM(M)) THEN
          KK=1
          DO K=1,NLIN
            NDSEG(1,K)=KK
            NDSEG(2,K)=2
            KK=KK+2
          END DO
          AF=PARADA(2,J_PFI)
          DO MW=0,12
            IW=MOD(IAREDO+MW,13)
            IF(ISTODS(4,IW,IWUSDO).GT.0) THEN
              IPIC=ISTODS(5,IW,IWUSDO)
              TPIC=TPICDP(IPIC)
              RZ=0.4*PSTODS(1,J_PTO,IW,IWUSDO)
              IF(ISTODS(6,IW,IWUSDO).EQ.1)
     &            RZ=RZ+0.6*PSTODS(1,J_PFR,IW,IWUSDO)
              IF(     TPIC.EQ.'YX') THEN                             ! XY
                SA=SIND(AF)
                CA=COSD(AF)
                HU=COSD(AF)*RZ
                VU=SIND(AF)*RZ
                GO TO 3
              ELSE IF(TPIC.EQ.'FT') THEN                             ! FT
                HU=-PSTODS(1,J_PTE,IW,IWUSDO)
                VU=PARADA(2,J_PFI)
                GO TO 3
              ELSE IF(TPIC.EQ.'FR'.OR.TPIC.EQ.'FZ') THEN         ! FR, FZ
                HU=RZ
                VU=PARADA(2,J_PFI)
                GO TO 3
              END IF
            END IF
          END DO
          CALL DWRT('Neither YX nor FT, FR, FZ are on the screen')
          RETURN
    3     CALL DQSET(IW,0.,0.)
          CALL DQPOC(HU,VU,HD,VD,FIN)
          CALL DGSCUR(HD,VD)
          CALL DM2D0(AF)
          IHC=HD
          IVC=VD
          NUMPL=0
          CALL DO_BAR_ANSWER_PLATFORM_TEXT('2m')
          CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DM2DM,.FALSE.,NDSEG,NUMPL)
          CALL DM2D1(AF)
          PARADA(2,J_PFI)=AF
          FYES=.TRUE.
          RETURN
        END IF
      END DO
      FYES=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM2DF
CH
      ENTRY DM2DF
CH
C     called by dmps
CH --------------------------------------------------------------------
      DLIN=DLINDD
      DLINDD=S1
      CALL DGLEVL(N8)
      DO K=K3,NUMPL
        CALL DGDRAW(NDSEG(2,K),H(NDSEG(1,K)),V(NDSEG(1,K)))
      END DO
      DLINDD=DLIN
      END
*DK DM2DM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM2DM
CH
      SUBROUTINE DM2DM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    MOVE CROSS AND MARKERS
C    Called by : DGTLNM after DM2D
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),NDSEG(2,*)
      CHARACTER *(*) TKBD
      CHARACTER *2 TPIC1,TPIC2
      DIMENSION H0(4),V0(4)
      DATA H0/-8.,8.,-8.,8./,V0/-8.,8.,8.,-8./
      DATA P1/1./
      LOGICAL FBUT(4)
      HD=IHC
      VD=IVC
      DO K=1,4
        H(K)=HD+H0(K)
        V(K)=VD+V0(K)
      END DO
      NUMPL=2
      NHV=5
      DO IW=0,12
        IF(ISTODS(4,IW,IWUSDO).GT.0) THEN
          IF(HMINDG(IW).LE.HD.AND.HD.LT.HHGHDG(IW).AND.
     &       VMINDG(IW).LE.VD.AND.VD.LT.VHGHDG(IW)) THEN
            IPIC=ISTODS(5,IW,IWUSDO)
            TPIC1=TPICDP(IPIC)
            CALL DQINV(IW,HD,VD,HU,VU)
            IF(     TPIC1.EQ.'YX') THEN                             ! XY
              FI=DATN2D(VU,HU)
              GO TO 3
            ELSE IF(TPIC1.EQ.'FT'
     &          .OR.TPIC1.EQ.'FR'
     &          .OR.TPIC1.EQ.'FZ') THEN                     ! FT, FR, FZ
              FI=VU
              GO TO 3
            END IF
            RETURN
          END IF
        END IF
      END DO
      RETURN
    3 FI=MOD(FI+3600.,360.)
C     .......................................... RECALCULATE ALL MARKERS
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PTO'
      CALL DPARAM(11
     &  ,J_PTE,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DO NW=1,12
        IF(ISTODS(4,NW,IWUSDO).GT.0) THEN
          NPIC=ISTODS(5,NW,IWUSDO)
          TPIC2=TPICDP(NPIC)
          CALL DQSET(NW,0.,0.)
          IF(     TPIC2.EQ.'FT') THEN                             ! FT
            TE=-PSTODS(1,J_PTE,NW,IWUSDO)
            CALL DM2DFI(TE,FI,NW,H,V,NHV,NUMPL)
          ELSE IF(TPIC2.EQ.'YX') THEN                             ! XY
C           ........................................ FI=FI+90
            CF=-SIND(FI)
            SF= COSD(FI)
            CALL DM2DXY(SF,CF,NW,H,V,NHV,NUMPL)
          ELSE IF(TPIC2.EQ.'FZ'.OR.TPIC2.EQ.'FR') THEN          ! FR, FZ
            RZ=PSTODS(1,J_PTO,NW,IWUSDO)-P1
            CALL DM2DFI(RZ,FI,NW,H,V,NHV,NUMPL)
          END IF
        END IF
      END DO
      WRITE(TXTADW,1020) FI
 1020 FORMAT(F9.2,'=FI>')
      CALL DWR_OVER_PRINT(13)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM2D0
CH
      ENTRY DM2D0(AF)
CH
CH --------------------------------------------------------------------
      FI0=AF
      FI=FI0
      CALL DWRT(' ')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DM2D1
CH
      ENTRY DM2D1(AF)
CH
CH --------------------------------------------------------------------
      AF=FI
      END
*DK DM2DFI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM2DFI
CH
      SUBROUTINE DM2DFI(HU,FI,NW,H,V,NHV,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :HU,FI = USER POSITIONS
C    Outputs   :H,V CROSS HAIR COORDINATES FOR VERTICAL AND HORIZONTAL LINES
C               LINE AT FI=FI+360.
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FIN
      DATA F270/270./,F40/40./,F400/400./
      DIMENSION H(*),V(*)
      DO VU=FI-F270,F400,180.
        IF(VU.GE.-F40.AND.VU.LE.F400) THEN
          CALL DQPOC(HU,VU,HD1,VD1,FIN)
          IF(FIN)THEN
            NUM=NUM+1
            H(NHV)=HLOWDG(NW)
            V(NHV)=VD1
            NHV=NHV+1
            H(NHV)=HHGHDG(NW)
            V(NHV)=VD1
            NHV=NHV+1
          END IF
        END IF
      END DO
      END
*DK DM2DRP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM2DRP
CH
      SUBROUTINE DM2DXY(SA,CA,NW,H,V,NHV,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :HU,VU = CROSS HAIR POSITIONS
C    Outputs   :H,V CROSS HAIR COORDINATES FOR VERTICAL AND HORIZONTAL LINES
C               LINE AT FI=FI+360.
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*)
      DATA RE/65536./
      LOGICAL FIN
      XE=RE*CA
      YE=RE*SA
      CALL DQPOC(-XE,-YE,HD1,VD1,FIN)
      CALL DQPOC( XE, YE,HD2,VD2,FIN)
C     .................................................. RL
      IHV=NHV
      H(NHV)=HD1
      V(NHV)=VD1
      NHV=NHV+1
      H(NHV)=HD2
      V(NHV)=VD2
      CALL DQCL0(HLOWDG(NW),VLOWDG(NW),HHGHDG(NW),VHGHDG(NW),1.)
      CALL DQCLP(H(IHV),V(IHV),H(NHV),V(NHV),FIN)
      IF(FIN) THEN
        NUM=NUM+1
        NHV=NHV+1
      ELSE
        NHV=IHV
      END IF
      END
*DK DMCTYP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMCTYP
CH
      SUBROUTINE DMCTYP(ITR)
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
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION PTR(3),CLTM(3),VTX1(3),XDUM(3)
      CHARACTER *52 T
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      NTR=ITR
      CALL DVMCNV(NVTX,NDUM)
    1 CALL DVMCVX(1,KDUM,KDUM,KDUM,XDUM)
      CALL DVMCTR(NTR,PTR,CLTM,ITYPE,NVTX2)
      PT=PTR(1)**2+PTR(2)**2
      PTOT=SQRT(PT+PTR(3)**2)
      PT=SQRT(PT)
      IF(PT.GT.0.) THEN
         FI=DATN2D(PTR(2),PTR(1))
      ELSE
         FI=0.
      END IF
      IF(PTOT.GT.0.) THEN
         TE=DATN2D(PT,PTR(3))
      ELSE
         TE=0.
      END IF
C                 1         2         3         4         5
C        12345678901234567890123456789012345678901234567890123456789
      T='mc-track 123  PROTON  p=+.123 fi=123 te=123  from .:'
      T(10:12)=DT3(FLOAT(NTR))
      IF(ITYPE.LE.0.OR.ITYPE.GT.47) THEN
         T(14:21)='type='//DT3(FLOAT(ITYPE))
         T(23:43)=' '
      ELSE
         CALL DVPART(ITYPE,6,T(15:20))
         T(26:29)=DT4(PTOT)
         T(34:36)=DT3(FI)
         T(41:43)=DT3(TE)
         ICH=NINT(CLTM(1))
         IF(ICH.EQ.0) THEN
            T(25:25)=' '
         ELSE IF(ICH.EQ.-1) THEN
            T(25:25)='-'
         ELSE IF(ICH.EQ.1) THEN
            T(25:25)='+'
         ELSE
            T(25:25)='*'
         END IF
      END IF
    3 DO 700  N=1,NVTX
         CALL DVMCVX(N,KT1,KT2,KIN,VTX1)
         IF(KT1.LE.NTR.AND.NTR.LE.KT2) GO TO 2
  700 CONTINUE
    2 IF(N.EQ.1) T(46:49)='    '
      CALL DWRT(T)
      IF(N.NE.1) THEN
         NTR=KIN
         GO TO 1
      END IF
      END
*DK DMIX
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DMIX
CH
      FUNCTION DMIX(F1,F2,F3)
CH
CH ********************************************************************
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
      DMIX=MIN(MAX(F1,F2),F3)
      RETURN
      END
*DK DMESS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMESS
CH
      SUBROUTINE DMESS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'MS'  (DMESS)
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *14 TFMES, TFNEW
      CHARACTER  TM*45, TDT*11, TTM*8
      CHARACTER * 80 TALL
      LOGICAL FOUT
      LOGICAL FILOP
      DATA TFMES /'MYMES.DAT'/ ,FILOP /.FALSE./
C
      IF(FILOP) GO TO 6
  930 IF(FILOP) THEN
        CALL DWRT('MS: message on file '//TFMES//'. File is open.')
      ELSE
        CALL DWRT('MS: message on file '//TFMES//'. NOT open yet')
        CALL DWRT('Do you want to APpend or get a NEw message file?')
      END IF
  936 CALL DOPER(0,0,
     &  1,0,' ',0,
     &  1,0,' ',0,
     &  NEXEC,FCHNG,TANSW)
      GO TO (910,920,931,940) NEXEC
  931 IF(FILOP) GO TO 930
  910 RETURN
  940 IF(FILOP) GO TO 6
      CALL DGOPEN(24,TFMES,3,*942,ISTAT)
      GO TO 941
  942 CALL DGOPEN(24,TFMES,1,*199,ISTAT)
      GO TO 941
  920 IF(TANSW.EQ.'NA') THEN
        CALL DWRT('Name of message file? <CR>='//TFMES)
        CALL DGETLN(TFNEW,NBC,LEN(TFNEW))
        IF(NBC.GT.2.AND.TFNEW.NE.TFMES) GO TO 22 
        GO TO 930
      END IF
      IF(TANSW.EQ.'CF') THEN
        TFNEW=TFMES
        GO TO 22
      END IF
      IF(TANSW.EQ.'RD') THEN
        IF(FILOP) THEN
          REWIND(24)
        ELSE
          CALL DGOPEN(24,TFMES,1,*199,ISTAT)
        END IF
        FOUT=.TRUE.
   44   DO L=1,20
          READ(24,1044,END=47) TALL
 1044     FORMAT(A)
          LALL=LENOCC(TALL)
          IF(FOUT) CALL DWRT(TALL(1:LALL))
        END DO
        CALL DGETLN(TALL,LALL,LEN(TALL))
        IF(LALL.GT.0) FOUT=.FALSE.
        GO TO 44
   47   IF(.NOT.FILOP) CLOSE(UNIT=24)
        GO TO 930
      END IF
      IF(TANSW.EQ.'AP') THEN
        IF(FILOP) GO TO 51
        CALL DGOPEN(24,TFMES,3,*199,ISTAT)
      ELSE IF(TANSW.EQ.'NE') THEN
        IF(FILOP) GO TO 51
        CALL DGOPEN(24,TFMES,1,*199,ISTAT)
      ELSE
        CALL DWR_IC(TANSW)
        GO TO 936
      END IF
  941 FILOP=.TRUE.
      CALL DATE4(TDT)
      CALL TIME(TTM)
      WRITE(TXTADW,1022) 'OPEN',TDT,TTM(1:5)
 1022 FORMAT('      0,     0, ',A,T34,':',28X,A,' ',A)
      CALL DWRT(TXTADW,'TL')
    6 CALL DQHLP('ME ')
      DO L2=80,1,-1
        IF(TFINDE(1)(L2:L2).NE.' ') GO TO 60
      END DO
   60 DO L1=L2,1,-1
        IF(TFINDE(1)(L1:L1).EQ.':'.OR.TFINDE(1)(L1:L1).EQ.'[') GO TO 61
      END DO
      L1=0
   61 L1=MAX(L1+1,L2-15)
      CALL DWRT('MS:enter your message. Stop with <CR><CR>.')
      DO NN=1,99
        IF(NN.EQ.1) THEN
          WRITE(TXTADW,1006) IRUNDE(1),IEVTDE(1),TFINDE(1)(L1:L2)
 1006     FORMAT('R',I7,' E',I6,' ',A,T34,':')
          CALL DWRC
        ELSE
          CALL DWRT('                                 ')
        END IF
        CALL DGETLN(TM,NCH,LEN(TM))
        IF (NCH.LE.0) GO TO 930
        IF(NN.EQ.1) THEN
          WRITE(24,1008) IRUNDE(1),IEVTDE(1),TFINDE(1)(L1:L2),TM
        ELSE
          WRITE(24,1008)         0,        0,'              ',TM
        END IF
 1008   FORMAT(1X,I7,', ',I6,', ',A,T34,'!',A)
      END DO
      GO TO 930
   51 CALL DWRT(' File already open.')
      GO TO 936
  199 CALL DWRT(TFMES//' doesn''t exist.')
      GO TO 930
   22 IF(FILOP) THEN
        CALL TIME(TTM)
        CALL DWRT(TFMES//' closed')
        WRITE(24,1022) 'CLOSED',TDT,TTM(1:5)
        CLOSE(UNIT=24)
        FILOP=.FALSE.
      END IF
      TFMES=TFNEW
      GO TO 930
      END
*DK DMPS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DMPS
CH
      SUBROUTINE DMPS(TACOL,TATXT,TACF)
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH


*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_UIS.INC'
C
C
      CHARACTER *(*) TACOL,TACF,TATXT
      DATA P4MA/1./
      CHARACTER *2 TANSW,TAR,TABGC(-1:3)
      DATA TABGC/'  ','  ','UB','UG','UC'/
      CHARACTER *3 TBGC
      DATA TBGC/'BGC'/
      CHARACTER *7 DT7, TRUN, TEVT
      CHARACTER *18 TCOM
      CHARACTER *19 TS
      PARAMETER (MP1=1)
      DIMENSION PR1(4,MP1)
      DATA PR1/0.,.8,1.,1./
      CHARACTER *2 TP1(MP1)
      DATA TP1/'FG'/,IBGC/1/
      LOGICAL CHG, FGKS/.FALSE./,FYES,FTXT,FPS,FSTRT
      DATA FSTRT/.TRUE./
      CHARACTER *49 T1,T2,T3
C              123456789 123456789 123456789 123456789 123456789 
C          T1/'P?:W?: window 1 2 3 4 5 6 U D L M R S W     TX HD
C          T1/'P?:W?:  WW W1 W2 W3 W4 W5 W6 WU WD WL WM WR WS HD'

C          T2/'FG=123 UU   suspended'
C          T2/'FG=123 UU   not open'

C          T3/'DP000000_000000_no_comment_.PS_951021_0912'/
C          T3/'DP012345_012345_TA-MU_TA-EL.PS_951021_0912'/
C              123456789 123456789 123456789 123456789 123456789
C          T3/'DP012345_012345_951021_0912.PS_TA-MU_TA-EL'/

      DATA T1/'P?:W?:'/
      DATA T2/'FG$123      not open'/
      DATA T3/'D____________________________.PS                 '/
C
      IF(TACOL.EQ.' ') THEN
        CALL DWR_PLATFORM_TEXT('PS')
        IF(FSTRT) THEN
    1     CALL DTYANS(
     &      'Read! Type "C" to continue ,  <cr> to go back.#',
     &      'C',NANSW)
          IF(NANSW.LT.0) THEN
            GO TO 1
          ELSE IF(NANSW.EQ.0) THEN
            CALL DWRT('Nothing stored.')
            TPICDO='GB'
            RETURN
          END IF
          FSTRT=.FALSE.
        END IF
      END IF
      CALL DCOPTL
      IF(T2(13:15).EQ.'not') CALL DW_TIME(T3(16:31))
  900 IF(T2(13:15).EQ.'not') THEN
        TRUN=DT7(FLOAT(1000000+IRUNDE(1)))
        TEVT=DT7(FLOAT(1000000+IEVTDE(1)))
        T3( 3: 8)=TRUN(2:)
        T3(10:15)=TEVT(2:)
        IRLST=IRUNDE(1)
        IELST=IEVTDE(1)
C        NAUTO=NAUT
C        IF(NAUTO.GT.0) THEN
C          CALL DGCLWK
C          CALL DGCHKX
C          TANSW='OP'
C          CALL DTYPT('TYPE',TPICDO,1,0,0,' ',T1)
C          PCMA=PDCODD(4,ICMADD)
C          IF(NAUTO.EQ.2) PDCODD(4,ICMADD)=-1.
C          GO TO 920
C        END IF
      ELSE IF(IRUNDE(1).NE.IRLST.OR.IEVTDE(1).NE.IELST) THEN
        CALL DWRT('Name of file has wrong run/event number.')
        CALL DWRT('If so, close old file.')
      END IF
  930 CALL DO_BAR_STATUS_0
      CALL DTYPT('TYPE',TPICDO,1,0,0,' ',T1)
      T2(8:9)=TABGC(IBGC)
      CALL DTYPT('TYPE',' ',1,MP1,PR1,TP1,T2)
      CALL DWRT(T3)
      IF(T2(13:15).EQ.'not') THEN
        CALL DO_BAR_STATUS('NO',0,' ')
      ELSE
        CALL DO_BAR_STATUS('OP',0,' ')
      END IF
  936 CHG=.FALSE.
      CALL DOPER(0,0,
     &  1,MP1,TP1,PR1,
     &  1,0,' ',0,
     &  NEXEC,CHG,TANSW)
      IF(CHG.AND.FBLWDT) THEN
        IF(PR1(4,1).GT.0.) THEN
          GREY=PR1(2,1)
        ELSE
          GREY=1.
        END IF
        RDCODD(0)=GREY
        GRCODD(0)=GREY
        BLCODD(0)=GREY
        CALL DW_SET_CO
      END IF
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
      RETURN
  920 IF(TANSW.EQ.'OP'.OR.TANSW.EQ.'OB') THEN
C                   123456789 123456789 123456789 123456789 123456789
        CALL DWRT('Colours can be modified editing the PS-file,')
        CALL DWRT('which contains a B/W, gray and color table.')
        IF(.NOT.FMONDT) THEN
CA        IF(NAUTO.LT.2) THEN
          IF(TACOL.EQ.' ') THEN
            CALL DWRT('A Black+white, Grey and Color table is stored.')
            CALL DTYANS('Which one shall be set to true? B/G/C?',
     &        TBGC,IBGC)
            IF(IBGC.LE.0) THEN
              GO TO 930
CA              IF(NAUTO.EQ.0) GO TO 930
CA              TPICDO='GB'
CA              RETURN
            END IF
CA          ..                              ELSE IF(IBGC.EQ.2) THEN
CA          ..                                IF(IBGC.EQ.3) IBGC=1
          ELSE
            DO K=1,3
              IF(TACOL.EQ.TBGC(K:K)) IBGC=K
            END DO
          END IF
        ELSE
          IBGC=1
        END IF
        T3(2:2)=TBGC(IBGC:IBGC)
        IF(TANSW.EQ.'OP') THEN
          CALL DGNAME(T3)
        ELSE
          CALL DGNAME(T3(1:2)//T3(16:))
        END IF
        CALL DGOPME(4,IBGC)
        CALL DWRT('PostScript colour metafile opened.')
C                  123456789
        LSTOR=0
        T1(9:)=' '
        T2(13:21)='suspended'
        FPSWDU=.FALSE.
        CALL DGBTIT
        GO TO 930
CA      IF(NAUTO.EQ.0) GO TO 930
CA      TANSW='SA'
      END IF
      IF(TANSW.EQ.'CO') THEN
        CALL DWR_PLATFORM_TEXT('KW')
  921   CALL DWRT(T3)
        CALL DO_BAR_WRT(5.,5,
     &    'Type commemt to be added to end of filename')
        CALL DGETLN(TCOM,L1,18)
        IF(L1.GT.0) THEN
          DO L=1,L1
            IF(TCOM(L:L).EQ.' ') THEN
              TCOM(L:L)='_'
            ELSE
              IF(  TCOM(L:L).NE.'_'
     &        .AND.TCOM(L:L).NE.'-'
     &        .AND.TCOM(L:L).NE.'$') THEN
                IL=ICHAR(TCOM(L:L))
                IF(             IL.LT.48 .OR.
     &            (IL.GT.57.AND.IL.LT.65).OR.
     &            (IL.GT.90.AND.IL.LT.97).OR.
     &             IL.GT.122) THEN
                  CALL DWRT('Wrong comment.')
                  GO TO 921
                END IF
              END IF
            END IF
          END DO
          T3(31:49)='_'//TCOM
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'LL'.AND.T2(13:15).EQ.'not') THEN
        CALL DGETLN(TCOM,L1,1)
        IF(L1.GT.0) T3(1:1)=TCOM(1:1)
        GO TO 930
      END IF
c      IF(TANSW.EQ.'KW') THEN
c        CALL DWR_PLATFORM_TEXT('KW')
c        GO TO 930
c      END IF
      DO K=1,3
        IF(TANSW.EQ.TABGC(K)) THEN
          IBGC=K
          GO TO 930
        END IF
      END DO
      IF(TANSW.EQ.'PC'.OR.
     &   TANSW.EQ.'DC'.OR.
     &   TANSW.EQ.'XC'.OR.
     &   TANSW.EQ.'PP') THEN
        CALL DCDPC(TANSW)
        GO TO 936
      END IF
      IF(TANSW.EQ.'BW') THEN
        IF(.NOT.FBLWDT) CALL DCDBW
        IF(FBLWDT.AND.PR1(4,1).GT.0.) THEN
          RDCODD(0)=PR1(2,1)
          GRCODD(0)=PR1(2,1)
          BLCODD(0)=PR1(2,1)
          CALL DW_SET_CO
        END IF
        GO TO 936
      END IF
      IF(TANSW.EQ.'BP') THEN
        PDCODD(4,ICBPDD)=-PDCODD(4,ICBPDD)
        GO TO 930
      END IF
      IF(TANSW.EQ.'MA') THEN
        PDCODD(4,ICMADD)=-PDCODD(4,ICMADD)
        GO TO 930
      END IF
C      IF(TANSW.EQ.'M0') THEN
C        PDCODD(4,ICMADD)=-1.
C        GO TO 930
C      END IF
      IF(TANSW.EQ.'M0') THEN
        P4MA=PDCODD(4,ICMADD)
        PDCODD(4,ICMADD)=-1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'M1') THEN
        PDCODD(4,ICMADD)=P4MA
        GO TO 930
      END IF
      IF(TANSW.EQ.'SP') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
        ELSE
          CALL DGSPME
C                    123456789
          T2(13:21)='suspended'
          FPSWDU=.FALSE.
          CALL DGBTIT
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'RS') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
        ELSE
          CALL DGRSME
          CALL DWRT('Resume storing PS file.')
C                    123456789
          T2(13:21)='    store'
          FPSWDU=.TRUE.
          CALL DGBTIT
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'CW') THEN
        LCBG=PDCODD(2,ICBGDD)
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
          CALL DQFWAF(LCBG)
        ELSE
          FPS=FPSWDU
          FPSWDU=.TRUE.
          CALL DGRSME
          CALL DQFWAF(LCBG)
          CALL DGSPME
          FPSWDU=FPS
        END IF
        GO TO 936
      END IF
      IF(TANSW.EQ.'CF') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
        ELSE
          IF(TACF.EQ.' ') THEN
            CALL DWRT(T1(6:49))
            CALL DTYANS('Continue storing "C" or close the file "Y" ?',
     &        'Y',NANSW)
          ELSE
            IF(TACF.EQ.'Y') THEN
              NANSW=1
            ELSE
              NANSW=0
            END IF
          END IF
          IF(NANSW.EQ.1) THEN
            CALL DGCSME
C                      123456789
            T2(13:21)='not open'
            FPRIDT = .FALSE.
            T1(8:)=' '
            FPSWDU=.FALSE.
            CALL DGBTIT
            GO TO 900
          ELSE
            CALL DWRT('File not closed!')
          END IF
          GO TO 930
        END IF
      END IF
      IF(TANSW.EQ.'ST') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
          CALL DTXRA(TATXT,FTXT)
        ELSE
          IF(FPSWDU) THEN
            CALL DTXRA(TATXT,FTXT)
          ELSE
            FPSWDU=.TRUE.
            CALL DGBTIT
            CALL DGRSME
            CALL DTXRA(TATXT,FTXT)
            CALL DGSPME
            FPSWDU=.FALSE.
            IF(FTXT) THEN
              IF(T1(48:49).EQ.'  ') THEN
                T1(48:49)='TX'
                CALL DWRT('Text stored on PostScript file.')
              ELSE
                CALL DWRT('text stored on PostScript file again.')
              END IF
            END IF
            CALL DGBTIT
          END IF
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'SH'.OR.TANSW.EQ.'SA') THEN
        IFULDB=1
        IF(T2(13:15).EQ.'not') THEN
          CALL DWRT('PostScript file not open!')
          CALL DQTIT(IFULDB)
        ELSE
          IF(FPSWDU) THEN
            CALL DQTIT(IFULDB)
          ELSE
            FPSWDU=.TRUE.
            CALL DGRSME
            CALL DGBTIT
            CALL DQTIT(IFULDB)
            CALL DGSPME
            FPSWDU=.FALSE.
            CALL DGBTIT
          END IF
        END IF
        IF(TANSW.EQ.'SH') GO TO 930
        GO TO 940
      END IF
      IF(TANSW.EQ.'M2') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DM2DF
        ELSE
          IF(FPSWDU) THEN
          CALL DM2DF
        ELSE
          FPSWDU=.TRUE.
          CALL DGRSME
          CALL DM2DF
          CALL DGSPME
          FPSWDU=.FALSE.
          END IF
        END IF
        GO TO 936
      END IF
      IF(TANSW.EQ.'M3') THEN
        IF(T2(13:15).EQ.'not') THEN
          CALL DM3DF
        ELSE
          IF(FPSWDU) THEN
          CALL DM3DF
        ELSE
          FPSWDU=.TRUE.
          CALL DGRSME
          CALL DM3DF
          CALL DGSPME
          FPSWDU=.FALSE.
          END IF
        END IF
        GO TO 936
      END IF
      CALL DAREA('S',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
      CALL DWR_IC(TANSW)
      GO TO 930
  940 IF(T2(13:15).EQ.'not') THEN
        CALL DWRT('PostScript file not open!')
        IF(TANSW.EQ.'SA') THEN
          CALL DPCEAR(0)
          CALL DTXRA(TATXT,FTXT)
        ELSE
          CALL DPCEWI(IAREDO)
        END IF
      ELSE
        IF(FPSWDU) THEN
          IF(TANSW.EQ.'SA') THEN
            CALL DO_BAR_WRT(5.,5,'Store all. Wait')
            CALL DPCEAR(0)
            CALL DTXRA(TATXT,FTXT)
          ELSE
            TS='Store window '//TWINDW(IAREDO)//' Wait'
            CALL DO_BAR_WRT(5.,5,TS)
            CALL DPCEWI(IAREDO)
          END IF
        ELSE
          FPSWDU=.TRUE.
          CALL DGBTIT
          CALL DGRSME
          IF(TANSW.EQ.'SA') THEN
            CALL DO_BAR_WRT(5.,5,'Store all. Wait')
            CALL DPCEAR(0)
            CALL DTXRA(TATXT,FTXT)
          ELSE
            TS='Store window '//TWINDW(IAREDO)//' Wait'
            CALL DO_BAR_WRT(5.,5,TS)
            CALL DPCEWI(IAREDO)
          END IF
          CALL DGSPME
          FPSWDU=.FALSE.
          CALL DGBTIT
        END IF
        IF(FTXT) THEN
          IF(T1(48:49).EQ.'  ') THEN
            T1(48:49)='TX'
            CALL DWRT('Text stored on PostScript file.')
          ELSE
            CALL DWRT('text stored on PostScript file again.')
          END IF
        END IF
      END IF
      TPICDO='PS'
      GO TO 930
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DMPSWI
CH
      ENTRY DMPSWI(NDAR)
CH
CH --------------------------------------------------------------------
CH
C     .................................. SET DUMMY IF DALI-BATCH IS REMADE
      IF(FPSWDU) THEN
        IF(LSTOR.EQ.0) THEN
          T1(8:13)='window'
          T1(41:46)='stored'
          LSTOR=13
        END IF
        TAR=TAREDO(NDAR)
        DO L=15,LSTOR
          IF(T1(L:L).EQ.TAR(2:2)) THEN
            CALL DWRT(TAR//' stored on PostScript file again.')
            RETURN
          END IF
        END DO
        LSTOR=LSTOR+2
        IF(LSTOR.GT.38) T1(41:46)=' '
        T1(LSTOR:LSTOR)=TAR(2:2)
        CALL DWRT(TAR//' stored on PostScript file.')
      END IF
      END
*DK DMW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMW
CH
      SUBROUTINE DMW
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  WINDOW MODIFICATIONS
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'RP'  (DMW)
C IND:   IF(TANSW.EQ.'M1'
C IND:   IF(TANSW.EQ.'M2'
C IND:   IF(TANSW.EQ.'M3'
C IND:   IF(TANSW.EQ.'M4'
C IND:   IF(TANSW.EQ.'R1'
C IND:   IF(TANSW.EQ.'R2'
C IND:   IF(TANSW.EQ.'R3'
C IND:   IF(TANSW.EQ.'R4'
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *49 T
C      CHARACTER *1 DT1
C      CHARACTER *2 TANSW,TROP(-4:4),DT2
      CHARACTER *2 TANSW,TROP(-4:4)
      DATA TROP/'M4','M3','M2','M1','**','R1','R2','R3','R4'/
      DIMENSION DFWI(0:1),DFWI0(0:1)
      DATA DFWI0/0.,0./
      DATA NROT/1/
C//   DATA NROT/1/,NBL/-1/
C//   PARAMETER (MR=2)
C//   DIMENSION PR(4,MR)
C//   CHARACTER *2 TR(MR)
C//   DATA TR/'CO','SZ'/
C//   DATA PR/0.,1.,31.,0., 1.,1.,9.,-1./
      LOGICAL FYES
      CALL DCOPTL
C        123456789 123456789 123456789 123456789 123456789
  930 T='RP:W1 rotate/reflect picture: R1       CO=12 SZ:1'
      T(31:32)=TROP(NROT)
C//   IF(PR(4,2).EQ.1.) THEN
C//     T(43:44)=DT2(PR(2,1))
C//     T(49:49)=DT1(PR(2,2))
C//   ELSE
        T(40:49)=' '
C//   END IF
      CALL DWR_HL_AR(T)
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,0,' ',0,
C//  &  1,MR,TR,PR,
     &  NEXEC,CHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
      RETURN
  920 DO NR=-4,4
        IF(TANSW.EQ.TROP(NR)) THEN
          NROT=NR
          GO TO 940
        END IF
      END DO
C//   IF(TANSW.EQ.'CS') THEN
C//     PR(4,2)=-PR(4,2)
C//     GO TO 930
C//   END IF
C//   IF(TANSW.EQ.'ST') THEN
C//     KSTOP=1
C//     GO TO 941
C//   END IF
      CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
      CALL DWR_IC(TANSW)
      GO TO 936
  940 KSTOP=0
  941 NAR=IAREDO
C//   NBL=PR(2,2)
      IF(NROT.NE.1) THEN
        DO NZ=0,1
          DFWI(NZ)=DFWIDU(NZ)
          DFWIDU(NZ)=DFWI0(NZ)
        END DO
        CALL DQROP(NROT)
C//     CALL DMWEAR(NAR,KSTOP,PR)
        CALL DPCEAR(NAR)
        CALL DQROP(1)
        DFWIDU(0)=DFWI(0)
        DFWIDU(1)=DFWI(1)
      ELSE
C//     CALL DMWEAR(NAR,KSTOP,PR)
        CALL DPCEAR(NAR)
      END IF
      IAREDO=NAR
      GO TO 930
      END
*DK DMAXLG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMAXLG
CH
      SUBROUTINE DMAXLG(TXZR,N,TETA,RA)
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
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TXZR
CDELET      DATA TC/42./,DT/0./
      IF(TXZR.EQ.'X') THEN
         RA=ROTODK(N)
         RETURN
      END IF
      IF(TXZR.EQ.'Z'.OR.TXZR.EQ.'S') THEN
         RA=ZZTODK(N)
         RETURN
      END IF
      IF(TETA.LE.90.) THEN
         TE=TETA
      ELSE
         TE=180.-TETA
      END IF
CDELET      IF(TE+DT.LT.TC) THEN
CDELET            RO1=ZZTODK(N)*TAND(TE)
CDELET            RO2=ZZTODK(N)*TAND(TE+DT)
CDELET            DRO=RO2-RO1
CDELET            R1=ZZTODK(N)/COSD(TE)
CDELET            R2=DRO*SIND(TE)
CDELET            RA=R1+R2
CDELET      ELSE IF(TE-DT.GT.TC) THEN
CDELET            Z1=ROTODK(N)/TAND(TE)
CDELET            Z2=ROTODK(N)/TAND(TE-DT)
CDELET            DZ=Z2-Z1
CDELET            R1=ROTODK(N)/SIND(TE)
CDELET            R2=DZ*COSD(TE)
CDELET            RA=R1+R2
CDELET      ELSE
CDELET            RA=SQRT(ROTODK(N)**2+ZZTODK(N)**2)
CDELET      END IF
      CT=COSD(TE)
      ST=SIND(TE)
      IF(TE.GT.60.) THEN
         RA=ROTODK(N)/ST
      ELSE IF(TE.LT.10.) THEN
         RA=ZMTODK(N)/CT
      ELSE
         RA=MIN(ROTODK(N)/ST,ZMTODK(N)/CT)
      END IF
      RETURN
      END
*DK DMCNVX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMCNVX
CH
      SUBROUTINE DMCNVX(NVTX1)
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
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUT,FCOL
      DATA CP2/0.000001/,CLOGT/-13./
      DIMENSION JCOL(-1:6)
      DATA JCOL/9,12,14,10,15,13,8,11/
C      LOGICAL FMVXDT(100)
      DIMENSION RMAX(9),ZMAX(9)
C               VX  VD   IT    T0    T3   E1   E3     H1    H2
      DATA RMAX/0.,12.8, 28.,31.64,177.9,84.7,229.4 ,297.3,468.4/
      DATA ZMAX/0.,100.,100.,220. ,220.,250.5,307.16,315. ,483.4/
      DATA NCCPV,NCUPV,NCCSV,NCUSV/9,10,12,13/
      NVTX1=-1
      LVTX1=0
      IF(BNUMDB(2,FKINDB).EQ.0..OR.
     &  BNUMDB(2,FVERDB).EQ.0..OR.
     &  BNUMDB(4,FKINDB).LE.0.) RETURN
C//?? IF(MODEDL(MTPCDL,1).EQ.1..AND.MCOLDL(MTPCDL).EQ.0) RETURN
      CALL DVMCNV(NVTX1,NTRK)
      LVTX1=NVTX1
      IF(NVTX1.EQ.0.OR.NTRK.EQ.0) RETURN
      MODEDT=2
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HLI,J_HP0,J_HCM,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ,J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HLI,J_HP0,J_HCM,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(ICOL.NE.0.OR.PARADA(2,J_HP0).EQ.1.) THEN
         FCOL=.FALSE.
         CALL DGLEVL(ICOL)
      ELSE
C//      IF(MODEDL(MTPCDL,1).EQ.1) THEN
C//         FCOL=.FALSE.
C//         CALL DGLEVL(MCOLDL(MTPCDL))
C//      ELSE
C//         FCOL=.TRUE.
C//         NOFS=ABS(WDSNDL(2,2,MTPCDL))
C//         MAMP=WDSNDL(2,3,MTPCDL)
C//         MAMP=MAX(1,MIN(8,MAMP)-1)
C//      END IF
        FCOL=.TRUE.
        CALL DPARGV(58,'CMA',2,AMP)
        MAMP=AMP
        CALL DPARGV(58,'CMO',2,OFS)
        NOFS=OFS
C       ................................     NCTRDM=JCOL(MOD(NVX+NOFS,MAMP))
      END IF
      IF(PARADA(4,J_HLI).NE.1.) THEN
         IHTO=0
         ZM1=9999.
         RM2=9999999.
      ELSE
         IHTO=PARADA(2,J_HLI)
         IHTO=IDMIX(2,IHTO,9)
         ZM1=ZMAX(IHTO)
         RM2=RMAX(IHTO)**2
      END IF
      L=-1
      FMCCDT=.FALSE.
      IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
         IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)**2
            CMO2=9999999.
         ELSE
            CMO1=0.
            CMO2=PARADA(2,J_HCM)**2
         END IF
         FMCCDT=.TRUE.
      ELSE
         CMO1=0.
         CMO2=9999999.
      END IF
      IF(PARADA(4,J_HT1).EQ.1.) THEN
         CT1=PARADA(2,J_HT1)
         FMCCDT=.TRUE.
      ELSE
         CT1=-99999.
      END IF
      IF(PARADA(4,J_HT2).EQ.1.) THEN
         CT2=PARADA(2,J_HT2)
         FMCCDT=.TRUE.
      ELSE
         CT2=99999.
      END IF
      IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
         IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
         ELSE
            CT2=MIN(90.,CT2)
         END IF
         FMCCDT=.TRUE.
      END IF
      IF(PARADA(4,J_HF1).EQ.1.) THEN
         CF1=PARADA(2,J_HF1)
         FMCCDT=.TRUE.
      ELSE
         CF1=-99999.
      END IF
      IF(PARADA(4,J_HF2).EQ.1.) THEN
         CF2=PARADA(2,J_HF2)
         FMCCDT=.TRUE.
      ELSE
         CF2=99999.
      END IF
      IF(PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
         CST=PARADA(2,J_HST)
         FMCCDT=.TRUE.
      ELSE
         CST=0.
      END IF
      IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
         CCH=PARADA(2,J_HCQ)
         FMCCDT=.TRUE.
      ELSE
         CCH=99.
      END IF
      CALL VZERO(FMNTDT,MMCTDT)
      MAXTR=0
      RETURN
C                                      DO 12 N=1,NVTX1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DMCVX
CH
      ENTRY DMCVX(NVX,KT1,KT2)
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
      KVX=NVX
      IF(FCOL) THEN
         IF(NVX.EQ.1) THEN
            NCTRDM=JCOL(-1)
         ELSE
            NCTRDM=JCOL(MOD(NVX+NOFS,MAMP))
         END IF
      END IF
      CALL DGLEVL(NCTRDM)
      FMVXDT(NVX)=.FALSE.
      CALL DVMCVX(NVX,KT1,KT2,KIN,VTX1DT)
      IF(KT2.EQ.0) GO TO 12
      IF(KT2.GT.MMCTDT) THEN
         IF(MAXTR.EQ.0) THEN
            MAXTR=1
            WRITE(TXTADW,1012)MMCTDT
 1012       FORMAT('More than',I4,' tracks ??')
            CALL DWRC
         END IF
         GO TO 12
      END IF
      ZA=ABS(VTX1DT(3))
      IF(ZA.GE.ZM1) GO TO 12
      RO=VTX1DT(1)**2+VTX1DT(2)**2
      IF(RO.GE.RM2) GO TO 12
      RETURN
   12 KT2=0
      RETURN
C                                       DO 11 K=KT1,KT2
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DMCTR
CH
      ENTRY DMCTR(KTR,FOUT)
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
      FOUT=.TRUE.
      IOK=IHTO
      CALL DVMCTR(KTR,PTRADT,CLTMDT,ITYPE,NVTX2)
      IF(ITYPE.LE.0) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HP0'
      CALL DPARAM(20
     &  ,J_HP0)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(CLTMDT(1).EQ.0.) THEN
         IF(PARADA(4,J_HP0).EQ.-1..OR.PARADA(2,J_HP0).EQ.0.) RETURN
         PT=PTRADT(1)**2+PTRADT(2)**2
         IF(PT.EQ.0.) RETURN
         CLTMDT(1)=PT/PARADA(2,J_HP0)
         IF(KVX.EQ.1) THEN
            NCTRDM=NCUPV
         ELSE
            NCTRDM=NCUSV
         END IF
      ELSE
         IF(PARADA(4,J_HP0).EQ.1..AND.PARADA(2,J_HP0).NE.0.) THEN
            IF(KVX.EQ.1) THEN
               NCTRDM=NCCPV
            ELSE
               NCTRDM=NCCSV
            END IF
         END IF
      END IF
      CALL DGLEVL(NCTRDM)
      IF(CLTMDT(2).LE.CLOGT) RETURN
      PT=PTRADT(1)**2+PTRADT(2)**2
      P2=PT+PTRADT(3)**2
      IF(P2.LT.CP2) RETURN
      IF(FMCCDT) THEN
         IF(P2.LT.CMO1.OR.P2.GT.CMO2) RETURN
         FI=DATN2D(PTRADT(2),PTRADT(1))
         IF(FI.LT.CF1.OR.FI.GT.CF2) RETURN
         PT=SQRT(PT)
         TE=DATN2D(PT,PTRADT(3))
         IF(TE.LT.CT1.OR.TE.GT.CT2) RETURN
         IF(CCH.NE.99..AND.CLTMDT(1).NE.CCH) RETURN
         NST=CST
         IF(NST.GT.0.) THEN
            IF(NST.NE.KTR) RETURN
         ELSE
            IF(-NST.EQ.KTR) RETURN
         END IF
      END IF
      FMNTDT(KTR)=.TRUE.
      IF(NVTX2.NE.0) THEN
         CALL DVMCVX(NVTX2,KDUM1,KDUM2,KDUM3,VTX2DT)
         FVTXDT=.TRUE.
      ELSE
         FVTXDT=.FALSE.
      END IF
      FMVXDT(KVX)=.TRUE.
      FOUT=.FALSE.
      RETURN
      END
*DK IDMIX
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  IDMIX
CH
      FUNCTION IDMIX(M1,M2,M3)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:M2=MIN(MAX(M1,M2),M3)
C    Inputs    :M1,M2,M3
C    Outputs   :M2
C
C    Called by :
C ---------------------------------------------------------------------
      IDMIX=MIN(MAX(M1,M2),M3)
      RETURN
      END
