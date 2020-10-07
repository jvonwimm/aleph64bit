*DK DRSCOL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRSCOL
CH
      SUBROUTINE DRSCOL(TANSW)
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
C INVOKED BY TANSW.EQ.'RS'  (DRSCOL)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      IF(TANSW.EQ.'SB') THEN
        IF(.NOT.FGRYDR) THEN
          MOTR=MOTRDC
          MOVD=MOVDDC
          MOIT=MOITDC
          MOTP=MOTPDC
          COVD=PDCODD(2,LCVDDD)
          COIT=PDCODD(2,LCITDD)
          COTR=PDCODD(2,LCNCDD)
          COTP=PDCODD(2,LCTCDD)
          FGRYDR=.TRUE.
        END IF
        CALL DWRT('VD,ITC,TPC and tracks set to grey.')
        MOTRDC=0
        MOVDDC=0
        MOITDC=0
        MOTPDC=0
        GO TO 10
      END IF
      IF(FGRYDR.AND.TANSW.EQ.'SN') THEN
        FGRYDR=.FALSE.
        MOTRDC=MOTR
        MOVDDC=MOVD
        MOITDC=MOIT
        MOTPDC=MOTP
        PDCODD(2,LCVDDD)=COVD
        PDCODD(2,LCITDD)=COIT
        PDCODD(2,LCNCDD)=COTR
        PDCODD(2,LCTCDD)=COTP
        CALL DWRT('Colour of VD,ITC,TPC and tracks set to normal.')
        CALL DSC0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRSCTR
CH
      ENTRY DRSCTR
CH
CH --------------------------------------------------------------------
CH
      IF(.NOT.FGRYDR) RETURN
   10 CALL DPARGV(31,'STC',4,STC4)
      IF(STC4.GE.0.) THEN
        PDCODD(2,LCVDDD)=PARADR(2,2)
        PDCODD(2,LCITDD)=PARADR(2,2)
        PDCODD(2,LCNCDD)=PARADR(2,2)
        PDCODD(2,LCTCDD)=PARADR(2,2)
        CALL DSC0
      END IF
      END
*DK DRSTIM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRSTIM
CH
      SUBROUTINE DRSTIM
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
      CHARACTER *3 DT3
      CALL DGTIM0
      NC=BNUMDB(2,FRFTDB)
      DO N=1,NC
        CALL DVFTR(N,NPNT)
      END DO
      CALL DGTIM1
      CALL DWRT('For '//DT3(FLOAT(NC))//' tracks.')
      NTRK=PARADR(2,1)
      CALL DVFTR(NTRK,NPNTDR)
      END
*DK DRSPTY
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRSPTY
CH
      SUBROUTINE DRSPTY
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
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)
      DATA PIDEG/57.29577951/

      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      CHARACTER *1 TDET(3)
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *5 DT5
      CHARACTER *6 DT6
      DATA TDET/'V','i','T'/
      CHARACTER *51 T1,T2
C              123456789 123456789 123456789 123456789 123456789 1
      DATA T1/' TR=123  row=T31   #=12  d-rho-phi=-1.234  +-1.3456'/
      DATA T2/'phi=123  rho=123.5 Z=-234.6     DZ=-1.234  +-1.2345'/
      NTRK=MDLPDP-1000
      IF(NTRK.LT.0.OR.NTRK.GT.BNUMDB(2,FRFTDB)) THEN
        CALL DWRT('Non existent track ??')
      ELSE
        IF(NPIKDP.GT.NPNTDR) THEN
          CALL DWRT('Non existent hit ??')
        ELSE
          CALL DRSRTN(RF(NPIKDP),IH,NDEV,LEV)
          FI_7=PIDEG*XME(1,NPIKDP)/RF(NPIKDP)
          FI=DFINXT(180.,FI_7)
          T1( 5: 7)=DT3(FLOAT(NTRK))
          T1(14:14)=TDET(NDEV)
          T1(15:16)=DT2(FLOAT(IH))
          T1(22:23)=DT2(FLOAT(NPIKDP))
          VM_7=VMEAS(1,1,NPIKDP)
          T1(46:51)=DT6(VM_7)
          T2( 5: 7)=DT3(FI)
          RF_7=RF(NPIKDP)
          T2(14:18)=DT5(RF_7)
          XM_7=XME(2,NPIKDP)
          T2(22:27)=DT6(XM_7)
          VM_7=VMEAS(2,2,NPIKDP)
          T2(46:51)=DT6(VM_7)
          IF(IUSED(NPIKDP).NE.0) THEN
            XM_7=XME (1,NPIKDP)
            XS_7=XSTS(1,NPIKDP)
            T1(36:41)=DT6(XM_7-XS_7)
            XM_7=XME (2,NPIKDP)
            XS_7=XSTS(2,NPIKDP)
            T2(36:41)=DT6(XM_7-XS_7)
          ELSE
            T1(36:41)=' un-  '
            T2(36:41)='known '
          END IF
          CALL DWRT(T1)
          CALL DWRT(T2)
        END IF
      END IF
      END
*DK DRSTYP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRSTYP
CH
      SUBROUTINE DRSTYP
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
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)

      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      CHARACTER *1 TD(3),TC(8:15),TU
      DATA TD/'V','i','T'/
      DATA TC/'W','G','Y','O','R','M','C','B'/
      DATA TU/' '/
      CHARACTER *41 TRS1
      CHARACTER *36 TRS2
      DATA TRS1/'  #   rho  rho*phi    Z    sigrf   sigZ  '/
      DATA TRS2/'rho*phi    Z     phi   la    1/r   c'/
      IF(PARADR(2,1).LE.0..OR.
     &   PARADR(2,1).GT.BNUMDB(2,FRTLDB)) THEN
        CALL DWRT(' Wrong track number !')
        RETURN
      END IF
      NTR=PARADR(2,1)
      IF(NTR.LE.0) GO TO 610
C     CALL DWRT('Type <CR> to continue after 20 lines.')
      CALL DWRT(TRS1//TRS2)
C     J=0
      DO K=NPNTDR,1,-1
        CALL DRSRTN(RF(K),IH,NDEV,LEV)
        IF(NDEV.EQ.2) THEN
          V22=0
        ELSE
          V22=VMEAS(2,2,K)
        END IF
        IH=IH+1
        IF(.NOT.FMONDT) TU=TC(LEV)
        IF(IUSED(K).EQ.1) THEN
          WRITE(TXTADW,1000) TD(NDEV),IH,RF(K),(XME(I,K),I=1,2),
     &      VMEAS(1,1,K),V22,(XSTS(N,K),N=1,5),TU
 1000     FORMAT(A1,I2,F6.1,2F8.2,2F7.3,2F8.2,2F6.2,F8.4,1X,A)
        ELSE
          WRITE(TXTADW,1003) TD(NDEV),IH,RF(K),(XME(I,K),I=1,2),
     &      VMEAS(1,1,K),V22
 1003     FORMAT(A1,I2,F6.1,2F8.2,2F7.3,' unused')
        END IF
        CALL DWRC
C       J=J+1
C       IF(J.EQ.N20) CALL DGETLN(T1,LENT1,LEN(T1))
      END DO
      CALL DWRT(TRS1//TRS2)
      RETURN
  610 CALL DWRT(' NO TRACK FIT')
      END

*DK DRO_HV_TO_CONE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRO_HV_TO_CONE
CH
      SUBROUTINE DRO_HV_TO_CONE(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Rubberband: from Box to Cone
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)

C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO,J_PAS,J_RDY'
      CALL DPARAM(10
     &  ,J_PFR,J_PTO,J_PAS,J_RDY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::

      PARADA(2,J_PFR)=MIN(HRB(1),HRB(3))
      PARADA(2,J_PTO)=MAX(HRB(1),HRB(3))
      PARADA(2,J_RDY)=0.5*(VRB(1)+VRB(3))
      DV=VRB(3)-VRB(1)
      IF(DV.NE.0.) PARADA(2,J_PAS)=ABS((HRB(3)-HRB(1))/DV)
      END

*DK DROJTC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROJTC
CH
      SUBROUTINE DROJTC(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION PR(4,2)
      DIMENSION PXYZ(3)
      CHARACTER *15 TB
      CHARACTER *2 DT2
      CHARACTER *4 DT4
      DATA YCOLD/-9./
C              123456789 123456789 123456789 123456789 123456789
      CHARACTER *49 TC
      DATA TC/' ==> 11 FI=180.1  TE=180.1  P=140.12   E=140.12'/
      IF(PR(4,2).LE.0.) THEN
        YCUT=-99999.
        TB='EJET 0'
      ELSE
        YCUT=PR(2,2)
        TB='EJET 1  yc='//DT4(YCUT)
      END IF
      CALL DVJET0(YCUT,NJET)
      PR(3,1)=NJET
      PR(2,1)=MIN(PR(2,1),PR(3,1))
      IF(NJET.GT.0.AND.PR(2,1).LE.0.) THEN
        PMAX=0.
        DO J=1,NJET
          CALL DVJET(J,PXYZ,E)
          CALL DPOLCR(PXYZ(1),PXYZ(2),PXYZ(3),PP,TE,FI)
          IF(PP.GE.PMAX) THEN
            PMAX=PP
            JMAX=J
          END IF
        END DO
        PR(2,1)=JMAX
      END IF
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROJT0
CH
      ENTRY DROJT0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      YCOLD=-9.
      NJET=0
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROJTA
CH
      ENTRY DROJTA(NJ,AF,AT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      IF(NJET.GT.0) THEN
        CALL DVJET(NJ,PXYZ,E)
        CALL DPOLCR(PXYZ(1),PXYZ(2),PXYZ(3),
     &    PP,AT,AF)
      ELSE
        CALL DWRT('No jet existent.')
      END IF
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROJTT
CH
      ENTRY DROJTT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH

C              123456789 123456789 123456789 123456789 123456789
C     DATA TC/' ==> 11 FI=180.1  TE=180.1  P=40.12   E=40.12'
C     DATA TC/' ==> 11 FI=180.1  TE=180.1  P=140.12   E=140.12'/
      IF(NJET.GT.0) THEN
        CALL DWRT(TB)
        CALL DPARGV(43,'RJN',2,RJN)
        IJ=RJN
        DO J=1,NJET
          IF(J.EQ.IJ) THEN
            TC(2:4)='==>'
          ELSE
            TC(2:4)=' '
          END IF
          TC(6:7)=DT2(FLOAT(J))
          CALL DVJET(J,PXYZ,E)
          CALL DPOLCR(PXYZ(1),PXYZ(2),PXYZ(3),PP,TE,FI)
          WRITE(TC(12:16),1000) FI
          WRITE(TC(22:26),1000) TE
          WRITE(TC(31:36),1001) PP
          WRITE(TC(42:47),1001) E
 1000     FORMAT(F5.1)
 1001     FORMAT(F5.2)
          CALL DWRT(TC)
        END DO
      ELSE
        CALL DWRT('No jet existent.')
      END IF
      END
*DK DROTRK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROTRK

CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROTRK
CH
      SUBROUTINE DROTRK(NTR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      CALL DVTR0(NUM)
      IF(NTR.LE.0.OR.NTR.GT.NUM) RETURN
      CALL DVTRV(NTR)
      CH=-SIGN(1.,FRFTDT(1))
      A0=FRFTDT(3)*PIDGDK+CH*90.
      CA0=COSD(A0)
      SA0=SIND(A0)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RXX,J_RYY,J_RZZ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(2,J_RXX)=-FRFTDT(4)*CA0*CH
      PARADA(2,J_RYY)=-FRFTDT(4)*SA0*CH
      PARADA(2,J_RZZ)=FRFTDT(5)
      PARADA(2,J_PFI)=FRFTDT(3)*PIDGDK
      PARADA(2,J_PTE)=DATN2D(1.,FRFTDT(2))
      END

*DK DRZ_DT_TO_AS
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRZ
CH
      SUBROUTINE DRZ_DT_TO_AS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     calculate AS from DT
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4),HR0(4),VR0(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PDT,J_PFR,J_PTO,J_PAS,J_PBZ,J_PZ0,J_PR0'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PDT,J_PFR,J_PTO,J_PAS,J_PBZ,J_PZ0,J_PR0)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C                             Aspect ratio = 0-2 / 1-3

C           1      2      3   Problem: the angle 1-0-2 is larger than 2-0-3
C       to ______________     theta = 4-0-2. In this drawing theta > 0
C         /      /      /     D_theta = 2-0-3
C        /  4   /      /
C       /   |  /      /
C      /    | /      /
C  fr /______/______/
C       dz  0  dz
C
C   1      2      3                   the angle 1-0-2 is smallerr than 2-0-3
C    ---------------          theta = 4-0-2. In this drawing theta < 0
C     \      \      \         D-theta = 2-0-1
C      \      \  4   \
C       \      \ |    \
C        \      \|     \
C         \______\______\
C                0
C
      RR=PARADA(2,J_PTO)-PARADA(2,J_PFR)
      TE=PARADA(2,J_PTE)
      Z2=RR*SIND(TE)
      R2=RR*COSD(TE)

      DT=PARADA(2,J_PDT)*0.5
      IF(TE.GT.0.) THEN
        Z3=R2*TAND(TE+DT)
        DZ=Z3-Z2
      ELSE
        Z1=R2*TAND(TE-DT)
        DZ=Z2-Z1
      END IF
      IF(DZ.NE.0.) PARADA(2,J_PAS)=RR/(2.*DZ)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------- DRZ_AS_TO_DT
CH
      ENTRY DRZ_AS_TO_DT
CH
CH --------------------------------------------------------------------
CH
C     Inverse of above: calculate DT from aspect ratio
C
      IF(PARADA(2,J_PAS).NE.0.) THEN
        RR=PARADA(2,J_PTO)-PARADA(2,J_PFR)
        IF(RR.GT.0.) THEN
          TE=PARADA(2,J_PTE)
          Z2=RR*SIND(TE)
          R2=RR*COSD(TE)

          DZ=0.5*RR/PARADA(2,J_PAS)
          IF(TE.GE.0.) THEN
            Z3=Z2+DZ
            DT=PIFCDK*ATAN2(Z3,R2)-TE
          ELSE
            Z1=Z2-DZ
            DT=TE-PIFCDK*ATAN2(Z1,R2)
          END IF
          PARADA(2,J_PDT)=2.*DT
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
CH -------------------------------------------------------  DRZ_HV_TO_CONE
CH
      ENTRY DRZ_HV_TO_CONE(HRB,VRB)
CH
CH --------------------------------------------------------------------
CH
      CALL DQZ_OFFSET(-1.,J_PZ0,J_PR0,PARADA(2,J_PZ0),HRB,VRB,HR0,VR0)
      IF(PARADA(2,J_PBZ).EQ.2.) THEN
         H13=0.5*(HR0(1)+HR0(3))
         V13=0.5*(VR0(1)+VR0(3))
         R13=SQRT(H13*H13+V13*V13)
         D13=0.5*ABS(HR0(1)-HR0(3))
         PARADA(2,J_PFR)=R13-D13
         PARADA(2,J_PTO)=R13+D13
         PARADA(2,J_PTE)=PIFCDK*ATAN2(ABS(V13),H13)
         IF(V13.LT.0.) PARADA(2,J_PFI)=MOD(PARADA(2,J_PFI)+1980.,360.)
      ELSE
         H34=0.5*(HR0(3)+HR0(4))
         H12=0.5*(HR0(1)+HR0(2))
         V34=VR0(3)
         V12=VR0(1)
         R34=SQRT(H34**2+V34**2)
         R12=SQRT(H12**2+V12**2)
         IF(     R12.LT.R34) THEN
            PARADA(2,J_PFR)=R12
            PARADA(2,J_PTO)=R34
            PARADA(2,J_PTE)=PIFCDK*ATAN2(ABS(V34),H34)
            IF(V34.LT.0.)
     &        PARADA(2,J_PFI)=MOD(PARADA(2,J_PFI)+1980.,360.)
         ELSE IF(R12.GT.R34) THEN
            PARADA(2,J_PFR)=R34
            PARADA(2,J_PTO)=R12
            PARADA(2,J_PTE)=PIFCDK*ATAN2(ABS(V12),H12)
            IF(V12.LT.0.)
     &        PARADA(2,J_PFI)=MOD(PARADA(2,J_PFI)+1980.,360.)
         ELSE
            RETURN
         END IF
         IF(V34*V12.LT.0.) PARADA(2,J_PFR)=-PARADA(2,J_PFR)
         DZ=ABS(HR0(2)-HR0(1))
         IF(DZ.GT.0.)
     &     PARADA(2,J_PAS)=(PARADA(2,J_PTO)-PARADA(2,J_PFR))/DZ
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DRZ_CONE_TO_HV
CH
      ENTRY DRZ_CONE_TO_HV(HRB,VRB)
CH --------------------------------------------------------------------
CH
      IF(PARADA(2,J_PBZ).EQ.2.) THEN
C       ......................................................... square
        R13=0.5*(PARADA(2,J_PTO)+PARADA(2,J_PFR))
        D13=0.5*(PARADA(2,J_PTO)-PARADA(2,J_PFR))
        H13=R13*COSD(PARADA(2,J_PTE))
        V13=R13*SIND(PARADA(2,J_PTE))
        HR0(1)=H13-D13
        HR0(3)=H13+D13
        VR0(1)=V13-D13
        VR0(3)=V13+D13
        CALL DQ24(HR0,VR0)
      ELSE
C       ....................................................... parallelogram
        R12=PARADA(2,J_PFR)
        R34=PARADA(2,J_PTO)
        CT=COSD(PARADA(2,J_PTE))
        ST=SIND(PARADA(2,J_PTE))
        H12=R12*CT
        V12=R12*ST
        H34=R34*CT
        V34=R34*ST
        DH=0.5*(R34-R12)/PARADA(2,J_PAS)
        HR0(1)=H12-DH
        VR0(1)=V12
        HR0(2)=H12+DH
        VR0(2)=V12
        HR0(3)=H34+DH
        VR0(3)=V34
        HR0(4)=H34-DH
        VR0(4)=V34
      END IF
      CALL DQZ_OFFSET(1.,J_PZ0,J_PR0,PARADA(2,J_PZ0),HR0,VR0,HRB,VRB)
      END

*DK DRZMO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZMO
CH
      SUBROUTINE DRZMO(NAR,TE,H,V,FIN)
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
      LOGICAL FIN
      CALL DQSET(NAR,0.,0.)
      R=TVRUDS(6,NAR,IWUSDO)*0.5
      Z =R*COSD(TE)
      RO=R*SIND(TE)
      CALL DQPOC(Z,RO,HH,VV,FIN)
      IF(FIN) THEN
         H=HH
         V=VV
      END IF
      RETURN
      END
*DK DRZPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPO
CH
      SUBROUTINE DRZPO(NAR,H,V,TE)
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
      CHARACTER *7 DT7,TZ,TR
      CALL DQINV(NAR,H,V,Z,R)
      TZ=DT7(Z)
      TR=DT7(R)
      CALL DWRT('          Z='//TZ//'    RHO='//TR//' [CM]')
      TE=DATN2D(ABS(R),Z)
      RETURN
      END
C*DK DRZRBC
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRAS
CCH
C      SUBROUTINE DRZRAS
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
C*CA DALLCO
C      INCLUDE 'DALI_CF.INC'
C      DIMENSION HRB(4),VRB(4)
C      NGO=1
C      GO TO 1
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRDT
CCH
C      ENTRY DRZRDT
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
C      NGO=2
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C    1 TPARDA=
C     &  'J_PTE,J_PDT,J_PFR,J_PTO,J_PAS'
C      CALL DPARAM(11
C     &  ,J_PTE,J_PDT,J_PFR,J_PTO,J_PAS)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TE=PARADA(2,J_PTE)
C      IF(TE.GT.90.) TE=180.-TE
C      R1=PARADA(2,J_PFR)
C      R2=PARADA(2,J_PTO)
C      IF(TE.LT.40.) THEN
C        QQ=1./COS(TE)
C      ELSE
C        QQ=1./SIN(TE)
C      END IF
C      R1=R1*QQ
C      R2=R2*QQ
C      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
C      IF(NGO.EQ.1) THEN
C        IF(PARADA(2,J_PDT).GT.0.)
C     &    PARADA(2,J_PAS)=Q/TAND(0.5*PARADA(2,J_PDT))
C      ELSE
C        IF(PARADA(2,J_PAS).GT.0.)
C     &    PARADA(2,J_PDT)=2.*ATAND(Q/PARADA(2,J_PAS))
C      END IF
C      RETURN
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRBC
CCH
C      ENTRY DRZRBC(HRB,VRB)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
CC     Rubberband: from Box to Cone
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TPARDA=
C     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM'
C      CALL DPARAM(11
C     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      IF(PARADA(2,J_PBM).EQ.1.) THEN
C         HM=0.5*(HRB(3)+HRB(1))
C         VM=0.5*(VRB(3)+VRB(1))
C         DH= ABS(HRB(3)-HRB(1))
C         DV= ABS(VRB(3)-VRB(1))
C         D2=0.5*MAX(DH,DV)
C         HRB(1)=HM-D2
C         HRB(2)=HM+D2
C         HRB(3)=HRB(2)
C         HRB(4)=HRB(1)
C         VRB(1)=VM-D2
C         VRB(2)=VRB(1)
C         VRB(3)=VM+D2
C         VRB(4)=VRB(3)
C      END IF
C      TE=DATN2D(VRB(1)+VRB(3),HRB(1)+HRB(3))
C      IF(PARADA(2,J_PBM).LT.5.) THEN
C         TT=MOD(TE+3600.,360.)
C         IF(TT.LT.40..OR.TT.GT.320.) THEN
C            FR=HRB(1)
C            TO=HRB(2)
C         ELSE IF(TT.LE.140.) THEN
C            FR=VRB(2)
C            TO=VRB(3)
C         ELSE IF(TT.LT.220.) THEN
C            FR=-HRB(2)
C            TO=-HRB(1)
C         ELSE
C            FR=-VRB(3)
C            TO=-VRB(2)
C         END IF
C         IF(FR.LE.TO) THEN
C           PARADA(2,J_PFR)=FR
C           PARADA(2,J_PTO)=TO
C         ELSE
C           PARADA(2,J_PFR)=TO
C           PARADA(2,J_PTO)=FR
C         END IF
C      ELSE
C         TT=TE
C         IF(TT.GT.180.) TT=360.-TT
C         IF(TT.LT.40..OR.TT.GT.140.) THEN
C            H12=0.5*(HRB(1)+HRB(2))
C            H34=0.5*(HRB(3)+HRB(4))
C            H1=ABS(H12)
C            H3=ABS(H34)
C            IF(H1.LT.H3) THEN
C              IF(H34.GT.0.) THEN
C                PARADA(2,J_PFR)= H12
C              ELSE
C                PARADA(2,J_PFR)=-H12
C              END IF
C              PARADA(2,J_PTO)=H3
C            ELSE
C              IF(H12.GT.0.) THEN
C                PARADA(2,J_PFR)= H34
C              ELSE
C                PARADA(2,J_PFR)=-H34
C              END IF
C              PARADA(2,J_PTO)=H1
C            END IF
C            QQ=1./COSD(TT)
C          ELSE
C            V12=0.5*(VRB(1)+VRB(2))
C            V34=0.5*(VRB(3)+VRB(4))
C            V1=ABS(V12)
C            V3=ABS(V34)
C            IF(V1.LT.V3) THEN
C              IF(V34.GT.0.) THEN
C                PARADA(2,J_PFR)= V12
C              ELSE
C                PARADA(2,J_PFR)=-V12
C              END IF
C              PARADA(2,J_PTO)=V3
C            ELSE
C              IF(V12.GT.0.) THEN
C                PARADA(2,J_PFR)= V34
C              ELSE
C                PARADA(2,J_PFR)=-V34
C              END IF
C              PARADA(2,J_PTO)=V1
C            END IF
C            QQ=1./SIND(TT)
C         END IF
C         D=(HRB(1)-HRB(2))*SIND(MAX(1.,TT))
C         PARADA(2,J_PAS)=ABS((PARADA(2,J_PTO)-PARADA(2,J_PFR))*QQ/D)
C      END IF
C      IF(TE.LT.180.) THEN
C         PARADA(4,J_PFI)= 1.
C      ELSE
C         PARADA(4,J_PFI)=-1.
C         TE=360.-TE
C      END IF
C      IF(TE.LT.1.) THEN
C         TE=1.
C      ELSE IF(TE.GT.179.) THEN
C         TE=179.
C      END IF
C      PARADA(2,J_PTE)=TE
C      END
*DK DRSD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRSD
CH
      SUBROUTINE DRSD
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
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      DIMENSION HH(4),VV(4),GH(3)
      DATA GH/2.,8.,21./,DVTX/5./,DHTX/3./
      CHARACTER *30 TX
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      DATA DV/1./
      NTR=PARADR(2,1)
      IF(NTR.LE.0) THEN
        CALL DWRT(' TR=0. No track selected yet!')
        RETURN
      END IF
      CALL DVFTR(NTR,NPNTDR)
      IF(NPNTDR.LE.0) THEN
        CALL DWRT(' Track without points?')
        RETURN
      END IF
      NUM1=1
      NUM2=NPNTDR
      DFWI0=DFWIDU(0)
      DFWI1=DFWIDU(1)
      DFWIDU(0)=1111.
      DFWIDU(1)=1111.
      CALL DQWIL(1.)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_STC,J_SDF,J_SDZ,J_SVD,J_SIT'
      CALL DPARAM(31
     &  ,J_STC,J_SDF,J_SDZ,J_SVD,J_SIT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FPIKDP) THEN
        IF(PARADA(2,J_STC).NE.PARADR(2,1)) THEN
          CALL DWRT('Track not active. Select track in RS.')
          RETURN
        END IF
        IF(FPIMDP) THEN
          IF(MDLPDP.NE.1000+NTR) THEN
            CALL DWRT('Wrong track.')
            RETURN
          END IF
          NUM1=NPIKDP
          NUM2=NPIKDP
        END IF
        MDLRDP=1000+NTR
      ELSE
        PARADA(2,J_STC)=PARADR(2,1)
        PARADA(4,J_STC)=PARADR(4,2)
        CALL DQCL(IAREDO)
        DH=(HHGHDG(IAREDO)-HLOWDG(IAREDO))/31.5
        VV(1)=VLOWDG(IAREDO)
        IF(PDCODD(2,ISTYDD).GE.2.) THEN
          VV(3)=VHGHDG(IAREDO)
          VV(2)=VV(1)
          VV(4)=VV(3)
          HH(3)=HLOWDG(IAREDO)
          DO 700 L=1,3
            HH(1)=HH(3)
            HH(3)=HH(1)+GH(L)*DH
            HH(2)=HH(3)
            HH(4)=HH(1)
            CALL DQLEVL(ICVDDD+L-1)
            CALL DGAREA(4,HH,VV)
  700     CONTINUE
        END IF
        CALL DQLEVL(ICFRDD)
        VV(2)=VHGHDG(IAREDO)
        HH(1)=HLOWDG(IAREDO)
        DO 710 L=1,2
          HH(1)=HH(1)+GH(L)*DH
          HH(2)=HH(1)
          CALL DGDRAW(2,HH,VV)
  710   CONTINUE
      END IF
      CALL DQAR0(1.,DV)
      CALL DRSARU(NUM1,NUM2)
      VLOW=VLOWDG(IAREDO)
      VHGH=VHGHDG(IAREDO)
C     ............................................................... RO*DF/RO
      VLOWDG(IAREDO)=0.5*(VLOW+VHGH)
      CALL DRSAR(1,J_SDF,NUM1,NUM2)
      VLOWDG(IAREDO)=VLOW
C     ................................................................... DZ/RO
      VHGHDG(IAREDO)=0.5*(VLOW+VHGH)
      CALL DRSAR(2,J_SDZ,NUM1,NUM2)
      VHGHDG(IAREDO)=VHGH
C     ........................................................................
      IF(FPIKDP) GO TO 900
      CALL DQSCA('H',0.,31.5,' ',0,'row',3)
C         123456789 123456789 123456789
C     TX='RS *12   *12         Track 123'
      TX='RS                   Track    '
      IF(PARADA(2,J_SVD).NE.1.) THEN
        TX( 4: 4)='*'
        TX( 5: 6)=DT2(PARADA(2,J_SVD))
      END IF
      IF(PARADA(2,J_SIT).NE.1.) THEN
        TX(10:10)='*'
        TX(11:12)=DT2(PARADA(2,J_SIT))
      END IF
      TX(28:30)=DT3(PARADR(2,1))
      CALL DGTEXT(HMINDG(IAREDO)+DHTX,VMINDG(IAREDO)+DVTX,TX,30)
      CALL DQFR(IAREDO)
      CALL DPCSAR
  900 DFWIDU(0)=DFWI0
      DFWIDU(1)=DFWI1
      END
*DK DRSAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRSAR
CH
      SUBROUTINE DRSAR(J,JP,NUM1,NUM2)
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
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      EQUIVALENCE (K,KPIKDP)
      DIMENSION HRB(4),VRB(4),P(3)
      DATA HLO,HHI/0.,31.5/,N0/1/,N8/8/,DLIN/1./,
     &  DMON/0.79/,DMON2/0.395/,QRU/1.2/,VU/5./
      DIMENSION NGR(2),MGR(2),DMX(2)
      DATA MGR/2,0/,NGR/1,3/,DHR/-3./,DVR/75./,DMX/1.5,1.5/
      CHARACTER *4 TX1(2)
      CHARACTER *11 TX2(2)
C               123456789 12345
      DATA TX1/'r f ','  Z '/
      DATA TX2/'(meas.-fit)','(fit-meas.)'/
      DATA P(3)/1./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_SVD,J_SIT'
      CALL DPARAM(31
     &  ,J_SVD,J_SIT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      P(1)=PARADA(2,J_SVD)
      P(2)=PARADA(2,J_SIT)
C      JP=J_STC+J
      IF(PARADA(4,JP).EQ.-1.) THEN
        DMAX=0.
        DO 702 K=1,NPNTDR
          IF(IUSED(K).EQ.1) THEN
            CALL DRSRTN(RF(K),IH,NDEV,LEV)
            IF(J.EQ.2.AND.NDEV.EQ.2) GO TO 702
            D=ABS((XME(J,K)-XSTS(J,K))*P(NDEV))
            DMAX=MAX(D,DMAX)
          END IF
  702   CONTINUE
        IF(DMAX.EQ.0.) DMAX=DMX(J)
        PARADA(2,JP)=10.*SIGN(DMAX,PARADA(2,JP))*QRU
      END IF
      DRU=PARADA(2,JP)*0.1
      IF(DRU.GT.0.) THEN
        SIG=1.
      ELSE
        SIG=-1.
        DRU=-DRU
      END IF
      CALL DQRER(0,HLO,-DRU,HHI,DRU,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(.NOT.FPIKDP) THEN
        CALL DQLEVL(ICFRDD)
        CALL DQL2E(0.,0.,31.,0.)
        DLINDD=DLIN
      END IF
      DO 700 K=NUM1,NUM2
        CALL DRSRTN(RF(K),IH,NDEV,LEV)
        CALL DGLEVL(LEV)
        IF(J.EQ.2.AND.NDEV.EQ.2) GO TO 700
        H1=IH
        H2=H1+DH2
        HM=H1+DHM
        IF(IUSED(K).EQ.0) THEN
          D=0.
        ELSE
          D=SIG*(XME(J,K)-XSTS(J,K))*P(NDEV)
        END IF
        IF(FPIKDP) THEN
          IF(ABS(D).GT.DRU) D=0.
          CALL DQPIK(HM,D)
          GO TO 700
        END IF
        VJJ=VMEAS(J,J,K)*P(NDEV)
        IF(D.LT.0.) THEN
          IF(IUSED(K).EQ.1) CALL DQAR(H1, D,H2,0.)
          CALL DGLEVL(N8)
          CALL DQL2E(HM,D-VJJ,HM,D)
          CALL DGLEVL(N0)
          DVM=D+VJJ
          IF(DVM.LT.0.) THEN
            CALL DQL2E(HM,D,HM,DVM)
          ELSE
            CALL DQL2E(HM,D,HM,0.)
            CALL DGLEVL(N8)
            CALL DQL2E(HM,0.,HM,DVM)
          END IF
        ELSE
          IF(IUSED(K).EQ.1) CALL DQAR(H1,0.,H2, D)
          CALL DGLEVL(N0)
          DVM=D-VJJ
          IF(DVM.GT.0.) THEN
            CALL DQL2E(HM,D,HM,DVM)
          ELSE
            CALL DQL2E(HM,D,HM,0.)
            CALL DGLEVL(N8)
            CALL DQL2E(HM,0.,HM,DVM)
          END IF
          CALL DGLEVL(N8)
          CALL DQL2E(HM,D,HM,D+VJJ)
        END IF
  700 CONTINUE
      IF(FPIKDP) RETURN
      DLINDD=PDCODD(2,LITRDD)
      CALL DQLEVL(ICTXDD)
      CALL DGTDIR(90)
      HRES=HMINDG(IAREDO)+DHR
      VRES=0.5*(VLOWDG(IAREDO)+VHGHDG(IAREDO))-DVR
      IF(PARADA(2,JP).GT.0.) THEN
        L=1
      ELSE
        L=2
      END IF
      CALL DGTXTG(HRES,VRES,TX1(J)//TX2(L),15,NGR,MGR(J))
      DMM=10.*DRU
      CALL DQSCA('V',-DMM,DMM,'mm',2,' ',0)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRSARU
CH
      ENTRY DRSARU(NUM1,NUM2)
CH
CH --------------------------------------------------------------------
CH
      CALL DQRER(0,HLO,-100.,HHI,100.,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(.NOT.FPIKDP) THEN
        CALL DQLEVL(ICFRDD)
        CALL DQL2E(HLO,0.,HHI,0.)
      END IF
      IF(FMONDT.OR.FBLWDT.OR.PARADR(4,3).EQ.1.) THEN
        DH2=DMON
        DHM=DMON2
      ELSE
        DH2=1.
        DHM=0.5
      END IF
      DO 710 K=NUM1,NUM2
        IF(IUSED(K).EQ.0) THEN
          CALL DRSRTN(RF(K),IH,NDEV,LEV)
          IF(FPIKDP) THEN
            CALL DQPIK(FLOAT(IH)+DHM,0.)
          ELSE
            CALL DGLEVL(LEV)
            H1=IH
            H2=H1+DH2
            CALL DQAR(H1,-VU,H2,VU)
          END IF
        END IF
  710 CONTINUE
      END
*DK DRS_TYP_RES
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRS_TYP_RES
CH
      SUBROUTINE DRS_TYP_RES
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
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      CHARACTER *2 TDEV(3),TCO(0:15)
      DATA TDEV/'VD','IT','TP'/
      DATA TCO/'c0','c1','c2','c3','c4','c5','c6','c7',
     &         'wh','gn','ye','br','rd','ma','cy','bl'/
      CHARACTER *49 TH
C              123456789 123456789 123456789 123456789 123456789 
C              rd 12:IT  1 23456.789 2345.678 23456.789 2345.678
      DATA TH/'co  #:d. IU   rho*phi    +-        Z        +-   '/
      DIMENSION KH(MPT)
      CALL VZERO(KH,MPT)
      CALL DWRT(TH)
      DO K=NPNTDR,1,-1
        TXTADW=' '
        CALL DRSRTN(RF(K),IH,NDEV,LEV)
        IH=IH+1
        RFR=XME(1,K)-XSTS(1,K)
        RFR=MAX(-9999.,MIN(9999.,RFR))
        DFR=MAX( -999.D0,MIN( 999.D0,VMEAS(1,1,K)))
        IF(NDEV.NE.2) THEN
          RZ=XME(2,K)-XSTS(2,K)
          RZ=MAX(-9999.,MIN(9999.,RZ))
C         .................. 999.D0 =double prexision constant
          DZ=MAX( -999.D0,MIN( 999.D0,VMEAS(2,2,K)))
          WRITE(TXTADW,1000) TCO(LEV),IH,TDEV(NDEV),IUSED(K),RFR,DFR
     &      ,RZ,VMEAS(2,2,K)
        ELSE
          WRITE(TXTADW,1000) TCO(LEV),IH,TDEV(NDEV),IUSED(K),RFR,DFR
        END IF
        CALL DWRC
 1000   FORMAT(A,I3,':',A,I3,2(F10.4,F9.4))
      END DO
      END
*DK DRSRTN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRSRTN
CH
      SUBROUTINE DRSRTN(RODP,N,NDEV,LEV)
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
      DOUBLE PRECISION RODP
      DIMENSION LEVV(0:1),LEVI(8),LEVT(0:20)
      DATA LEVV/ 8,14/
      DATA LEVI/9,15,10,14,11,8,12,9/
      DATA LEVT/14, 8,13,10,12, 9,15,11,
     &          14,13,12, 8,15,10, 9,11,
     &          13,10,15, 8,12/
      DATA RTPC0,RTPC1,DTPC/34.,40.   ,6.525/
      DIMENSION RITC(8)
      DATA RITC/16.64,17.95,19.25,20.75,22.35,23.81,25.36,34./
      DATA RITC0/12.8/
C      DATA RVDE0,RVDE1,DVDE/ 0., 8.2,3.164/
      DATA RVDE1/ 8.2/
      DATA N0/0/
      RO=RODP
      IF     (RO.GT.RTPC0) THEN
        N=NINT((RO-RTPC1)/DTPC)
        IF(PARADR(4,3).EQ.1.) THEN
          LEV=PARADR(2,3)
        ELSE
          LEV=LEVT(N)
        END IF
        N=N+10
        NDEV=3
      ELSE IF(RO.GT.RITC0) THEN
        DO 700 N=1,8
          IF(RO.LT.RITC(N)) GO TO 2
  700   CONTINUE
    2   IF(PARADR(4,3).EQ.1.) THEN
          LEV=PARADR(2,3)
        ELSE
          LEV=LEVI(N)
        END IF
        N=N+1
        NDEV=2
      ELSE
        IF(RO.LT.RVDE1) THEN
          N=N0
        ELSE
          N=N0+1
        END IF
C        N=NINT((RO-RVDE1)/DVDE)
        IF(PARADR(4,3).EQ.1.) THEN
          LEV=PARADR(2,3)
        ELSE
          LEV=LEVV(N)
        END IF
        NDEV=1
      END IF
      END
*DK DRO1TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRO1TR
CH
      SUBROUTINE DRO1TR(SF,CF,ST,CT,SG,CG)
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
      EXTERNAL DRODIS
      DATA CMPIX/32./
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      DATA IPDEB/0/
      LOGICAL FBAR(2),FOUT
      LOGICAL DCCTR
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_RES,J_REY'
      CALL DPARAM(10
     &  ,J_PTE,J_RES,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      IF(MODEDT.NE.2) THEN
         CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
      ELSE
         FFRTO(2)=0.
         CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
         IF(FOUT) RETURN
      END IF
      IF(DCCTR(FFRTO(4),FFRTO(5))) RETURN
      IHTFR=MIN(IHTRDO(3),6)
      IHTTO=MIN(IHTRDO(4),7)
      IF(IHTFR.GE.IHTTO) RETURN
      F1=FFRTO(IHTFR)
      F2=FFRTO(IHTTO)
      IF(F1.GE.F2) RETURN
      IF(PARADA(4,J_RES).EQ.1.) THEN
        ES= PARADA(2,J_RES)*CMPIX/AHSCDQ
        EY=-PARADA(2,J_REY)*CMPIX/AHSCDQ
      ELSE
        ES=0.
      END IF
      IF(FPIKDP.AND.IPDEB.EQ.1) THEN
        CALL DROPIT(F1,F2,SF,CF,ST,CT,SG,CG,ES,EY)
      ELSE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
        X1=DHELIX(F1,IVXXDV)
        Y1=DHELIX(F1,IVYYDV)
        Z1=DHELIX(F1,IVZZDV)
        X2= CF*X1+SF*Y1
        Y2=-SF*X1+CF*Y1
        H1= CT*X2+ST*Z1
        Z3=-ST*X2+CT*Z1
        V1= CG*Y2+SG*Z3
        IF(ES.NE.0.) THEN
          W=-SG*Y2+CG*Z3
          ESW=ES+W
          H1=H1+(EY-H1)*W/ESW
          V1=V1*ES/ESW
        END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
        X1=DHELIX(F2,IVXXDV)
        Y1=DHELIX(F2,IVYYDV)
        Z1=DHELIX(F2,IVZZDV)
        X2= CF*X1+SF*Y1
        Y2=-SF*X1+CF*Y1
        H2= CT*X2+ST*Z1
        Z3=-ST*X2+CT*Z1
        V2= CG*Y2+SG*Z3
        IF(ES.NE.0.) THEN
          W=-SG*Y2+CG*Z3
          ESW=ES+W
          H2=H2+(EY-H2)*W/ESW
          V2=V2*ES/ESW
        END IF
        CALL DRODS0(SF,CF,ST,CT,SG,CG,ES,EY)
        CALL DDRAWA(DRODIS,F1,H1,V1,F2,H2,V2)
      END IF
      END
*DK DRO3AX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRO3AX
CH
      SUBROUTINE DRO3AX(SF,CF,ST,CT,SG,CG)
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
      DIMENSION HX(2),VY(2)
      DATA Q1/.1/,Q2/1.6/,D3D/200./
      DIMENSION CXL(4),CXH(4),CYL(4),CYH(4)
      DATA CXL/1.,1.,0.,0./
      DATA CXH/0.,0.,1.,1./
      DATA CYL/0.,1.,0.,1./
      DATA CYH/1.,0.,1.,0./
      DATA NC/1/
      CHARACTER *6 TUNIT(3)
      DATA TUNIT /'BARREL','ENDCAP','BAR+EC'/
      DATA RVDI,RVDO,ZVD/4.3,12.,10./,RTPI,ZTP/35.,220./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO,J_RRM'
      CALL DPARAM(10
     &  ,J_PTO,J_RRM)

      TPARDA=
     &  'J_RWV,J_RWE,J_RWH'
      CALL DPARAM(43
     &  ,J_RWV,J_RWE,J_RWH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DLINDD=PDCODD(2,LIDTDD)
      NROM=PARADA(2,J_RRM)
      IF(NROM.EQ.6.AND.PARADA(2,J_PTO).LT.TPOFDU) NROM=0
      IF(NROM.NE.6) THEN
        IF((.NOT.FPIKDP).AND.PDCODD(2,ISTYDD).GE.2.
     &    .AND.NOCLDT.EQ.0) THEN
          CALL DQLEVL(ICTPDD)
          CALL DGRAR(HLOWDG(IAREDO),VLOWDG(IAREDO),
     &               HHGHDG(IAREDO),VHGHDG(IAREDO))
        END IF
      END IF
      GO TO (10,20,20,20,20,60,70,70,70) NROM
C     ........................................................ NO DETECTOR ...
   10 GO TO 99
C     ............................................. 3 AXES at one of 4 corners
   20 NC=NROM-1
      D=Q1*(HHGHDG(1)-HLOWDG(1))
      DD=D*Q2
      HX(2)=CXH(NC)*(HHGHDG(IAREDO)-DD)+CXL(NC)*(HLOWDG(IAREDO)+DD)
      VY(2)=CYH(NC)*(VHGHDG(IAREDO)-DD)+CYL(NC)*(VLOWDG(IAREDO)+DD)
      X1=D
      X2= CF*X1
      Y2=-SF*X1
      X3= CT*X2
      Z3=-ST*X2
      Y3= CG*Y2+SG*Z3
      HX(1)=HX(2)+X3
      VY(1)=VY(2)+Y3
      CALL DGLEVL(10)
      CALL DGDRAW(2,HX,VY)
      CALL DGLEVL(8)
      CALL DGTEXT(HX,VY,'X',1)
      Y1=D
      X2=SF*Y1
      Y2=CF*Y1
      X3= CT*X2
      Z3=-ST*X2
      Y3= CG*Y2+SG*Z3
      HX(1)=HX(2)+X3
      VY(1)=VY(2)+Y3
      CALL DGLEVL(12)
      CALL DGDRAW(2,HX,VY)
      CALL DGLEVL(8)
      CALL DGTEXT(HX,VY,'Y',1)
      Z1=D
      X3=ST*Z1
      Z3=CT*Z1
      Y3=SG*Z3
      HX(1)=HX(2)+X3
      VY(1)=VY(2)+Y3
      CALL DGLEVL(14)
      CALL DGDRAW(2,HX,VY)
      CALL DGLEVL(8)
      CALL DGTEXT(HX,VY,'Z',1)
      GO TO 99
C     ......................................................... drawing of TPC
   60 CALL DODTPC('3-D')
      IF(PARADA(4,J_RWV).GT.0.) THEN
        CALL   DODCYL(RTPI,-ZTP,ZTP,KCTPDD)
        IF(     PARADA(2,J_RWV).EQ.2.) THEN
          CALL DODCYL(RVDI,-ZVD,ZVD,KCVDDD)
        ELSE IF(PARADA(2,J_RWV).EQ.3.) THEN
          CALL DODCYL(RVDO,-ZVD,ZVD,KCVDDD)
        ELSE IF(PARADA(2,J_RWV).EQ.4.) THEN
          CALL DODCYL(RVDI,-ZVD,ZVD,KCVDDD)
          CALL DODCYL(RVDO,-ZVD,ZVD,KCVDDD)
        END IF
      END IF
      IF(PARADA(4,J_RWE).GT.0.) THEN
        FECADR=.TRUE.
        CALL DODECA('3-D',TUNIT(IFIX(PARADA(2,J_RWE))))
      END IF
      IF(PARADA(4,J_RWH).GT.0.) THEN
        FHCADR=.TRUE.
        CALL DODHCA('3-D',TUNIT(IFIX(PARADA(2,J_RWH))))
      END IF
      GO TO 99
C     ................. 3 axes at centre with verticle lines to YZ,XZ,YX plane
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
   70 X1=D3D
      X2= CF*X1
      Y2=-SF*X1
      X3= CT*X2
      Z3=-ST*X2
      Y3= CG*Y2+SG*Z3
      CALL DQLEVL(KCLGDD)
      CALL DQL2E(X3,Y3,-X3,-Y3)
      CALL DGLEVL(8)
      CALL DQTXT(X3,Y3,'X',1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
      Y1=D3D
      X2=SF*Y1
      Y2=CF*Y1
      X3= CT*X2
      Z3=-ST*X2
      Y3= CG*Y2+SG*Z3
      CALL DQLEVL(KCLGDD)
      CALL DQL2E(X3,Y3,-X3,-Y3)
      CALL DGLEVL(8)
      CALL DQTXT(X3,Y3,'Y',1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
      Z1=D3D
      X3=ST*Z1
      Z3=CT*Z1
      Y3=SG*Z3
      CALL DQLEVL(KCLGDD)
      CALL DQL2E(X3,Y3,-X3,-Y3)
      CALL DGLEVL(8)
      CALL DQTXT(X3,Y3,'Z',1)
   99 DLINDD=PDCODD(2,LITRDD)
      END
*DK DROD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROD
CH
      SUBROUTINE DROD
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
C     ............................... 330 PIXELS = 10.3 CM  1 CM = 32 PIXELS
      DATA CMPIX/32./,NU/7/,ND/8/,V331/331./
      DATA MOTR/1/,LDEB/0/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_RES,J_REY'
      CALL DPARAM(15
     &  ,J_RES,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IARE=IAREDO
      CALL DQCL(IARE)
      IF((IAREDO.EQ.NU.OR.IAREDO.EQ.ND)
     &  .AND.PARADA(4,J_RES).EQ.1.) THEN
        DFWI0=DFWIDU(0)
        DFWI1=DFWIDU(1)
        DFWIDU(0)=0.
        DFWIDU(1)=0.
        HMIN=HMINDG(IAREDO)
        HHGH=HHGHDG(IAREDO)
        VMIN=VMINDG(IAREDO)
        VHGH=VHGHDG(IAREDO)
        HSID=0.5*(HMIN+HHGH)
        VMID=0.5*(VMIN+VHGH)
C       ................. this correction yields the same images on WS ...
        REY=PARADA(2,J_REY)
        E=REY*(VHGH-VMIN)/V331
        EC=E*CMPIX
        E2=EC*2
        HMINDG(IAREDO)=HSID-E2
        HHGHDG(IAREDO)=HSID
        VMINDG(IAREDO)=VMID-EC
        VHGHDG(IAREDO)=VMID+EC
        PARADA(2,J_REY)=-E
        FLEYDB=.TRUE.
        CALL DROD0
        FLEYDB=.FALSE.
        HMINDG(IAREDO)=HSID
        HHGHDG(IAREDO)=HSID+E2
        PARADA(2,J_REY)= E
        IF(LDEB.EQ.1) THEN
          MOT=MOTRDC
          MOTRDC=MOTR
          FDTRDC=.FALSE.
          FDTPDC=.FALSE.
          CALL DROD0
          MOTRDC=MOT
          FDTRDC=.FALSE.
          FDTPDC=.FALSE.
        ELSE
          CALL DROD0
        END IF
        HMINDG(IAREDO)=HMIN
        HHGHDG(IAREDO)=HHGH
        VMINDG(IAREDO)=VMIN
        VHGHDG(IAREDO)=VHGH
        DFWIDU(0)=DFWI0
        DFWIDU(1)=DFWI1
        PARADA(2,J_REY)=REY
      ELSE
        CALL DROD0
      END IF
      END
*DK DROD0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROD0
CH
      SUBROUTINE DROD0
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
      INCLUDE 'DALI_EX.INC'
      LOGICAL FNROT,FEMTY
      CHARACTER *2 TPVW(6),TH,TV,TPR
      DIMENSION FIVW(6),TEVW(6),ALVW(6)
      DATA TPVW/'YX','YZ','ZX','XZ','ZY','XY'/
      DATA FIVW/  0.,  0.,  0.,270., 90., 90./
      DATA TEVW/ 90.,  0., 90.,  0., 90., 90./
      DATA ALVW/  0.,  0., 90.,  0., 90.,180./
      DIMENSION HRB(4),VRB(4)
      DIMENSION SCA(4)
      DIMENSION HB(2),VB(2),ZZ(2)
      DATA ZZ/-1024.,1024./
      FNROT=.TRUE.
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DROD1
CH
      ENTRY DROD1
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
      FNROT=.FALSE.
      CALL DQCL(IAREDO)
    1 CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DDRFLG
      IF(FNOPDR) GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_RAL,J_RSV,J_RRM,J_RDY,J_RXX,J_RYY'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_RAL,J_RSV,J_RRM,J_RDY,J_RXX,J_RYY)
      TPARDA=
     &  'J_RZZ'
      CALL DPARAM(15
     &  ,J_RZZ)
      TPARDA=
     &  'J_CFL'
      CALL DPARAM(40
     &  ,J_CFL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      SF=SIND(PARADA(2,J_PFI))
      CF=COSD(PARADA(2,J_PFI))
      ST=SIND(90.-PARADA(2,J_PTE))
      CT=COSD(90.-PARADA(2,J_PTE))
      SG=SIND(PARADA(2,J_RAL))
      CG=COSD(PARADA(2,J_RAL))
      DO 700 K=1,6
         IF(PARADA(2,J_PFI).EQ.FIVW(K).AND.
     &      PARADA(2,J_PTE).EQ.TEVW(K).AND.
     &      PARADA(2,J_RAL).EQ.ALVW(K)) THEN
            TH=TPVW(K)(2:2)
            TV=TPVW(K)(1:1)
            GO TO 2
         END IF
  700 CONTINUE
      TH='X"'
      TV='Y"'
    2 IF(PARADA(4,J_RDY).NE.1.) THEN
        X1=PARADA(2,J_RXX)
        Y1=PARADA(2,J_RYY)
        Z1=PARADA(2,J_RZZ)
        X2= CF*X1+SF*Y1
        Y2=-SF*X1+CF*Y1
C       HM= CT*X2+ST*Z1
        Z3=-ST*X2+CT*Z1
        DY= CG*Y2+SG*Z3
        PARADA(2,J_RDY)=DY
      END IF
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,ADUM)
      ELSE IF(IZOMDO.EQ.0) THEN
        RFR=PARADA(2,J_PFR)
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,DY-RTO,RTO,DY+RTO,HRB,VRB)
        FEMTY=.TRUE.
      ELSE
        CALL DRO_CONE_TO_HV(HRB,VRB)
C       CALL DRORCB(HRB,VRB)
      END IF
      CALL DQRU(HRB,VRB)
      IF(ABS(PARADA(4,J_CFL)).EQ.2.) THEN
        CALL DSCD1('RO')
        GO TO 98
      END IF
      CALL DQSC0('2 SC')
      CALL DRO3AX(SF,CF,ST,CT,SG,CG)
      FZTRDT=.TRUE.
      CALL DHTINP
      CALL DCTYP0(1)
      TPR='RO'
      CALL DAPEF(TPR)
      IF (TPR.EQ.' ') GO TO 98 
      IF(IHTRDO(1).EQ.0) THEN
         CALL DROET1(SF,CF,ST,CT,SG,CG)
         CALL DQFR(IAREDO)
         RETURN
      ELSE IF(IHTRDO(1).EQ.4) THEN
        IF(PDCODD(4,LCNBDD).EQ.1.) THEN
          CALL DQLEVL(LCNBDD)
          DO 707 J=1,2
            HB(J) = ST*ZZ(J)
            Z3=-CT*ZZ(J)
            VB(J) = SG*Z3
  707       CONTINUE
          CALL DQLIE(HB,VB)
        END IF
        CALL DAP_TR
        CALL DAPEO('RO')
      ELSE IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
        IF(IHTRDO(2).EQ.1) THEN
          CALL DROTT(SF,CF,ST,CT,SG,CG)
C       ELSE IF(IHTRDO(2).GE.4) THEN
C         CALL D_AP_KALMAN_TR
        ELSE
C          CALL DROETA(SF,CF,ST,CT,SG,CG)
          CALL DAP_TR
C         ........................ draw V0 tracks
          CALL DAP_V0_TR('RO')
C         ........................ draw tracks calculated in VX
          CALL DAPTN('RO')
        END IF
      END IF
      IF(IHTRDO(1).LE.2) THEN
        IF(FTPCDR) THEN
          IF(IHTRDO(6).LE.1) THEN
            IF(PARADA(2,J_RRM).GE.7.) THEN
              CALL DAPPT('3D',DVTP,TPCODB,0)
            ELSE IF(PARADA(4,J_RSV).EQ.1.
     &        .AND.PARADA(2,J_RSV).GT.0.) THEN
              CALL DAPPT('RS',DVTP,TPCODB,0)
            END IF
            CALL DAPPT('RO',DVTP,TPCODB,0)
            CALL DAPVC('RO',0)
          ELSE IF(IHTRDO(6).EQ.2) THEN
            CALL DAPPT('RO',DVPADA,TPADDB,0)
          ELSE IF(IHTRDO(6).EQ.3) THEN
            CALL DAPPT('RO',DVTP,TBCODB,0)
          END IF
        END IF
      END IF
      CALL DAPTRF('RO')
      CALL DAPVX('RO',LDUM)
      CALL DAP_KNV('RO',LDUM)
      IF(FPIKDP) RETURN
      CALL DQMID
   98 IF(FNROT) THEN
         CALL DCTYEX('RO TPC',6)
         IF(FEMTY) THEN
           CALL DQSCA('H',HRB(1),HRB(3),'cm',2,TH,2)
           CALL DQSCA('V',VRB(1),VRB(3),'cm',2,TV,2)
         ELSE
           CALL DQSCA('H',SCA(1),SCA(2),'cm',2,' ',0)
           CALL DQSCA('V',SCA(3),SCA(4),'cm',2,' ',0)
         END IF
         CALL DLSITX(MTPCDL)
      END IF
   99 CALL DQFR(IAREDO)
      CALL DPCSAR
      RETURN
      END
*DK DRODIS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRODIS
CH
      SUBROUTINE DRODIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
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
      LOGICAL FC
      IF(FC) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,HM,VM)
         CALL DQL2EP(HM,VM,H2,V2)
         RETURN
      END IF
      FM=0.5*(F1+F2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
      X1=DHELIX(FM,IVXXDV)
      Y1=DHELIX(FM,IVYYDV)
      Z1=DHELIX(FM,IVZZDV)
      X2= CF*X1+SF*Y1
      Y2=-SF*X1+CF*Y1
      HM= CT*X2+ST*Z1
      Z3=-ST*X2+CT*Z1
      VM= CG*Y2+SG*Z3
      IF(ES.NE.0.) THEN
        W=-SG*Y2+CG*Z3
        ESW=ES+W
        HM=HM+(EY-HM)*W/ESW
        VM=VM*ES/ESW
      END IF
      DHU=H2-H1
      DVU=V2-V1
      DH21=AHSCDQ*DHU+BHSCDQ*DVU
      DV21=AVSCDQ*DHU+BVSCDQ*DVU
      DHU=HM-H1
      DVU=VM-V1
      DHM1=AHSCDQ*DHU+BHSCDQ*DVU
      DVM1=AVSCDQ*DHU+BVSCDQ*DVU
      DM=(DV21*DHM1-DH21*DVM1)**2/(DV21**2+DH21**2)
      IF(D.LT.DMAXDQ.AND.DM.LT.DMAXDQ) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,H2,V2)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRODS0
CH
      ENTRY DRODS0(SSF,CCF,SST,CCT,SSG,CCG,ESG,EYG)
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
      SF=SSF
      CF=CCF
      ST=SST
      CT=CCT
      SG=SSG
      CG=CCG
      ES=ESG
      EY=EYG
      END
*DK DROET1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROET1
CH
      SUBROUTINE DROET1(SF,CF,ST,CT,SG,CG)
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
      DIMENSION TP(5)
      CALL DCIRCL(TP)
      CALL DQLEVL(LCNCDD)
      CALL DGLEVL(MCOLDL(MTPCDL))
      CALL DVXT(0,XYZVDT)
      CALL DRO1TR(TP,SF,CF,ST,CT,SG,CG)
      END
*DK DROETA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROETA
CH
      SUBROUTINE DROETA(SF,CF,ST,CT,SG,CG)
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
      DATA W2/2./
      EQUIVALENCE (KPIKDP,N)
      LOGICAL FOUT,DVM0
      LOGICAL DCCVX
      EXTERNAL DVTR0
C     CALL DLHITR('TR',MTPCDL,ICOL)
      CALL DAPTRN('RO',0)
      IF(IHTRDO(2).LE.2) THEN
         CALL DSCTR
         CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
         IF(FOUT) RETURN
         MODEDT=1
         IF(.NOT.FPIKDP.AND.PDCODD(4,NFTRDD).GT.0.) THEN
           CALL DQLEVL(NFTRDD)
           DLINDD=DLINDD+W2
           DO 301 N=NUM1,NUM2
             IF(FNOTDT(N)) GO TO 301
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             CALL DRO1TR(TP,SF,CF,ST,CT,SG,CG)
  301      CONTINUE
           DLINDD=DLINDD-W2
         END IF
         DO 1 N=NUM1,NUM2
           IF(FNOTDT(N)) GO TO 1
           IF(FVTRDC) CALL DGLEVL(NCTRDC(N))
           IF(IHTRDO(1).NE.4) THEN
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             CALL DRO1TR(TP,SF,CF,ST,CT,SG,CG)
             CALL DAPTRN('RO',N)
           ELSE
             CALL DRO1CT(N,SF,CF,ST,CT,SG,CG)
           END IF
    1    CONTINUE
      ELSE IF(IHTRDO(1).NE.4) THEN
         MODEDT=2
         CALL DMCNVX(NVTX1)
         IF(NVTX1.LE.0) RETURN
         DO 12 NV=1,NVTX1
            CALL DMCVX(NV,KT1,KT2)
            IF(KT2.LE.0) GO TO 12
            DO 11 N=KT1,KT2
               IF(DVM0(N)) GO TO 11
               CALL DMCTR(N,FOUT)
               IF(FOUT) GO TO 11
               CALL DRO1TR(TP,SF,CF,ST,CT,SG,CG)
   11       CONTINUE
   12    CONTINUE
         IF(.NOT.FPIKDP) THEN
            IF(NVTX1.LE.0) RETURN
            CALL DPAR_SET_CO(49,'CMC')
            IF(ICOLDP.LT.0) RETURN
            CALL DPARGV(67,'SSY',2,SYMB)
            CALL DPARGV(67,'SSZ',2,SYSZ)
            MSYMB=SYMB
            CALL DQPD0(MSYMB,SZVXDT*BOXSDU*SYSZ,0.)
C\\         DS=SZVXDT*BOXSDU*WDSNDL(2,4,MTPCDL)
            CALL DQPD0(3,DS,0.)
C           CALL DGLEVL(NCVXDT)
            DO 13 N=1,NVTX1
               IF(FMVXDT(N)) THEN
                  CALL DVMCVX(N,KDUM1,KDUM2,KDUM3,VTX1DT)
                  IF(DCCVX(VTX1DT)) GO TO 13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
                  X1=VTX1DT(1)
                  Y1=VTX1DT(2)
                  Z1=VTX1DT(3)
                  X2= CF*X1+SF*Y1
                  Y2=-SF*X1+CF*Y1
                  X3= CT*X2+ST*Z1
                  Z3=-ST*X2+CT*Z1
                  Y3= CG*Y2+SG*Z3
                  CALL DQPD(H,V)
C                  CALL DLSYMC(MTPCDL,3,'N','N','S',X3,Y3,DS,DS)
               END IF
   13       CONTINUE
         END IF
      END IF
      END
*DK DRO1CT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRO1CT
CH
      SUBROUTINE DRO1CT(NTR,SF,CF,ST,CT,SG,CG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   DRAW MOMENTUM VECTOR OF CHARGED TRACKS
*CA DALLCO
      DATA PFAC/1./
      INCLUDE 'DALI_CF.INC'
      CALL DVTRP(NTR,PX,PY,PZ,IC)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC ROTATE AROUND XROTDR,YROTDR,ZROTDR
      X1=PX*PFAC
      Y1=PY*PFAC
      Z1=PZ*PFAC
      X2= CF*X1+SF*Y1
      Y2=-SF*X1+CF*Y1
      H = CT*X2+ST*Z1
      Z3=-ST*X2+CT*Z1
      V = CG*Y2+SG*Z3
      IF(FPIKDP) THEN
        CALL DQPIK(H,V)
      ELSE
        CALL DQL2E(0.,0.,H,V)
      END IF
      END
*DK DRO_CONE_TO_HV
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRO_CONE_TO_HV
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DRORCB
CH
      SUBROUTINE DRO_CONE_TO_HV(HRB,VRB)
C     SUBROUTINE DRORCB(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO,J_PAS,J_RDY'
      CALL DPARAM(10
     &  ,J_PFR,J_PTO,J_PAS,J_RDY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      HRB(1)=PARADA(2,J_PFR)
      HRB(3)=PARADA(2,J_PTO)
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      DV=ABS(0.5*(HRB(3)-HRB(1)))
      IF(PARADA(2,J_PAS).NE.0.) DV=DV/PARADA(2,J_PAS)
      VRB(1)=PARADA(2,J_RDY)-DV
      VRB(3)=PARADA(2,J_RDY)+DV
      VRB(2)=VRB(1)
      VRB(4)=VRB(3)
      END
*DK DROPIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROPIT
CH
      SUBROUTINE DROPIT(F1,F2,SF,CF,ST,CT,SG,CG,ES,EY)
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
C      DATA FP/20./,KP/20/
C      DF=(F2-F1)/FP
C      F=F1
C      DO 700 K=1,KP
C        F=F+DF
      DF=F2-F1
      DO 700 K=1,MTRSDP
        F=F1+TRSTDP(K)*DF
        X1=DHELIX(F,IVXXDV)
        Y1=DHELIX(F,IVYYDV)
        Z1=DHELIX(F,IVZZDV)
        X2= CF*X1+SF*Y1
        Y2=-SF*X1+CF*Y1
        H1= CT*X2+ST*Z1
        Z3=-ST*X2+CT*Z1
        V1= CG*Y2+SG*Z3
        IF(ES.NE.0.) THEN
          W=-SG*Y2+CG*Z3
          ESW=ES+W
          H1=H1+(EY-H1)*W/ESW
          V1=V1*ES/ESW
        END IF
        CALL DQPIK(H1,V1)
  700 CONTINUE
      END
*DK DROTT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROTT
CH
      SUBROUTINE DROTT(SF,CF,ST,CT,SG,CG)
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
      DIMENSION H(2),V(2)
      DATA CMPIX/32./
      EQUIVALENCE (K,KPIKDP)
      LOGICAL FOUT
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM0,NUM2,FOUT)
      IF(FOUT) RETURN
C     CALL DVTQ0(NUM2)
C     IF(NUM2.EQ.0) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_RES,J_REY'
      CALL DPARAM(10
     &  ,J_PFI,J_RES,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
C      CALL DLHITR('TR',MTPCDL,ICOL)
      IF(PARADA(4,J_RES).EQ.1.) THEN
        ES= PARADA(2,J_RES)*CMPIX/AHSCDQ
        EY=-PARADA(2,J_REY)*CMPIX/AHSCDQ
      ELSE
        ES=0.
      END IF
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHT(M,NUM)
         IF(NUM.LE.0) GO TO 3
         K1=IDVCHT(1)
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
    2    DO 1 KIND=1,NUM
            K=IDVCHT(KIND)
            IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
            TE=DVTP(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
               I=0
               GO TO 1
            END IF
            FI=DFINXT(FIMID,DVTP(IVFIDV,K))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            X1=DVTP(IVXXDV,K)
            Y1=DVTP(IVYYDV,K)
            Z1=DVTP(IVZZDV,K)
            X2= CF*X1+SF*Y1
            Y2=-SF*X1+CF*Y1
            H(I)= CT*X2+ST*Z1
            Z3  =-ST*X2+CT*Z1
            V(I)= CG*Y2+SG*Z3
            IF(ES.NE.0.) THEN
              W=-SG*Y2+CG*Z3
              ESW=ES+W
              H(I)=H(I)+(EY-H(I))*W/ESW
              V(I)=V(I)*ES/ESW
            END IF
            IF(FPIKDP) THEN
               CALL DQPIK(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               CALL DQLIE(H,V)
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DROSMR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROSMR
CH
      SUBROUTINE DROSMR(TA)
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
      PARAMETER (NLIN=99,MVX=9)
      DIMENSION X1(2,NLIN),Y1(2,NLIN),Z1(2,NLIN),NCOL(NLIN)
      DIMENSION HT(2,NLIN),VT(2,NLIN)
      DIMENSION XV(MVX),YV(MVX),ZV(MVX),JCOL(MVX)
      DIMENSION HVX(MVX),VVX(MVX)
      DIMENSION HRB(4),VRB(4),HMO(2),VMO(2)
      DIMENSION IP(2)
      DATA IP/8,4/,NDASH/2/
      DATA L1/1/,L8/8/,J1/1/,D1/1./,DS/20./,DA/5./,DL1/1./
      DATA Q1/0.0/,Q2/0.000000005/,QP/0.001/,DC/20./
      DATA JEND/99999/,JPIC/20/,WLONG/0.04/
      DATA SVX/6./
      CHARACTER *2 TA
      DATA IDEB/0/
      LOGICAL FEMTY,F1,FBUT(3),DGPNTR
C     ......... take out when GT;RO:SS is running on HP
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO,J_RAL,J_RDY,J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PTO,J_RAL,J_RDY,J_RXX,J_RYY,J_RZZ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      HVL=0.
    1 JSTOP=0
      CALL DQCL(IAREDO)
      DY=PARADA(2,J_RDY)
      IF(IZOMDO.EQ.0) THEN
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      ELSE
        PARADA(2,J_RDY)=0.
        CALL DGRBST(HRB,VRB,FEMTY)
        IF(FEMTY) CALL DRO_CONE_TO_HV(HRB,VRB)
C       IF(FEMTY) CALL DRORCB(HRB,VRB)
        PARADA(2,J_RDY)=DY
      END IF
      DY=-DY
      CALL DQRU(HRB,VRB)
      CALL DCTYEX('RO TPC',6)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'Y"',2)
      CALL DLSITX(MTPCDL)
      CALL DROSTR(NLIN,NUM,NFRFT,X1,Y1,Z1,NCOL)
      CALL DROSVX(MVX ,NVX,XV,YV,ZV,JCOL)
      IF(NVX.GT.0) CALL DQPD0(8,SVX,0.)
      H1=HLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V1=VLOWDG(IAREDO)
      V2=VHGHDG(IAREDO)
      IF(NOCLDT.EQ.0) THEN
        CALL DGLEVL(J1)
C       ................................. IF J1#L1 ONE SEES THE TOTAL MOVEMENT
        CALL DQFAR(H1+D1,V1+D1,H2-D1,V2-D1)
        CALL DQDWI
      END IF
      HM=0.5*(H1+H2)
      VM=0.5*(V1+V2)
      VLS=VM-DS
      VHS=VM+DS
      VLA=VM-DA
      VHA=VM+DA
      IF(TA.EQ.'SS') THEN
        CALL DGSCUR(H1+DC,VM)
      ELSE
        CALL DGSCUR(H2-DC,VM)
      END IF
      FI=    PARADA(2,J_PFI)
      TE=90.-PARADA(2,J_PTE)
      AL=    PARADA(2,J_RAL)
      AL0=AL
      CALL DWRT('To stop move pointer out of the DALI window or')
      CALL DWRT('press two mouse buttons together: first 1, then 3')
C      WRITE(TXTADW,1000) AL
C 1000 FORMAT('AL=',F6.2)
C      CALL DWRC
      CALL DWRT('Move mouse.')
      SF=SIND(FI)
      CF=COSD(FI)
      ST=SIND(TE)
      CT=COSD(TE)
   10 IF(HVL.GT.0.) THEN
        DLIN=DLINDD
        DLINDD=DL1
        CALL DGLEVL(L8)
        CALL DGDRAW(2,HMO,VMO)
        DLINDD=DLIN
      END IF
      SG=SIND(AL)
      CG=COSD(AL)
      IF(PARADA(4,J_RDY).NE.1.) THEN
        X0=PARADA(2,J_RXX)
        Y0=PARADA(2,J_RYY)
        Z0=PARADA(2,J_RZZ)
        X2= CF*X0+SF*Y0
        Y2=-SF*X0+CF*Y0
C       HM= CT*X2+ST*Z0
        Z3=-ST*X2+CT*Z0
        DY= CG*Y2+SG*Z3
        PARADA(2,J_RDY)=DY
        DY=-DY
      END IF
      DO N=1,NFRFT
        DO I=1,2
          X2     = CF*X1(I,N)+SF*Y1(I,N)
          Y2     =-SF*X1(I,N)+CF*Y1(I,N)
          HT(I,N)= CT*X2     +ST*Z1(I,N)
          Z3     =-ST*X2     +CT*Z1(I,N)
          VT(I,N)= CG*Y2     +SG*Z3     +DY
        END DO
        CALL DGLEVL(NCOL(N))
        CALL DQLIE(HT(1,N),VT(1,N))
      END DO
      IF(NUM.GT.NFRFT) THEN
        CALL DGDASH(NDASH,IP)
        DO N=NFRFT+1,NUM
          DO I=1,2
            X2     = CF*X1(I,N)+SF*Y1(I,N)
            Y2     =-SF*X1(I,N)+CF*Y1(I,N)
            HT(I,N)= CT*X2     +ST*Z1(I,N)
            Z3     =-ST*X2     +CT*Z1(I,N)
            VT(I,N)= CG*Y2     +SG*Z3     +DY
          END DO
          CALL DGLEVL(NCOL(N))
          CALL DQLIE(HT(1,N),VT(1,N))
        END DO
        CALL DGDASH(0,0)
      END IF
      DO N=1,NVX
        X2    = CF*XV(N)+SF*YV(N)
        Y2    =-SF*XV(N)+CF*YV(N)
        HVX(N)= CT*X2     +ST*ZV(N)
        Z3    =-ST*X2     +CT*ZV(N)
        VVX(N)= CG*Y2     +SG*Z3     +DY
        CALL DGLEVL(JCOL(N))
        CALL DQARD(HVX(N),VVX(N))
      END DO
      CALL DQCHKX
      CALL DWAIT1(WLONG)
   20 IF(JSTOP.GT.JEND) GO TO 99
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
C       ...... BUTTON 1 DOWN = WAIT1               PRIORITY 3
C       ...... BUTTON 2 DOWN = INVERSE DIRECTION   PRIORITY 2          
C       ...... BUTTON 3 DOWN = STOP                PRIORITY 1           
        IF(     FBUT(2)) THEN
          JSTOP=0
          GO TO 21
        ELSE IF(FBUT(1)) THEN
          IF(FBUT(3)) GO TO 99
          JSTOP=0
          F1=.TRUE.
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          IF(HVL.GT.0.) THEN
            DLIN=DLINDD
            DLINDD=DL1
            CALL DGLEVL(L1)
            CALL DGDRAW(2,HMO,VMO)
            DLINDD=DLIN
          END IF
          CALL DGEXEC
          IF(IDEB.EQ.0) CALL DQCHKX
          CALL DTDM
          CALL DTDMVL(HVL)
          IF(HVL.GT.0.) THEN
            HMO(1)=HVL
            HMO(2)=HVL
            VMO(1)=VLOWDG(IAREDO)
            VMO(2)=VHGHDG(IAREDO)
          END IF
          GO TO 1
        END IF
      ELSE
        GO TO 99
      END IF
   21 HC=JHC
      VC=JVC
      IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
        JSTOP=0
        HCOLD=HC
        VCOLD=VC
      END IF
      IF(TA.EQ.'SS') THEN
C       ............................................................. SPEED
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          JSTOP=JSTOP+1
          GO TO 20
        END IF
        AL=AL+DA
      ELSE
        IF(VC.GT.VHA) THEN
          DA=QP*(H2-HC)*(VC-VHA)
        ELSE IF(VC.LT.VLA) THEN
          DA=QP*(H2-HC)*(VC-VLA)
        ELSE
          JSTOP=JSTOP+1
          GO TO 20
        END IF
        IF(F1) THEN
          AL0=AL-DA
          F1=.FALSE.
        END IF
        AL=AL0+DA
      END IF
      IF(AL.GE.360.) THEN
        AL=AL-360.
      ELSE IF(AL.LT.0.) THEN
        AL=AL+360.
      END IF
      WRITE(TXTADW,1001) AL
 1001 FORMAT('AL=',F6.2,'m')
      CALL DWR_OVER_PRINT(10)
      PARADA(2,J_RAL)=AL
      CALL DGLEVL(L1)
      DO N=1,NUM
        CALL DQLIE(HT(1,N),VT(1,N))
      END DO
      DO N=1,NVX
        CALL DQARD(HVX(N),VVX(N))
      END DO
      JSTOP=JSTOP+JPIC
      GO TO 10
   99 CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DROSTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROSTR
CH
      SUBROUTINE DROSTR(NLIN,N,NFRFT,X,Y,Z,NCOL)
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
C     DATA DFI/0.2/
      DIMENSION X(2,*),Y(2,*),Z(2,*),NCOL(*)
      DIMENSION FI(2)
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      LOGICAL FOUT,DVM0,FBAR(2)
      LOGICAL DCCTR
      CALL DPARGV(15,'RDF',2,DFI)
      IF(IHTRDO(2).LE.2) THEN
        FZTRDT=.TRUE.
        CALL DHTINP
        CALL DSCTR
        CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
        IF(FOUT) RETURN
        N=0
        DO 1 K=NUM1,NUM2
          IF(FNOTDT(K)) GO TO 1
          CALL DVTRV(K)
          CALL DVXT(K,XYZVDT)
          CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
          IF(DCCTR(FFRTO(4),FFRTO(5))) GO TO 1
          IHTFR=MIN(IHTRDO(3),2)
          FI(1)=FFRTO(IHTFR)
          FI(2)=FI(1)+DFI
          N=N+1
          NCOL(N)=NCTRDC(K)
          DO I=1,2
            X(I,N)=DHELIX(FI(I),IVXXDV)
            Y(I,N)=DHELIX(FI(I),IVYYDV)
            Z(I,N)=DHELIX(FI(I),IVZZDV)
          END DO
          IF(N.GE.NLIN) RETURN
    1   CONTINUE
        CALL DVXTN
        NFRFT=N
        IF(NTRKDN.GT.0) THEN
          DO K=1,NTRKDN
            IF(ICOLDN(K).GE.0) THEN
              N=N+1
              IF(ICOLDN(K).EQ.0) THEN
                NCOL(N)=8
              ELSE
                NCOL(N)=ICOLDN(K)
              END IF
              MM=2
              DO I=1,2
                X(I,N)=XYZTDN(1,I,K)
                Y(I,N)=XYZTDN(2,I,K)
                Z(I,N)=XYZTDN(3,I,K)
              END DO
              IF(N.GE.NLIN) RETURN
            END IF
          END DO
        END IF
      ELSE IF(IHTRDO(1).NE.4) THEN
        MODEDT=2
        CALL DMCNVX(NVTX1)
        N=0
        IF(NVTX1.LE.0) RETURN
        DO 12 NV=1,NVTX1
          CALL DMCVX(NV,KT1,KT2)
          IF(KT2.LE.0) GO TO 12
          DO 11 K=KT1,KT2
            IF(DVM0(K)) GO TO 11
            CALL DMCTR(K,FOUT)
            IF(FOUT) GO TO 11
            CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
            IF(DCCTR(FFRTO(4),FFRTO(5))) GO TO 11
            FFRTO(2)=0.
            IHTFR=MIN(IHTRDO(3),2)
            FI(1)=FFRTO(IHTFR)
            FI(2)=FI(1)+DFI
            N=N+1
            DO I=1,2
              X(I,N)=DHELIX(FI(I),IVXXDV)
              Y(I,N)=DHELIX(FI(I),IVYYDV)
              Z(I,N)=DHELIX(FI(I),IVZZDV)
            END DO
   11     CONTINUE
   12   CONTINUE
      END IF
      END
*DK DROSVX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DROSTR
CH
      SUBROUTINE DROSVX(MVX,N,X,Y,Z,NCOL)
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
      DIMENSION X(*),Y(*),Z(*),NCOL(*)
      DIMENSION HEL(61),VEL(61),HAX(2,2),VAX(2,2),XYZ(3)
      N=0
      IF(BNUMDB(4,PYERDB).LE.0.) RETURN
      NVX=BNUMDB(2,PYERDB)
      IF(NVX.EQ.0) RETURN
      DO K=1,NVX
        IF(N.GE.MVX) GO TO 9
        CALL DVXC(K,JCOL)
        IF(JCOL.GE.0) THEN
          CALL DVVX(K,HAX,VAX,HEL,VEL,ARG,XYZ)
          N=N+1
          X(N)=XYZ(1)
          Y(N)=XYZ(2)
          Z(N)=XYZ(3)
          NCOL(N)=JCOL
        END IF
      END DO
    9 END
*DK DRTSP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRTSP
CH
      SUBROUTINE DRTSP
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
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRB(4),VRB(4)
C     ................................................. DATA RFR/30./,RTO/180./
      DATA DR/150./,D173/-173./,D7/-7./
C     DATA ZTO/428./,RCOR/253./,DD/50./,AMIN/1./
      DIMENSION HRT1(4),HRT2(4),VRTU(4),VRTD(4)
      DATA HRT1/-172.,-141., -39., -39./
      DATA HRT2/  -8., -39.,-141.,-141./
      DATA VRTU/   0., 150., 150.,   0./
      DATA VRTD/   0.,-150.,-150.,   0./
C      DIMENSION QWIND(0:12,0:1)
CC                 0  1  2  3  4  5  6  U  D  L  M  R  S
C      DATA QWIND/.5,1.,1.,1.,1.,1.,1.,1.,1.,.5,.5,.5,.5,
C     &           0.,0.,0.,0.,0.,0.,0.,0.,0.,.7,.7,0.,0./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PDT,J_PRT'
      CALL DPARAM(10
     &  ,J_PTE,J_PDT,J_PRT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PRT).EQ.1..AND.PARADA(2,J_PRT).GE.1.) THEN
        VLOW=VLOWDG(IAREDO)
        DV=0.01*PARADA(2,J_PRT)*(VHGHDG(IAREDO)-VLOW)
C     &    *QWIND(IAREDO,IFIX(WISUDW))
        IF(DV.EQ.0.) RETURN
        IF(IZOMDO.EQ.0.AND.PARADA(4,J_PTE).NE.0.) THEN
          DV=2.*DV
          CALL DQRER(0,D173,-DR,D7,DR,HRB,VRB)
        ELSE
          CALL DQRER(0,D173, 0.,D7,DR,HRB,VRB)
        END IF
        VLOWDG(IAREDO)=VHGHDG(IAREDO)-DV
        IF(IZOMDO.NE.0.) THEN
          DT=0.5*PARADA(2,J_PDT)
          HRB(1)=-PARADA(2,J_PTE)-DT
          HRB(3)=-PARADA(2,J_PTE)+DT
          HRB(2)=HRB(3)
          HRB(4)=HRB(1)
        END IF
        CALL DQRU(HRB,VRB)
        IF(.NOT.FPIKDP) THEN
          IF(PDCODD(2,ISTYDD).GE.2) THEN
            CALL DQRA0(2,ICTPDD)
            CALL DQRAR(HRT1,VRTU)
            CALL DQRAR(HRT2,VRTU)
            IF(IZOMDO.EQ.0.AND.PARADA(4,J_PTE).NE.0.) THEN
              CALL DQRAR(HRT1,VRTD)
              CALL DQRAR(HRT2,VRTD)
            END IF
          END IF
          CALL DQLEVL(KCTPDD)
          CALL DQLIE(HRT1,VRTU)
          CALL DQLIE(HRT2,VRTU)
          IF(IZOMDO.EQ.0.AND.PARADA(4,J_PTE).NE.0.) THEN
            CALL DQLIE(HRT1,VRTD)
            CALL DQLIE(HRT2,VRTD)
          END IF
          CALL DQDWI
          IF(IZOMDO.EQ.0.AND.PARADA(4,J_PTE).NE.0.)
     &    CALL DQL2E(-180.,0.,0.,0.)
        END IF
        CALL DAPPT('RT',DVTP,TPCODB,0)
        VHGHDG(IAREDO)=VLOWDG(IAREDO)
        VLOWDG(IAREDO)=VLOW
      END IF
      END
*DK DRZ1TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZ1TR
CH
      SUBROUTINE DRZ1TR
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
      EXTERNAL DRZDIS
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      DATA IPDEB/0/
      LOGICAL FBAR(2),FOUT,FDOBL
      LOGICAL DCCTR
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      FIMID=PARADA(2,J_PFI)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      IF(PARADA(4,J_PTE).NE.0.) THEN
         FDOBL=.TRUE.
      ELSE
         FDOBL=.FALSE.
      END IF
      IF(MODEDT.NE.2) THEN
         CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
      ELSE
         FFRTO(2)=0.
         CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
         IF(FOUT) RETURN
      END IF
      IF(DCCTR(FFRTO(4),FFRTO(5))) RETURN
      IHTFR=MIN(IHTRDO(3),7)
      IHTTO=MIN(IHTRDO(4),7)
      IF(IHTFR.GE.IHTTO) RETURN
      F1=FFRTO(IHTFR)
      H1=DHELIX(F1,IVZZDV)
      IF(FDOBL) THEN
         FI=DFINXT(FIMID,DHELIX(F1,IVFIDV))
         IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
            V1=DHELIX(F1,IVRODV)
         ELSE
            V1=-DHELIX(F1,IVRODV)
         END IF
      ELSE
         V1=DHELIX(F1,IVRODV)
      END IF
      F2=FFRTO(IHTTO)
      IF(PARADA(2,J_PTO).GE.1001.AND.
     &  PARADA(2,J_PTO).LE.1004.) THEN
         F2=MIN(F2,FFRTO(5))
      END IF
      IF(F1.GE.F2) RETURN
      IF(FPIKDP.AND.IPDEB.EQ.1) THEN
        CALL DRZPIT(F1,F2,FDOBL,FMIN,FMAX)
      ELSE
        H2=DHELIX(F2,IVZZDV)
        IF(FDOBL) THEN
           FI=DFINXT(FIMID,DHELIX(F2,IVFIDV))
           IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
              V2=DHELIX(F2,IVRODV)
           ELSE
              V2=-DHELIX(F2,IVRODV)
           END IF
        ELSE
           V2=DHELIX(F2,IVRODV)
        END IF
C        IF(SP.NE.0.) THEN
C           Q=(1.+SP*(FI1-FIMID))
C           H1=H1*Q
C           V1=V1*Q
C           Q=(1.+SP*(FI2-FIMID))
C           H2=H2*Q
C           V2=V2*Q
C        END IF
        CALL DRZDS0(FDOBL,FIMID,FMIN,FMAX,SP)
        CALL DDRAWA(DRZDIS,F1,H1,V1,F2,H2,V2)
      END IF
      END
*DK DRZCC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZCC
CH
      SUBROUTINE DRZCC(SP)
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
      DIMENSION H(2),V(2)
      EQUIVALENCE (K,KPIKDP)
      DATA SHRINK/0./
      LOGICAL FOUT
      IF(FPIKDP) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      QPI=SHRINK/PIFCDK
C      CALL DLHITR('TR',MTPCDL,ICOL)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
C        Find numbers of ITC and TPC coordinates for track M
         CALL DVCHCC(M,K1,K2,NFI)
         IF(K1.EQ.0) GO TO 3
C        Set color for track M
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
C        load itc coordinate into H(1),V(1)
         I=0
         TE=DVIT(IVTEDV,K2,NFI)
         IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
            I=0
            GO TO 1
         END IF
         FI=DFINXT(FIMID,DVIT(IVFIDV,K2,NFI))
         IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
            I=0
            GO TO 1
         END IF
         I=I+1
         H(I)=DVIT(IVZZDV,K2,1)
         IF(PARADA(4,J_PTE).NE.0.) THEN
            IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
               V(I)=DVIT(IVRODV,K2,NFI)
            ELSE
               V(I)=-DVIT(IVRODV,K2,NFI)
            END IF
         ELSE
            V(I)=DVIT(IVRODV,K2,NFI)
         END IF
C         IF(SP.NE.0.) THEN
C            Q=(1.+SP*(FI-FIMID))
C            H(I)=H(I)*Q
C            V(I)=V(I)*Q
C         END IF
C        add tpc coordinate into H(2),V(2)
         TE=DVTP(IVTEDV,K1)
         IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
            I=0
            GO TO 1
         END IF
         FI=DFINXT(FIMID,DVTP(IVFIDV,K1))
         IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
            I=0
            GO TO 1
         END IF
         I=I+1
         IF(PARADA(4,J_PTE).NE.0.) THEN
            FI=DFINXT(FIMID,DVTP(IVFIDV,K1))
            IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
               V(I)=DVTP(IVRODV,K1)
            ELSE
               V(I)=-DVTP(IVRODV,K1)
            END IF
         ELSE
            V(I)=DVTP(IVRODV,K1)
         END IF
         H(I)=DVTP(IVZZDV,K1)
C        IF(SP.NE.0.) THEN
C           Q=(1.+SP*(FI-FIMID))
C           H(I)=H(I)*Q
C           V(I)=V(I)*Q
C        END IF
C        CONNECT ITC TRACK SEGMENT TO TPC TRACK SEGMENT
         IF(I.EQ.2) THEN
            IF(SIGN(1.,V(1)).EQ.SIGN(1.,V(2))) THEN
C               IF(FPIKDP) THEN
C                  CALL DQPIK(H,V)
C                  CALL DQPIK(H(2),V(2))
C               ELSE
C                  CALL DQLIE(H,V)
C               END IF
              CALL DQLIE(H,V)
            END IF
         END IF
    1 CONTINUE
    3 CONTINUE
      END

*DK DRZD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZD
CH
      SUBROUTINE DRZD
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
      INCLUDE 'DALI_EX.INC'
      INCLUDE 'DALI_UIS.INC'
      LOGICAL FEMTY,FSR
      CHARACTER *3 T3
      CHARACTER *5 TH
      DIMENSION HRB(4),VRB(4),PNOPS(4)
      DIMENSION SCA(4)
      DATA PNOPS/0.,0.,0.,-1./,MO/3/
C     DATA ZTO/428./,RCOR/253./,DD/50./,AMIN/1./,HMIN,VMIN/0.,0./
      CALL DQCL(IAREDO)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PIT,J_PFR,J_PTO,J_PAS,J_PDS,J_PBZ,J_PHM,J_PDH'
      CALL DPARAM(10
     &  ,J_PTE,J_PIT,J_PFR,J_PTO,J_PAS,J_PDS,J_PBZ,J_PHM,J_PDH)
      TPARDA=
     &  'J_CFL'
      CALL DPARAM(40
     &  ,J_CFL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(ABS(PARADA(4,J_CFL)).EQ.1.
     &  .AND.PARADA(2,J_PTO).GT.DSOFDU) THEN
        CALL DQPRS0(PARADA(1,J_PDS))
        FPSQDQ=.TRUE.
      ELSE
        CALL DQPRS0(PNOPS)
      END IF
      DFWI=DFWIDU(IZOMDO)
      IF(FPRSDQ) DFWIDU(IZOMDO)=MOD(DFWI,100.)-MOD(DFWI,10.)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DDRFLG
      IF(FNOPDR) GO TO 99
      RFR=PARADA(2,J_PFR)
      RTO=PARADA(2,J_PTO)
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,PARADA(2,J_PAS))
      ELSE IF(IZOMDO.EQ.0.) THEN
        IF(PARADA(4,J_PTE).NE.0.) THEN
C         SIGNED RHO
          FSR=.TRUE.
          CALL DQRER(MO,-RTO,-RTO,RTO,RTO,HRB,VRB)
        ELSE
C         POSITIVE RHO
          FSR=.FALSE.
          IF(IAREDO.EQ.7.OR.IAREDO.EQ.8) THEN
            CALL DQRER(2,RTO,0.,0.,0.,HRB,VRB)
          ELSE
            CALL DQRER(0,-RTO,0.,RTO,RTO,HRB,VRB)
          END IF
        END IF
        TH='Z   '
        FEMTY=.TRUE.
      ELSE
C       .............................. calculate user area for zooming
        CALL DRZ_CONE_TO_HV(HRB,VRB)
        IF(PARADA(2,J_PBZ).EQ.2.) THEN
C         .................................... calculate scales
          SCA(1)=HRB(1)
          SCA(2)=HRB(3)
          SCA(3)=VRB(1)
          SCA(4)=VRB(3)
        ELSE
C         .................................... calculate scales
          SCA(3)=PARADA(2,J_PFR)
          SCA(4)=PARADA(2,J_PTO)
          DSCA  =0.5*ABS(SCA(4)-SCA(3))/PARADA(2,J_PAS)
          SCA(1)=-DSCA
          SCA(2)= DSCA
        END IF

      END IF
      CALL DQRUPR(HRB,VRB)
      RPOSDT=RTO
      FZTRDT=.TRUE.
      CALL DHTINP
      CALL DCTYP0(1)
      IF(IHTRDO(1).EQ.0) THEN
        CALL DRZET1(SP)
        GO TO 999
      END IF
      IF(PARADA(4,J_PTE).GT.0.) THEN
         T3='SRZ'
      ELSE
         T3='PRZ'
      END IF
      CALL DODBPI(T3)
      CALL DODVDT(T3)
      CALL DODSIC(T3)
      CALL DODLCA(T3)
      CALL DODITC(T3)
      CALL DODTPC(T3)
      CALL DODMAG(T3)
      CALL DODECA(T3,'bar+ec')
      CALL DODHCA(T3,'bar+ec')
      CALL DODMUD(T3,'B+MA+E',0)
      CALL DBC_DETECTOR(0)
      CALL DQSC0('2 SC')

      IF(FPSWDU) CALL DGTEXT(1.,-100.,'End of detector',15)
      CALL DQMID
      CALL DAPVX('RZ',LDUM)
      CALL DAP_KNV('RZ',LDUM)
      CALL DAPEF('RZ')
      CALL DAPEO('RZ')
C     ........................ draw DTX tracks for animation
      CALL DTX_DRAW_LINES

      CALL DRZTPC(SP)

      IF(IHTRDO(1).LE.2.AND.PARADA(4,J_PIT).GT.0.) CALL DRZPI(SP)
      CALL DAPPV('RZ',0)
      CALL DAPVC('RZ',0)
      CALL DRZPL
      IF(PARADA(4,J_PHM).EQ.1.) CALL DAPPE('RZ')

      IF(PARADA(4,J_PDH).EQ.1.) THEN
        CALL DRZHST(FSR)
      ELSE
        CALL DRZPH(SP)
      END IF
      CALL=DVMDCU(0.)
      CALL DAPPT('RZ',DVMD,MHITDB,0)
      CALL DAPTRF('RZ')
      CALL DAPETR('RZ')
      IF(FPIKDP) GO TO 999
      CALL DCTYEX('RZ',2)
      IF(FEMTY) THEN
        IF(PARADA(4,J_PDS).LE.0.) THEN
          CALL DQSCA('H',HRB(1),HRB(2),'cm',2,TH,5)
          CALL DQSCA('V',VRB(2),VRB(3),'cm',2,'&r   ',5)
        END IF
      ELSE
        CALL DQSCA('H',SCA(1),SCA(2),'cm',2,' ',0)
        CALL DQSCA('V',SCA(3),SCA(4),'cm',2,' ',0)
      END IF
      IF(PARADA(2,J_PTO).LT.1005.) THEN
         CALL DLSITX(MTPCDL)
      ELSE IF(PARADA(2,J_PTO).LT.1008.) THEN
         CALL DLSITX(MECADL)
      ELSE
         CALL DLSITX(MHCADL)
      END IF
   99 CALL DQFR(IAREDO)
      CALL DPCSAR
  999 FPSQDQ=.FALSE.
      DFWIDU(IZOMDO)=DFWI
      END

*DK DRZDIS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZDIS
CH
      SUBROUTINE DRZDIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
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
      LOGICAL FC,FDOBL,FD
C     ....................... FC = TRUE only at last step of iteration
      IF(FC) THEN
         FKTRDP=FM
         IF(V1.EQ.0..OR.V2.EQ.0..OR.VM.EQ.0..OR.
     &     (SIGN(1.,V1).NE.SIGN(1.,V2)).OR.
     &     (SIGN(1.,V1).NE.SIGN(1.,VM))) THEN
           D=0.
           FC=.TRUE.
           RETURN
         END IF
         VV=V1*VM
         IF(VV.GE.0.) CALL DQL2EP(H1,V1,HM,VM)
         VV=V2*VM
         IF(VV.GE.0.) THEN
            CALL DQL2EP(HM,VM,H2,V2)
         END IF
         RETURN
      END IF
      FM=0.5*(F1+F2)
      HM=DHELIX(FM,IVZZDV)
      IF(FDOBL) THEN
         FI=DFINXT(FIMID,DHELIX(FM,IVFIDV))
         IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
            VM=DHELIX(FM,IVRODV)
         ELSE
            VM=-DHELIX(FM,IVRODV)
         END IF
      ELSE
         VM=DHELIX(FM,IVRODV)
      END IF
      IF(V1.EQ.0..OR.V2.EQ.0..OR.
     &  (SIGN(1.,V1).NE.SIGN(1.,V2))) THEN
         D=0.
         FC=.TRUE.
         RETURN
      END IF
      IF(FPRSDQ) THEN
C       .......................................
        CALL DQPR0(H1,V1)
        HD1=AHSCDQ*H1*PRSHDQ+BHSCDQ*V1*PRSVDQ
        VD1=AVSCDQ*H1*PRSHDQ+BVSCDQ*V1*PRSVDQ
C       ....................................... 
        CALL DQPR0(H2,V2)
        HD2=AHSCDQ*H2*PRSHDQ+BHSCDQ*V2*PRSVDQ
        VD2=AVSCDQ*H2*PRSHDQ+BVSCDQ*V2*PRSVDQ
C       .......................................
        CALL DQPR0(HM,VM)
        HDM=AHSCDQ*HM*PRSHDQ+BHSCDQ*VM*PRSVDQ
        VDM=AVSCDQ*HM*PRSHDQ+BVSCDQ*VM*PRSVDQ
C       .......................................
        DH21=HD2-HD1
        DV21=VD2-VD1
        DHM1=HDM-HD1
        DVM1=VDM-VD1
      ELSE
        DHU=H2-H1
        DVU=V2-V1
        DH21=AHSCDQ*DHU+BHSCDQ*DVU
        DV21=AVSCDQ*DHU+BVSCDQ*DVU
        DHU=HM-H1
        DVU=VM-V1
        DHM1=AHSCDQ*DHU+BHSCDQ*DVU
        DVM1=AVSCDQ*DHU+BVSCDQ*DVU
      END IF
      Q=DV21**2+DH21**2
      IF(Q.LE.0.000001) THEN
         FKTRDP=FM
         FC=.FALSE.
         RETURN
      END IF
      DM=(DV21*DHM1-DH21*DVM1)**2/Q
      IF(D.LT.DMAXDQ.AND.DM.LT.DMAXDQ) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,H2,V2)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRZDS0
CH
      ENTRY DRZDS0(FD,FIMD,FMI,FMA,S)
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
      SP=S
      FDOBL=FD
      FIMID=FIMD
      FMIN=FMI
      FMAX=FMA
      END
*DK DRZET1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZET1
CH
      SUBROUTINE DRZET1(SP)
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
      MODEDT=1
      CALL DCIRCL
      CALL DGLEVL(MCOLDL(MTPCDL))
      CALL DVXT(0,XYZVDT)
      CALL DRZ1TR
      END
*DK DRZETA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZETA
CH
      SUBROUTINE DRZETA(SP)
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
      EQUIVALENCE (KPIKDP,N)
      DATA W2/2./
      LOGICAL FOUT,DVM0
      LOGICAL DCCVX
      EXTERNAL DVTR0
C     CALL DLHITR('TR',MTPCDL,ICOL)
      CALL DAPTRN('RZ',0)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IHTRDO(2).LE.2) THEN
         CALL DSCTR
         CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
         IF(FOUT) RETURN
         MODEDT=1
         IF(.NOT.FPIKDP.AND.PDCODD(4,NFTRDD).GT.0.) THEN
           CALL DQLEVL(NFTRDD)
           DLINDD=DLINDD+W2
           DO 301 N=NUM1,NUM2
             IF(FNOTDT(N)) GO TO 301
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             CALL DRZ1TR
  301      CONTINUE
           DLINDD=DLINDD-W2
         END IF
         DO 1 N=NUM1,NUM2
           IF(FNOTDT(N)) GO TO 1
           IF(FVTRDC) CALL DGLEVL(NCTRDC(N))
           CALL DVTRV(N)
           CALL DVXT(N,XYZVDT)
           CALL DRZ1TR
           CALL DAPTRN('RZ',N)
    1    CONTINUE
      ELSE
         CALL DQLEVL(LCNCDD)
         MODEDT=2
         FIMID=PARADA(2,J_PFI)
         FMIN=FIMID-90.
         FMAX=FIMID+90.
         CALL DMCNVX(NVTX1)
         IF(NVTX1.LE.0) RETURN
         DO 12 NV=1,NVTX1
            CALL DMCVX(NV,KT1,KT2)
            IF(KT2.LE.0) GO TO 12
            DO 11 N=KT1,KT2
               IF(DVM0(N)) GO TO 11
               CALL DMCTR(N,FOUT)
               IF(FOUT) GO TO 11
               CALL DRZ1TR
   11       CONTINUE
   12    CONTINUE
         IF(.NOT.FPIKDP) THEN
            CALL DPAR_SET_CO(49,'CMC')
            IF(ICOLDP.LT.0) RETURN
            CALL DPARGV(67,'SSY',2,SYMB)
            CALL DPARGV(67,'SSZ',2,SYSZ)
            MSYMB=SYMB
            CALL DQPD0(MSYMB,SZVXDT*BOXSDU*SYSZ,0.)
C\\         DS=SZVXDT*BOXSDU*WDSNDL(2,4,MTPCDL)
            CALL DQPD0(3,DS,0.)
C           CALL DGLEVL(NCVXDT)
            DO 13 N=1,NVTX1
               IF(FMVXDT(N)) THEN
                  CALL DVMCVX(N,KDUM1,KDUM2,KDUM3,VTX1DT)
                  IF(DCCVX(VTX1DT)) GO TO 13
                  FI=DFINXT(FIMID,DVCORD(VTX1DT,IVFIDV))
                  H=DVCORD(VTX1DT,IVZZDV)
                  IF(PARADA(4,J_PTE).GT.0.) THEN
                     IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
                        V=DVCORD(VTX1DT,IVRODV)
                     ELSE
                        V=-DVCORD(VTX1DT,IVRODV)
                     END IF
                  ELSE
                     V=DVCORD(VTX1DT,IVRODV)
                  END IF
C                  IF(SP.NE.0.) THEN
C                     Q=(1.+SP*(FI-FIMID))
C                     H=H*Q
C                     V=V*Q
C                  END IF
                  CALL DQPD(H,V)
C                  CALL DLSYMC(MTPCDL,3,'R','N','S',H,V,DS,DS)
               END IF
   13       CONTINUE
         END IF
      END IF
      END
*DK DRZFI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZFI
CH
      SUBROUTINE DRZFI(AFI)
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
      LOGICAL FOUT
      PARAMETER (MAXT=111)
      DIMENSION SIG(2),NSIDE(2,MAXT)
      DATA DFSET/40./,DDF/1./,SIG/1.,-1./
      CHARACTER *3 DT3
      CHARACTER *27 T
C             123456789 123456789 1234567
      DATA T/'FI changed from 123 to 123.'/
      CALL DV0(TPCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      NTRK=BNUMDB(2,FRFTDB)
      NTRK=MIN(MAXT,NTRK)
      IF(NTRK.EQ.0) RETURN
      IF(CHTFDU.LE.1.) THEN
        DF1=DFSET
      ELSE
        DF1=CHTFDU
      END IF
      CALL=DVTPST(IVNTDV,IVNT)
      CALL=DVTPST(IVFIDV,IVFI)
      JTR=9999
      JHI=9999
      DO DF=0.,DF1,DDF
        DO IS=1,2
          FMID=FIMXDE+SIG(IS)*DF
          FMIN=FMID-90.
          FMAX=FMID+90.
          CALL VZERO(NSIDE,102)
          DO K=NUM1,NUM2
            NTR=NTPCDV(IVNT,K)
            IF(NTR.GT.0.AND.NTR.LE.MAXT) THEN
              FI=DFINXT(FMID,VTPCDV(IVFIDV,K))
              IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
                NSIDE(1,NTR)=NSIDE(1,NTR)+1
              ELSE
                NSIDE(2,NTR)=NSIDE(2,NTR)+1
              END IF
            END IF
          END DO
          MTR=0
          MHI=0
          DO N=1,NTRK
            IF(NSIDE(1,N).NE.0.AND.NSIDE(2,N).NE.0.) THEN
              MTR=MTR+1
              MHI=MHI+MIN(NSIDE(1,N),NSIDE(2,N))
            END IF
          END DO
          IF(MTR.EQ.0.AND.MHI.EQ.0) GO TO 9
          IF(MTR.LT.JTR) THEN
            FIRZ=FMID
            JTR=MTR
            JHI=MHI
          ELSE IF(MTR.EQ.JTR) THEN
            IF(MHI.LT.JHI) THEN
              FIRZ=FMID
              JTR=MTR
              JHI=MHI
            END IF
          END IF
        END DO
      END DO
    9 T(17:19)=DT3(AFI)
      T(24:26)=DT3(FIRZ)
      CALL DWRT(T)
      AFI=FIRZ
      END
*DK DRZPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPH
CH
      SUBROUTINE DRZPH(SP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG  9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_EX.INC'
      DIMENSION HL(2),VL(2)
      EQUIVALENCE (KPIKDP,K)
      DATA SHRNK/0.8/
      LOGICAL FOUT
      IF(.NOT.FHCADR) RETURN
      CALL DSCHC
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C      CALL DORCAL
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      QPI=SHRNK/PIFCDK
      DO 1 K=NUM1,NUM2
         IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
         CALL DCUTHC(K,FOUT)
         IF(FOUT) GO TO 1
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FI=DFINXT(FIMID,DVHC(IVFIDV,K))
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
C         CALL DLCOLH(DVHC,MHCADL,K,NCOL)
C         IF(NCOL.EQ.0) GO TO 1
         IF(FVHCDC) CALL DGLEVL(NCHCDC(K))
         H=DVHC(IVZZDV,K)
         IF(PARADA(4,J_PTE).NE.0.) THEN
            IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
               V=DVHC(IVRODV,K)
            ELSE
               V=-DVHC(IVRODV,K)
            END IF
         ELSE
            V=DVHC(IVRODV,K)
         END IF
C         IF(SP.NE.0.) THEN
C            Q=(1.+SP*(FI-FIMID))
C            H=H*Q
C            V=V*Q
C         END IF
         IF(FPIKDP) THEN
            CALL DQPIK(H,V)
         ELSE
            R=SQRT(DVHC(IVRODV,K)**2+DVHC(IVZZDV,K)**2)
            D=R*DVHC(IVDTDV,K)*QPI
            NN=DVHC(IVNNDV,K)
            IF(NN.EQ.2) THEN
               D=D/SIND(TE)
               HL(1)=H-D
               HL(2)=H+D
               VL(1)=V
               VL(2)=V
            ELSE
               D=D/COSD(TE)
               HL(1)=H
               HL(2)=H
               VL(1)=V-D
               VL(2)=V+D
            END IF
            CALL DQLIE(HL,VL)
         END IF
    1 CONTINUE
      END
*DK DRZPL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPL
CH
      SUBROUTINE DRZPL
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
      EXTERNAL DVLC
      EQUIVALENCE (KPIKDP,K)
      LOGICAL FOUT
C      CALL DSCEL(PLSDDB,LCLKDD, FVLCDC,FDLCDC, MODE,LCOL,NCDEM)
      CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      FD1=FC1+180.
      FD2=FC2+180.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
C//   CALL DQPD0(MSYMDL(MECADL,0),WDSNDL(2,4,MECADL),0.)
      CALL DPAR_SET_SY(63,0.)
      CALL DPAR_SET_CO(56,'CCN')
C     .................. COL.FUNCTION SET CONSTANT      
      MODE=0
      LCOL=1
      NCOL=8
      DO 2 L=1,LCOL
        IF(MODE.EQ.1) THEN
C         ......................... CORRECT WHEN COLOR FUNCTION USED
C          NCOL=PDCODD(2,NCOLDD(L))
C          CALL DGLEVL(NCOL)
        END IF
        DO 1 K=NUM1,NUM2
          IF(MODE)11,12,13
   11     CALL DGLEVL(NCLCDC(K))
          GO TO 12
   13     IF(NCLCDC(K).NE.NCOL) GO TO 1
   12     CALL DCUTLC(K,FOUT)
          IF(FOUT) GO TO 1
          TE=DVLC(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
          FI=DFINXT(FIMID,DVLC(IVFIDV,K))
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
          IF(PARADA(4,J_PTE).NE.0.) THEN
            IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
              V=DVLC(IVRODV,K)
            ELSE
              V=-DVLC(IVRODV,K)
            END IF
          ELSE
            IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
            V=DVLC(IVRODV,K)
          END IF
          H=DVLC(IVZZDV,K)
          IF(FPIKDP) THEN
            CALL DQPIK(H,V)
          ELSE
            CALL DQPD(H,V)
          END IF
    1   CONTINUE
    2 CONTINUE
      END
*DK DRZPI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPI
CH
      SUBROUTINE DRZPI(SP)
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
      DATA N8/8/
      EQUIVALENCE (KPIKDP,K)
      LOGICAL FOUT
      IF(.NOT.FITCDR) RETURN
C     ............................ ONLY RESIDUALS ARE DRAWN BUT NOT OTHER HITS
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
      TPARDA=
     &  'J_STC'
      CALL DPARAM(31
     &  ,J_STC)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FGRYDR.AND.PARADA(4,J_STC).LT.0.) RETURN
      CALL DSCIT(IVNT)
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     CALL DVITST(IVNTDV,IVNT,IDUM)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
C//   MSYM=MSYMDL(MITCDL,0)
      CALL DPARGV(61,'SSY',2,SYMB)
      CALL DPARGV(61,'SSZ',2,SYSZ)
      MSYM=SYMB
      IF(MSYM.LE.2) MSYM=N8
C//   CALL DQPD0(MSYM,WDSNDL(2,4,MITCDL),0.)
      CALL DQPD0(MSYM,SYSZ,0.)
      DO 1 K=NUM1,NUM2
         TE=DVIT(IVTEDV,K,1)
C        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FI=DFINXT(FIMID,DVIT(IVFIDV,K,1))
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         IF(FVITDC) CALL DGLEVL(NCITDC(K))
         H=DVIT(IVZZDV,K,1)
         IF(PARADA(4,J_PTE).NE.0.) THEN
            IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
               V=DVIT(IVRODV,K,1)
            ELSE
               V=-DVIT(IVRODV,K,1)
            END IF
         ELSE
            V=DVIT(IVRODV,K,1)
         END IF
C         IF(SP.NE.0.) THEN
C            Q=(1.+SP*(FI-FIMID))
C            H=H*Q
C            V=V*Q
C         END IF
         IF(FPIKDP) THEN
            CALL DQPIK(H,V)
            GO TO 1
         END IF
         CALL DQPD(H,V)
    1 CONTINUE
      END
*DK DRZPIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPIT
CH
      SUBROUTINE DRZPIT(F1,F2,FDOBL,FMIN,FMAX)
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
      LOGICAL FDOBL
C      DATA FP/20./,KP/20/
C      DF=(F2-F1)/FP
C      F=F1
C      DO 700 K=1,KP
C        F=F+DF
C        H1=DHELIX(F,IVZZDV)
C        IF(FDOBL) THEN
C           FI=DFINXT(FIMID,DHELIX(F,IVFIDV))
C           IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
C              V1=DHELIX(F,IVRODV)
C           ELSE
C              V1=-DHELIX(F,IVRODV)
C           END IF
C        ELSE
C           V1=DHELIX(F,IVRODV)
C        END IF
C        CALL DQPIK(H1,V1)
C  700 CONTINUE
      DF=F2-F1
      DO 700 K=1,MTRSDP
        F=F1+TRSTDP(K)*DF
        H1=DHELIX(F,IVZZDV)
        IF(FDOBL) THEN
           FI=DFINXT(FIMID,DHELIX(F,IVFIDV))
           IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
              V1=DHELIX(F,IVRODV)
           ELSE
              V1=-DHELIX(F,IVRODV)
           END IF
        ELSE
           V1=DHELIX(F,IVRODV)
        END IF
        CALL DQPIK(H1,V1)
  700 CONTINUE
      END
C*DK DRZRCB
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRCB
CCH
C      SUBROUTINE DRZRCB(HRB,VRB,TH)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
CC     Rubberband: from Cone to Box
C*CA DALLCO
C      INCLUDE 'DALI_CF.INC'
C      CHARACTER *5 TH
C      DIMENSION HRB(4),VRB(4)
C      DATA V0/0./
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TPARDA=
C     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM'
C      CALL DPARAM(11
C     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TE=PARADA(2,J_PTE)
C      IF(PARADA(4,J_PFI).EQ.-1.) TE=360.-TE
C      ST=SIND(TE)
C      CT=COSD(TE)
C      R1=PARADA(2,J_PFR)
C      R2=PARADA(2,J_PTO)
C      RM=0.5*(R1+R2)
C      IF(PARADA(2,J_PBM).LT.5.) THEN
C         D2=0.5*(R2-R1)
C         TM=MOD(TE+3600.,360.)
C         IF(TM.LT.40..OR.TM.GT.320.) THEN
C            HM=RM
C            VM=HM*ST/CT
C         ELSE IF(TM.LE.140.) THEN
C            VM=RM
C            HM=VM*CT/ST
C         ELSE IF(TM.LT.220.) THEN
C            HM=-RM
C            VM=HM*ST/CT
C         ELSE
C            VM=-RM
C            HM=VM*CT/ST
C         END IF
C         IF(TH.NE.'*****') THEN
C           CALL DQRHV(SH,SV)
C           DH=D2*SH
C           DV=D2*SV
C           TH='Z    '
C         ELSE
C           DH=D2
C           DV=D2
C         END IF
C         HRB(1)=HM-DH
C         HRB(2)=HM+DH
C         HRB(3)=HRB(2)
C         HRB(4)=HRB(1)
C         IF(PARADA(4,J_PTE).EQ.0.) THEN
C           VRB(1)=MAX(V0,VM-DV)
C           VRB(2)=VRB(1)+2.*DV
C         ELSE
C           VRB(1)=VM-DV
C           VRB(3)=VM+DV
C         END IF
C         VRB(2)=VRB(1)
C         VRB(4)=VRB(3)
C      ELSE
C         TM=MOD(TE+3600.,360.)
C         IF(TM.LT.40..OR.TM.GT.320.) THEN
C            H12=PARADA(2,J_PFR)
C            H34=PARADA(2,J_PTO)
C            V12=H12*ST/CT
C            V34=H34*ST/CT
C         ELSE IF(TM.LE.140.) THEN
C            V12=PARADA(2,J_PFR)
C            V34=PARADA(2,J_PTO)
C            H12=V12*CT/ST
C            H34=V34*CT/ST
C         ELSE IF(TM.LT.220.) THEN
C            H12=-PARADA(2,J_PFR)
C            H34=-PARADA(2,J_PTO)
C            V12=H12*ST/CT
C            V34=H34*ST/CT
C         ELSE
C            V12=-PARADA(2,J_PFR)
C            V34=-PARADA(2,J_PTO)
C            H12=V12*CT/ST
C            H34=V34*CT/ST
C         END IF
C         R=SQRT((H34-H12)**2+(V34-V12)**2)
C         Q=1./MAX(ABS(ST),0.018)
C         IF(PARADA(2,J_PAS).EQ.0.) THEN
C            D2=0.5*R*Q
C         ELSE
C            D2=0.5*R*Q/PARADA(2,J_PAS)
C         END IF
C         HRB(1)=H12-D2
C         HRB(2)=H12+D2
C         HRB(3)=H34+D2
C         HRB(4)=H34-D2
C         VRB(1)=V12
C         VRB(2)=VRB(1)
C         VRB(3)=V34
C         VRB(4)=VRB(3)
C         IF(TH.NE.'*****') TH='Z+c&r'
C      END IF
C      END
*DK DRZTI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZTI
CH
      SUBROUTINE DRZTI(SP)
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
      DIMENSION H(2),V(2)
      EQUIVALENCE (K,KPIKDP)
      DATA SHRINK/0./
      LOGICAL FOUT
      IF(.NOT.FITCDR) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      QPI=SHRINK/PIFCDK
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHI(M,NUM)
         IF(NUM.LE.0) GO TO 3
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
C        There is no l/r ambiguity in this view, but K must be positive
C        since it is used as a pointer in DVIT.
         DO 1 KIND=1,NUM
            K=IDVCHI(KIND)
            IF(K.GT.0) THEN
               NFI=1
            ELSE
               NFI=2
               K=-K
            END IF
            TE=DVIT(IVTEDV,K,NFI)
C           IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
C              I=0
C              GO TO 1
C           END IF
            FI=DFINXT(FIMID,DVIT(IVFIDV,K,NFI))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            H(I)=DVIT(IVZZDV,K,1)
            IF(PARADA(4,J_PTE).NE.0.) THEN
               IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
                  V(I)=DVIT(IVRODV,K,NFI)
               ELSE
                  V(I)=-DVIT(IVRODV,K,NFI)
               END IF
            ELSE
               V(I)=DVIT(IVRODV,K,1)
            END IF
C           IF(SP.NE.0.) THEN
C             Q=(1.+SP*(FI-FIMID))
C             H(I)=H(I)*Q
C             V(I)=V(I)*Q
C           END IF
            IF(FPIKDP) THEN
               CALL DQPIK(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               IF(SIGN(1.,V(1)).EQ.SIGN(1.,V(2))) CALL DQLIE(H,V)
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DRZTPC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZTPC
CH
      SUBROUTINE DRZTPC(SP)
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
      INCLUDE 'DALI_UIS.INC'
      INCLUDE 'DALI_EX.INC'
      IF(IHTRDO(1).EQ.0) THEN
         CALL DRZET1(SP)
         CALL DQFR(IAREDO)
         RETURN
      END IF
      IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
        IF(IHTRDO(2).EQ.1) THEN
          IF(FTPCDR) CALL DRZTT(SP)
          CALL DPARGV(11,'PIT',4,PIT4)
          IF(PIT4.GT.0.) THEN
            IF(FITCDR) CALL DRZTI(SP)
            IF(FTPCDR.AND.FITCDR) CALL DRZCC(SP)
          END IF
C       ELSE IF(IHTRDO(2).GE.4) THEN
C         CALL D_AP_KALMAN_TR
        ELSE
C          CALL DRZETA(SP)
          CALL DAP_TR
        END IF
      END IF
      IF(.NOT.FTPCDR) RETURN
      IF(FPSWDU) CALL DGTEXT(1.,-100.,'End of tracks',13)
      IF(IHTRDO(1).LE.2) THEN
        IF(IHTRDO(6).LE.1) THEN
          CALL DAPPT('RZ',DVTP,TPCODB,0)
        ELSE IF(IHTRDO(6).EQ.2) THEN
          CALL DAPPT('RZ',DVPADA,TPADDB,0)
        ELSE IF(IHTRDO(6).EQ.3) THEN
          CALL DAPPT('RZ',DVTP,TBCODB,0)
        END IF
      END IF
      END
*DK DRZTT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZTT
CH
      SUBROUTINE DRZTT(SP)
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
      DIMENSION H(2),V(2)
      EQUIVALENCE (K,KPIKDP)
      DATA SHRINK/0./
      LOGICAL FOUT
      IF(.NOT.FTPCDR) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM0,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      QPI=SHRINK/PIFCDK
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHT(M,NUM)
         IF(NUM.LE.0) GO TO 3
         K1=IDVCHT(1)
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
    2    DO 1 KIND=1,NUM
            K=IDVCHT(KIND)
            IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
            TE=DVTP(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
               I=0
               GO TO 1
            END IF
            FI=DFINXT(FIMID,DVTP(IVFIDV,K))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            IF(PARADA(4,J_PTE).NE.0.) THEN
               FI=DFINXT(FIMID,DVTP(IVFIDV,K))
               IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
                  V(I)=DVTP(IVRODV,K)
               ELSE
                  V(I)=-DVTP(IVRODV,K)
               END IF
            ELSE
               V(I)=DVTP(IVRODV,K)
            END IF
            H(I)=DVTP(IVZZDV,K)
C            IF(SP.NE.0.) THEN
C               Q=(1.+SP*(FI-FIMID))
C               H(I)=H(I)*Q
C               V(I)=V(I)*Q
C            END IF
            IF(FPIKDP) THEN
               CALL DQPIK(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               IF(SIGN(1.,V(1)).EQ.SIGN(1.,V(2))) CALL DQLIE(H,V)
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DRZVRT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZVRT
CH
      SUBROUTINE DRZVRT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: DRAW VERTEX
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      V=SQRT(VRTXDV*VRTXDV+VRTYDV*VRTYDV)
      CALL DQVRT(VRTZDV,V)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE'
      CALL DPARAM(11
     &  ,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PTE).NE.0.) CALL DQVRT(VRTZDV,-V)
      END
*DK DRZHST
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZHST
CH
      SUBROUTINE DRZHST(FSR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  Draw rz histograms for ECAL and HCAL
C    Inputs    :FSR=TRUE if signed rho
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *3 DT3,T3
      LOGICAL FSR
      SE=0.
      SH=0.
      IF(PDCODD(2,ISTYDD).LT.2.) THEN
        CALL DRZHH(FSR,'LINE',SH)
        CALL DRZHE(FSR,'LINE',SE)
      ELSE
        CALL DRZHH(FSR,'AREA',SH)
        PES=PDCODD(2,LCESDD)
        IF(SQRT(AHSCDQ*AHSCDQ+BHSCDQ*BHSCDQ).LE.PES.AND.
     &     SQRT(AVSCDQ*AVSCDQ+BVSCDQ*BVSCDQ).LE.PES)  THEN
          CALL DRZHE(FSR,'AREA',SE)
        ELSE
          CALL DRZHE(FSR,'AR+L',SE)
        END IF
        CALL DRZHH(FSR,'LINE',SH)
      END IF
      IF(FPIKDP) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PSE,J_PSH'
      CALL DPARAM(12
     &  ,J_PSE,J_PSH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(SE.GT.0.) THEN
        T3=DT3(PARADA(2,J_PSE))
        CALL DQSCE(T3//' Gev EC',10,SE*PARADA(2,J_PSE))
        SCALDS(IAREDO,MSECDS)=SE
      END IF
      IF(SH.GT.0.) THEN
        T3=DT3(PARADA(2,J_PSH))
        CALL DQSCE(T3//' Gev HC',10,SH*PARADA(2,J_PSH))
        SCALDS(IAREDO,MSHCDS)=SH
      END IF
      END
*DK DRZHE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZHE
CH
      SUBROUTINE DRZHE(FSR,TST,QE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C     DRAW ECAL HISTOGRAM IN RZ. The towers show the histogram and are
C     therefore drawn as parallelograms/
C    Inputs    : FSR=.TRUE. if signed rho, TST='LINE' or 'AREA' or 'AR+L'
C    OUTPUT    : QE = scale factor
C
C    Called by : DRZHST
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TST
      DIMENSION HIST(228,2),HD(5),VD(5)
      DIMENSION REC(2),ZEC(2),LL(2)
      DATA ZEC/250.,310./,REC/180.,243./,Q/1.1/,DL/1./,QESET/.54/
     &  ,LF/1/,D60/60./
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *49 T
      LOGICAL FOUT,FIN,FSR
      EQUIVALENCE (J,KPIKDP)
C     ................... SETUP
      IF(FPIMDP) RETURN
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      DLIN=DLINDD
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PSE,J_PHM'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PSE,J_PHM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPAR_SET_WIDTH(63,'SHL',0,' ')
      IF(PARADA(4,J_PHM).EQ.1.) THEN
        LAYER=2
      ELSE
        LAYER=1
      END IF
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      IF(FSR) THEN
        FMIN=FIMID-90.
        FMAX=FIMID+90.
        N2=2
      ELSE
        FMIN=-999.
        FMAX= 999.
        N2=1
      END IF
      IF(.NOT.FPIKDP) THEN
C       ................... CLEAR HISTOGRAM ARRAY
        CALL VZERO(HIST,456)
C       ................... STORE HISTOGRAM
        DO 1 K=NUM1,NUM2
          CALL DCUTEC(K,FOUT)
          IF(FOUT) GO TO 1
          TE=DVEC(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
          FI=DFINXT(FIMID,DVEC(IVFIDV,K))
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
          JJ=DVEC(IVJJDV,K)
          EN=DVEC(IVENDV,K)
          IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
            HIST(JJ,1)=HIST(JJ,1)+EN
          ELSE
            HIST(JJ,2)=HIST(JJ,2)+EN
          END IF
    1   CONTINUE
C       .................. ADD ENDCAP CHANNELS AT CORNER INTO BARREL
        DO N=1,N2
C         ........................................ 46-50 -> 51-55
          DO J=46,50
            HIST(J+5,N)=HIST(J+5,N)+HIST(J,N)
            HIST(J,N)=0.
          END DO
C         ...................................... 179-183 -> 174-178
          DO J=179,183
            HIST(J-5,N)=HIST(J-5,N)+HIST(J,N)
            HIST(J,N)=0.
          END DO
        END DO
C       ................... CALCULATE SCALE
        IF(PARADA(4,J_PSE).EQ.-1.) THEN
          EMAX=0.
          DO N=1,N2
            DO J=1,228
              IF(HIST(J,N).GT.0.) EMAX=MAX(EMAX,HIST(J,N))
            END DO
          END DO
          EMAX=EMAX/SHECDU
          IF(EMAX.GT.0.) PARADA(2,J_PSE)=EMAX
        END IF
        CALL DQPOC(ZEC(1)    ,REC(1)    ,HD1,VD1,FIN)
        CALL DQPOC(ZEC(1)+D60,REC(1)+D60,HD2,VD2,FIN)
        RD=SQRT((HD2-HD1)**2+(VD2-VD1)**2)
        QE=QESET*RD/PARADA(2,J_PSE)
C       ................... CALCULATE TOWER AND DRAW IN DISPLAY COORDINATES
        LL(1)=PDCODD(2,LCECDD)
        LL(2)=PDCODD(2,LCELDD)
        CALL DQPO0(TST,LL(1),LL(2),' ')
        DLINDD=DL
C       ...........  1, 50 = RIGHT ENDCAP   50-  1=49   : 50 CHANNELS
C       ........... 51,114 = RIGHT BARREL  114- 51=63   : 64 CHANNELS
C       ...........115,178 = LEFT  BARREL  178-115=63   : 64 CHANNELS
C       ...........179,228 = LEFT  ENDCAP  228-179=49   : 50 CHANNELS
      END IF
      RU=REC(LAYER)
      SR=1.
      DO N=1,N2
        IF(FPIKDP) MDLRDP=2002+N
        ZU=ZEC(LAYER)
        J1=1
        J2=50
        DO I=1,2
          RUU=-9999.
          RUD= 9999.
          DO J=J1,J2
            IF(HIST(J,N).GT.0.) THEN
C             ............................................ ENDCAPS
              CALL=DVEC2(J,3)
              TE=DVEC(IVTEDV,-1)
              DT=DVEC(IVDTDV,-1)
C             .............................. LOWER + UPPER BASE POINT
              RU1=SR*ZU*TAND(TE-DT)
              RU2=SR*ZU*TAND(TE+DT)
              RUU=MAX(RUU,RU1,RU2)
              RUD=MIN(RUD,RU1,RU2)
              RUM=0.5*(RU2+RU1)
              IF(FPIKDP) THEN
                CALL DQPIK(ZU,RUM)
              ELSE
                RUM=RUM*Q
                ZUM=ZU*Q
                CALL DQPOC(ZU,RU1,HD(1),VD(1),FIN)
                CALL DQPOC(ZU,RU2,HD(2),VD(2),FIN)
                CALL DQPOC(ZUM,RUM,HDM ,VDM  ,FIN)
                E=QE*HIST(J,N)
                CALL DRZPOL(HD,VD,HDM,VDM,E)
              END IF
            END IF
          END DO
          IF((.NOT.FPIKDP).AND.LF.GT.0.AND.RUD.NE.9999) THEN
            CALL DGLEVL(LL(LF))
            CALL DQL2E(ZU,RUU,ZU,RUD)
            CALL DGLEVL(LL(2))
          END IF
          ZU=-ZEC(LAYER)
          J1=179
          J2=228
        END DO
        ZUR=-9999.
        ZUL= 9999.
        DO J=51,178
          IF(HIST(J,N).GT.0.) THEN
C           ............................................ BARREL
            CALL=DVEC2(J,3)
            TE=DVEC(IVTEDV,-1)
            DT=DVEC(IVDTDV,-1)
C           .................................... RIGHT + LEFT BASE POINT
            ZU1=REC(LAYER)/TAND(TE+DT)
            ZU2=REC(LAYER)/TAND(TE-DT)
            ZUR=MAX(ZUR,ZU2)
            ZUL=MIN(ZUL,ZU1)
            ZUM=0.5*(ZU2+ZU1)
            IF(FPIKDP) THEN
              CALL DQPIK(ZUM,RU)
            ELSE
              ZUM=ZUM*Q
              RUM=RU*Q
              CALL DQPOC(ZU1,RU ,HD(1),VD(1),FIN)
              CALL DQPOC(ZU2,RU ,HD(2),VD(2),FIN)
              CALL DQPOC(ZUM,RUM,HDM  ,VDM  ,FIN)
              E=QE*HIST(J,N)
              CALL DRZPOL(HD,VD,HDM,VDM,E)
            END IF
          END IF
        END DO
        IF((.NOT.FPIKDP).AND.LF.GT.0.AND.ZUL.NE.9999) THEN
          CALL DGLEVL(LL(LF))
          CALL DQL2E(ZUL,RU,ZUR,RU)
          CALL DGLEVL(LL(2))
        END IF
        RU=-REC(LAYER)
        SR=-1.
      END DO
      CALL DPAR_SET_TR_WIDTH
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRZHEP
CH
      ENTRY DRZHEP(NTOWR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  CALLED BY PICKING:
C    Inputs    :$ OF TOWER
C    Outputs   :NO
C
C    Called by :
C ---------------------------------------------------------------------
C     MDLPDP=2003 RHO=+ , MDLPDP=2004 RHO=-
C        123456789 123456789 123456789 123456789 123456789
      T='RZ:EC:Energy of tower +123=12.4  Sum(12)=12.4 GeV'
      IF(MDLPDP.EQ.2003) THEN
        T(23:23)='+'
        NRP=1
      ELSE
        T(23:23)='-'
        NRP=2
      END IF
      T(24:26)=DT3(FLOAT(NTOWR))
      T(28:31)=DT4(HIST(NTOWR,NRP))
      STW=0.
      SUM=0.
      DO LP=NTOWR,228
        IF(HIST(LP,NRP).EQ.0.) GO TO 101
        SUM=SUM+HIST(LP,NRP)
        STW=STW+1.
      END DO
  101 DO LP=NTOWR-1,1,-1
        IF(HIST(LP,NRP).EQ.0.) GO TO 102
        SUM=SUM+HIST(LP,NRP)
        STW=STW+1.
      END DO
  102 T(38:39)=DT2(STW)
      T(42:45)=DT4(SUM)
      CALL DWRT(T)
      END
*DK DRZHH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZHH
CH
      SUBROUTINE DRZHH(FSR,TST,QE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C     DRAW HCAL HISTOGRAM IN RZ. The towers represent the pads and are
C     therefore drawn parrallel, i.e. as trapezoids.
C    Inputs    : FSR=.TRUE. if signed rho, TST='LINE' or 'AREA' or 'AR+L'
C    OUTPUT    : QE = scale factor
C
C    Called by : DRZHST
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TST
      DIMENSION HIST(63,2),HD(5),VD(5),ZU(4),RU(4)
      DIMENSION RUB(4,2),ZUE(4,2),LL(2)
      DATA Q/1.1/,D100/100./
      DATA QESET/.75/,RMIN/45./
      CHARACTER *2 DT2
      CHARACTER *4 DT4
      CHARACTER *49 T
      LOGICAL FOUT,FIN,FSR,FRST
      DATA FRST/.TRUE./
      DATA KDEB/0/,JDEB/3/
      EQUIVALENCE (J,KPIKDP)
      IF(KDEB.NE.0) RETURN
C     ................... SETUP
      IF(FPIMDP) RETURN
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PSH,J_PDH'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PSH,J_PDH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPAR_SET_WIDTH(64,'SHL',0,' ')
      S=1.
      RBA=RMNBDH        +(RMXBDH        -RMNBDH        )*PARADA(2,J_PDH)
      ZEC=CORADH(3,12,1)+(CORADH(3,11,1)-CORADH(3,12,1))*PARADA(2,J_PDH)
      DO N=1,2
        DO K=1,2
          ZUE(K  ,N)=ZEC*S
          ZUE(K+2,N)=ZEC*S*Q
          RUB(K  ,N)=RBA*S
          RUB(K+2,N)=RBA*S*Q
        END DO
        S=-1.
      END DO
      FRST=.FALSE.
      DLIN=DLINDD
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      IF(FSR) THEN
        FMIN=FIMID-90.
        FMAX=FIMID+90.
        N2=2
      ELSE
        FMIN=-999.
        FMAX= 999.
        N2=1
      END IF
C     ................... CLEAR HISTOGRAM ARRAY
      CALL VZERO(HIST,126)
C     ................... STORE HISTOGRAM
      DO 1 K=NUM1,NUM2
        CALL DCUTHC(K,FOUT)
        IF(FOUT) GO TO 1
        TE=DVHC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVHC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
        JJ=DVHC(IVJJDV,K)
        IF(JJ.GT.63.OR.JJ.LT.1) CALL DWRT('DRZHH 1#')
        EN=DVHC(IVENDV,K)
        IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
          HIST(JJ,1)=HIST(JJ,1)+EN
        ELSE
          HIST(JJ,2)=HIST(JJ,2)+EN
        END IF
    1 CONTINUE
C     ................... CALCULATE SCALE
      IF(PARADA(4,J_PSH).EQ.-1.) THEN
        EMAX=0.
        DO N=1,N2
          DO J=1,63
            IF(HIST(J,N).GT.0.) EMAX=MAX(EMAX,HIST(J,N))
          END DO
        END DO
        EMAX=EMAX/SHHCDU
        IF(EMAX.GT.0.) PARADA(2,J_PSH)=EMAX
      END IF
      CALL DQPOC(ZEC     ,RBA     ,HD1,VD1,FIN)
      CALL DQPOC(ZEC+D100,RBA+D100,HD2,VD2,FIN)
      RD=SQRT((HD2-HD1)**2+(VD2-VD1)**2)
      QE=QESET*RD/PARADA(2,J_PSH)
C     ................... CALCULATE TOWER AND DRAW IN DISPLAY COORDINATES
      LL(1)=PDCODD(2,LCHCDD)
      LL(2)=PDCODD(2,LCHLDD)
      CALL DQPO0(TST,LL(1),LL(2),' ')
      DLINDD=PDCODD(2,LIGLDD)
C     ...........  1, 14 = RIGHT ENDCAP   50-  1=49   : 50 CHANNELS
C     ........... 15, 49 =       BARREL  114- 51=63   : 64 CHANNELS
C     ........... 50, 63 = LEFT  ENDCAP  228-179=49   : 50 CHANNELS
      SR=1.
C     ..................................................... +RHO,-RHO LOOP
      DO N=1,N2
        IF(FPIKDP) MDLRDP=2004+N
        J1=1
        J2=14
C       ............................................ RIGHT, LEFT ENDCAP LOOP
        DO I=1,2
          DO J=J1,J2
            IF(HIST(J,N).GT.0.) THEN
              CALL HUSRAN(J,1,1,TE,FI,DT,DF)
              DT=0.5*DT
C             .............................. LOWER + UPPER BASE POINT
              RU(1)=SR*ZUE(1,I)*TAN(TE-DT)
              RU(2)=SR*ZUE(1,I)*TAN(TE+DT)
              IF(FPIKDP) THEN
                CALL DQPIK(ZUE(1,I),0.5*(RU(2)+RU(1)))
              ELSE
                RU(3)=RU(2)*Q
                RU(4)=RU(1)*Q
                DO M=1,4
                  IF(N.EQ.1) THEN
                    RU(M)=MAX( RMIN,RU(M))
                  ELSE
                    RU(M)=MIN(-RMIN,RU(M))
                  END IF
                  CALL DQPOC(ZUE(M,I),RU(M),HD(M),VD(M),FIN)
                END DO
                E=QE*HIST(J,N)
CDEB            IF(J.EQ.JDEB) CALL US3_0(HD,VD,E)
                CALL DRZTRP(HD,VD,E)
              END IF
            END IF
          END DO
          J1=50
          J2=63
        END DO
        DO J=15,49
          IF(HIST(J,N).GT.0.) THEN
C           ............................................ BARREL
            CALL HUSRAN(J,1,1,TE,FI,DT,DF)
            DT=0.5*DT
C           .................................... RIGHT + LEFT BASE POINT
            ZU(1)=RUB(1,1)/TAN(TE+DT)
            ZU(2)=RUB(1,1)/TAN(TE-DT)
            IF(FPIKDP) THEN
              CALL DQPIK(0.5*(ZU(2)+ZU(1)),RUB(1,N))
            ELSE
              ZU(3)=ZU(2)*Q
              ZU(4)=ZU(1)*Q
              DO M=1,4
                CALL DQPOC(ZU(M),RUB(M,N),HD(M),VD(M),FIN)
              END DO
              E=QE*HIST(J,N)
CDEB          IF(J.EQ.JDEB) CALL US3_0(HD,VD,E)
              CALL DRZTRP(HD,VD,E)
            END IF
          END IF
        END DO
        SR=-1.
      END DO
      CALL DPAR_SET_TR_WIDTH
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRZHHP
CH
      ENTRY DRZHHP(NTOWR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  CALLED BY PICKING:
C    Inputs    :$ OF TOWER
C    Outputs   :NO
C
C    Called by :
C ---------------------------------------------------------------------
C     MDLPDP=2005 RHO=+ , MDLPDP=2006 RHO=-
C        123456789 123456789 123456789 123456789 123456789
      T='RZ:HC:Energy of tower  +12=12.4  Sum(12)=12.4 GeV'
      IF(MDLPDP.EQ.2005) THEN
        T(24:24)='+'
        NRP=1
      ELSE
        T(24:24)='-'
        NRP=2
      END IF
      T(25:26)=DT2(FLOAT(NTOWR))
      T(28:31)=DT4(HIST(NTOWR,NRP))
      STW=0.
C     ..................... Sum all neighbours until a tower is 0.
      SUM=0.
      DO LP=NTOWR,63
        IF(HIST(LP,NRP).EQ.0.) GO TO 101
        SUM=SUM+HIST(LP,NRP)
        STW=STW+1.
      END DO
  101 DO LP=NTOWR-1,1,-1
        IF(HIST(LP,NRP).EQ.0.) GO TO 102
        SUM=SUM+HIST(LP,NRP)
        STW=STW+1.
      END DO
  102 T(38:39)=DT2(STW)
      T(42:45)=DT4(SUM)
      CALL DWRT(T)
      END
*DK DRZTRP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZTRP
CH
      SUBROUTINE DRZTRP(HD,VD,E)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C     .................. Draw a trapez with length E between 12 and 34
C     .................. in display coordinates
C    Inputs    : HD, VD = trapez coordinates to be scaled by E
C
C ---------------------------------------------------------------------
      DIMENSION HD(5),VD(5)
      HD12=0.5*(HD(1)+HD(2))
      VD12=0.5*(VD(1)+VD(2))
      HD34=0.5*(HD(3)+HD(4))
      VD34=0.5*(VD(3)+VD(4))
      Q=E/SQRT((HD34-HD12)*(HD34-HD12)+(VD34-VD12)*(VD34-VD12))
      HD(3)=HD(2)+Q*(HD(3)-HD(2))
      HD(4)=HD(1)+Q*(HD(4)-HD(1))
      HD(5)=HD(1)
      VD(3)=VD(2)+Q*(VD(3)-VD(2))
      VD(4)=VD(1)+Q*(VD(4)-VD(1))
      VD(5)=VD(1)
      CALL DQPOL1(5,HD,VD)
      END
*DK DRZPOL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZPOL
CH
      SUBROUTINE DRZPOL(HD,VD,HD34,VD34,E)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C     .... Draw parallelogram with a length E between 12 and 34
c     .... in display coordinates. HD(3:5) and VD(3:5) are recalculated.
C    Inputs  : HD(1:2), VD(1:2), HD34, VD34  = coordinates to be scaled by E
C
C ---------------------------------------------------------------------
      DIMENSION HD(5),VD(5)
      DATA SH/1./,SV/1./
      HD12=0.5*(HD(2)+HD(1))
      VD12=0.5*(VD(2)+VD(1))
      Q=E/SQRT((HD34-HD12)*(HD34-HD12)+(VD34-VD12)*(VD34-VD12))
      HDM=HD12+Q*(HD34-HD12)
      VDM=VD12+Q*(VD34-VD12)
      DHD=0.5*(HD(2)-HD(1))
      HD(3)=HDM+SH*DHD
      HD(4)=HDM-SH*DHD
      HD(5)=HD(1)
      DVD=0.5*(VD(2)-VD(1))
      VD(3)=VDM+SV*DVD
      VD(4)=VDM-SV*DVD
      VD(5)=VD(1)
      CALL DQPOL1(5,HD,VD)
      END
*DK DRO_SMOOTH_ROT_AC_ONLY
CH..............+++               AR
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++ DRO_SMOOTH_ROT_AC_ONLY
CH
      SUBROUTINE DRO_SMOOTH_ROT_AC_ONLY
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C
      CHARACTER *4 TX
      DIMENSION HRB(4),VRB(4)
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,L7/7/,SFAC/.97/
      DIMENSION IP(2)
      DATA IP/8,4/,NDASH/2/
C
      DIMENSION XYZSQ(3,8)
C     ............................................ cube in system C1
      DATA XYZSQ/
     &   1., 1.,-1.,    ! 1
     &   1.,-1.,-1.,    ! 2
     &  -1.,-1.,-1.,    ! 3
     &  -1., 1.,-1.,    ! 4
     &   1., 1., 1.,    ! 5
     &   1.,-1., 1.,    ! 6
     &  -1.,-1., 1.,    ! 7
     &  -1., 1., 1./    ! 8
C
      DIMENSION HUWSQ(3,8),HSQ(8),VSQ(8),DSQ(8),NSQL(12,2)
      DIMENSION LSQ1(3,8),LSQ2(9,8),NSQF(4,6),NFAC(3,8)
      DIMENSION LICO(12),LSCO(6)
C     .................................................. 12 lines of the cube
C                1  2  3  4  5  6  7  8  9 10 11 12
      DATA NSQL/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &           2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     .................................thin lines containing corner #
      DATA LSQ1/1,4, 9,  ! 1
     &          1,2,10,  ! 2
     &          2,3,11,  ! 3
     &          3,4,12,  ! 4
     &          5,8, 9,  ! 5
     &          5,6,10,  ! 6
     &          6,7,11,  ! 7
     &          7,8,12/  ! 8
C     ................................thick lines not containing corner #
      DATA LSQ2/2,3,5,6,7,8,10,11,12,  ! 1
     &          3,4,5,6,7,8, 9,11,12,  ! 2
     &          1,4,5,6,7,8, 9,10,12,  ! 3
     &          1,2,5,6,7,8, 9,10,11,  ! 4
     &          1,2,3,4,6,7,10,11,12,  ! 5
     &          1,2,3,4,7,8, 9,11,12,  ! 6
     &          1,2,3,4,5,8, 9,10,12,  ! 7
     &          1,2,3,4,5,6, 9,10,11/  ! 8
C     .................................... corners of the 6 Faces of the cube
      DATA NSQF/1,2,3,4,   ! 1 BLACK=1
     &          5,6,7,8,   ! 2 BLACK=1
     &          1,2,6,5,   ! 3 RED=5
     &          2,3,7,6,   ! 4 BLUE=6
     &          3,4,8,7,   ! 5 RED=5
     &          4,1,5,8/   ! 6 BLUE=6
C     .......................................... face containing corner # 
      DATA NFAC/1,3,6,   ! 1
     &          1,3,4,   ! 2
     &          1,4,5,   ! 3
     &          1,5,6,   ! 4
     &          2,3,6,   ! 5
     &          2,3,4,   ! 6
     &          2,4,5,   ! 7
     &          2,5,6/   ! 8
C     ............................................ colors of lines and faces
      DATA LSCO/1,1,5,6,5,6/
      DATA LICO/9,9,9,9,10,10,10,10,13,14,12,8/
C     ............................................. line width
      DATA DL1/1./,DL2/3./,DLL/3./,DLC/3./
C
      PARAMETER (NLIN=99,MVX=9)
      DIMENSION X1(2,NLIN),Y1(2,NLIN),Z1(2,NLIN),NCOL(NLIN)
      DIMENSION XYZL(3,2),HVDL(6,2,NLIN)
C     ............................................ face of nside #
      DIMENSION NSID(2,NLIN),NFASI(3)
      DATA NFASI/3,4,1/
      DIMENSION NSCOL(3)
      DATA NSCOL/9,10,8/,QCR/0.04/
C
      DIMENSION H(4),V(4),HCR(2),VCR(2)
C
      LOGICAL FNO,FBUT(4),DGPNTR,FIN(NLIN)
      DATA IDEB/0/
C
      TPARDA=
     &  'J_PTO,J_RAL'
      CALL DPARAM(10
     &  ,J_PTO,J_RAL)
C
      TPARDA=
     &  'J_RFA,J_RTA,J_RGA'
      CALL DPARAM(86
     &  ,J_RFA,J_RTA,J_RGA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQCL(IAREDO)
      IF(IZOMDO.EQ.0) THEN
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      ELSE
        CALL DRO_CONE_TO_HV(HRB,VRB)
C       CALL DRORCB(HRB,VRB)
      END IF
      CALL DCTYEX('RO TPC',6)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'Y"',2)
      CALL DPARSV(86,'RBA',4,-1.)
      CALL DWRT('Rotation of artificial cube. Move mouse.')
      CALL DPARGI(86,'RCB',L7)
      CALL DPARGI(86,'RCS',LSCO(1))
      CALL DPARGI(86,'RCT',LSCO(3))
      CALL DPARGI(86,'RCF',LSCO(4))
      LSCO(2)=LSCO(1)
      LSCO(5)=LSCO(3)
      LSCO(6)=LSCO(4)
      CALL DCDST0
      CALL DPARGV(86,'RFS',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(1)
        CALL DPARGV(86,'RFS',2,GRCODD(LS))
        RDCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFT',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(3)
        CALL DPARGV(86,'RFT',2,RDCODD(LS))
        GRCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFF',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(4)
        CALL DPARGV(86,'RFF',2,BLCODD(LS))
        RDCODD(LS)=0.
        GRCODD(LS)=0.
      END IF
      CALL DW_SET_CO
      CALL DPARGI(86,'RLL',LICO( 1))
      CALL DPARGI(86,'RRL',LICO( 5))
      CALL DPARGI(86,'RL1',LICO( 9))
      CALL DPARGI(86,'RL2',LICO(10))
      CALL DPARGI(86,'RL3',LICO(11))
      CALL DPARGI(86,'RL4',LICO(12))
      CALL UFILL(LICO,2,4,LICO(1))
      CALL UFILL(LICO,6,8,LICO(5))
      CALL DPARGV(86,'RSC',2,QCR)
      CALL DPARGV(86,'RQC',4,RQ4)
      IF(RQ4.EQ.1.) THEN
        CALL DPARGV(86,'RQC',2,QDI)
      ELSE
        QDI=0.
      END IF
      CALL DPARGV(86,'RD1',2,DL1)
      CALL DPARGV(86,'RD2',2,DL2)
      CALL DPARGV(86,'RDC',2,DLC)
      CALL DPARGV(86,'RDT',2,DLL)
      CALL DPARGI(86,'RXS',NSCOL(1))
      CALL DPARGI(86,'RXT',NSCOL(2))
      CALL DPARGI(86,'RXF',NSCOL(3))
      H1=HLOWDG(IAREDO)
      V1=VLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      VHS=VM+DS
      VLS=VM-DS
      DLIN=DLINDD
      AL0=PARADA(2,J_RAL)
      MODE=2
      TX='GA ='
      JANG=J_RGA
      CALL DGSCUR(HM,VM)
      AL=0.
C     ............................................. setup tracks
      CALL DROSTR(NLIN,NUM,NFRFT,X1,Y1,Z1,NCOL)
      NFR1=NFRFT+1
C     ........................................... SETUP ANGLES      
   33 CALL D_ROT_AC_IN(0,0.,FNO,BETA)
      IF(FNO) GO TO 99
C     K=1,8 cube corners / J=1,3 xyz
C     L=1,NUM # of lines / I=1,2 points of line
C     ....................... get rotated cube (rotation 2, system C1 -> CR)
      DO K=1,8
        CALL D_ROT_VECTOR(2,XYZSQ(1,K),HUWSQ(1,K))
      END DO
C     ........................................... calculate cube size
      HMAX=0.01
      VMAX=0.01
      DO K=1,8
        HMAX=MAX(HMAX,HUWSQ(1,K))
        VROT= CA*HUWSQ(2,K)+SA*HUWSQ(3,K)
        VMAX=MAX(VMAX,VROT)
      END DO
      SC=SFAC*MIN(DH/HMAX,DV/VMAX)
      CALL DQ_CLIP_3D_IN(SC)
      DO K=1,8
        DO J=1,3
          HUWSQ(J,K)=HUWSQ(J,K)*SC
        END DO
      END DO
C     ....................... 1 to NFRFT = tracks,  to NUM = sec. tracks
      DO L=1,NUM
        DO I=1,2
          XYZL(1,I)=X1(I,L)
          XYZL(2,I)=Y1(I,L)
          XYZL(3,I)=Z1(I,L)
        END DO
        CALL D_ROT_LINE_AC(XYZL,HVDL(1,1,L),NSID(1,L),FIN(L))
      END DO
C     
C     ............................................. start of loop
    1 CALL DGLEVL(L7)
      CALL DQFAR(H1,V1,H2,V2)
      SA=SIND(AL)
      CA=COSD(AL)
C     ............................................. rotate cube      
      DO K=1,8
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)= CA*HUWSQ(2,K)+SA*HUWSQ(3,K)
        DSQ(K)=-SA*HUWSQ(2,K)+CA*HUWSQ(3,K)
      END DO
C     .............................................. deep corner of cube
      DMIN=9999.
      DO K=1,8
        IF(DSQ(K).LT.DMIN) THEN
          DMIN=DSQ(K)
          KMIN=K
        END IF
      END DO
C     ............................................... draw faces
      DO N=1,3
        MN=NFAC(N,KMIN)
        DO I=1,4
          MI=NSQF(I,MN)
          H(I)=HSQ(MI)+HM
          V(I)=VSQ(MI)+VM
        END DO
        CALL DGLEVL(LSCO(MN))
        CALL DGAREA(4,H,V)
      END DO
C     ................................................ draw thin lines
      DLINDD=DL1
      DO N=1,3
        MN=LSQ1(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw thick lines
      DLINDD=DL2
      DO N=1,9
        MN=LSQ2(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw tracks
      DO L=1,NUM
        IF(FIN(L)) THEN
          DO I=1,2
            H(I)=HM+HVDL(1,I,L)
            V(I)=VM+CA*HVDL(2,I,L)+SA*HVDL(3,I,L)
            DI  =  -SA*HVDL(2,I,L)+CA*HVDL(3,I,L)
            IF(NSID(I,L).NE.0) THEN
              CALL DGLEVL(NSCOL(NSID(I,L)))
              DLINDD=DLC
              NF=NFASI(NSID(I,L))
              N0=NSQF(2,NF)
              N1=NSQF(1,NF)
              DO IC=1,2
C               ............................................ draw crosses
                DCR=QCR*(1.+QDI*DI/SC)
                DCH=DCR*(HSQ(N1)-HSQ(N0))
                DCV=DCR*(VSQ(N1)-VSQ(N0))
                HCR(1)=H(I)-DCH
                HCR(2)=H(I)+DCH
                VCR(1)=V(I)-DCV
                VCR(2)=V(I)+DCV
                CALL DGDRAW(2,HCR,VCR)
                N1=NSQF(3,NF)
              END DO
            END IF
          END DO
          DLINDD=DLL
          CALL DGLEVL(NCOL(L))
          IF(L.EQ.NFR1) CALL DGDASH(NDASH,IP)
          CALL DGDRAW(2,H,V)
        END IF
      END DO
      CALL DGDASH(0,0)
      CALL DQCHKX
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(1)) THEN
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          CALL DTYANS(
     &      'blank=Continue, F=phi, Theta, Gamma. else = stop',
     &      'FTG c',NANSW)
C            12345
          IF(     NANSW.LE.0) THEN
            GO TO 99
          ELSE IF(NANSW.EQ.1) THEN
            MODE=1
            TX(1:2)='FI'
            JANG=J_RFA
          ELSE IF(NANSW.EQ.2) THEN
            MODE=2
            TX(1:2)='TE'
            JANG=J_RTA
          ELSE IF(NANSW.EQ.3) THEN
            MODE=1
            TX(1:2)='GA'
            JANG=J_RGA
          END IF
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
C       ........................................ rotate cube only
        PARADA(2,JANG)=PARADA(2,JANG)+DA
        IF(     MODE.EQ.1) THEN
          IF(     PARADA(2,JANG).LT.  0.) THEN
            PARADA(2,JANG)=PARADA(2,JANG)+360.
          ELSE IF(PARADA(2,JANG).GE.360.) THEN 
            PARADA(2,JANG)=PARADA(2,JANG)-360.
          END IF
        ELSE IF(MODE.EQ.2) THEN
          IF(     PARADA(2,J_RTA).LT.  0.) THEN
            PARADA(2,J_RTA)=0.
          ELSE IF(PARADA(2,J_RTA).GE.180.) THEN 
            PARADA(2,J_RTA)=180.
          END IF
        END IF
        WRITE(TXTADW,1001) TX,PARADA(2,JANG)
 1001   FORMAT('Artificial cube rotation:',A,F7.2,'m')
        CALL DWR_OVER_PRINT(37)
        GO TO 33
      END IF
   99 DLINDD=DLIN
      CALL DCDSTC
      CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DRO_SMOOTH_ROT_AC_FIXED
CH..............+++                   AD
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++ DRO_SMOOTH_ROT_AC_FIXED
CH
      SUBROUTINE DRO_SMOOTH_ROT_AC_FIXED
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C
      DIMENSION HRB(4),VRB(4)
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,L7/7/,SFAC/.97/
      DIMENSION IP(2)
      DATA IP/8,4/,NDASH/2/,NSHAD/-1/
C
      DIMENSION XYZSQ(3,8)
C     ............................................ cube in system C1
      DATA XYZSQ/
     &   1., 1.,-1.,    ! 1
     &   1.,-1.,-1.,    ! 2
     &  -1.,-1.,-1.,    ! 3
     &  -1., 1.,-1.,    ! 4
     &   1., 1., 1.,    ! 5
     &   1.,-1., 1.,    ! 6
     &  -1.,-1., 1.,    ! 7
     &  -1., 1., 1./    ! 8
C
      DIMENSION HUWSQ(3,8),HSQ(8),VSQ(8),DSQ(8),NSQL(12,2)
      DIMENSION LSQ1(3,8),LSQ2(9,8),NSQF(4,6),NFAC(3,8)
      DIMENSION LICO(12),LSCO(6)
C     .................................................. 12 lines of the cube
C                1  2  3  4  5  6  7  8  9 10 11 12
      DATA NSQL/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &           2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     .................................thin lines containing corner #
      DATA LSQ1/1,4, 9,  ! 1
     &          1,2,10,  ! 2
     &          2,3,11,  ! 3
     &          3,4,12,  ! 4
     &          5,8, 9,  ! 5
     &          5,6,10,  ! 6
     &          6,7,11,  ! 7
     &          7,8,12/  ! 8
C     ................................thick lines not containing corner #
      DATA LSQ2/2,3,5,6,7,8,10,11,12,  ! 1
     &          3,4,5,6,7,8, 9,11,12,  ! 2
     &          1,4,5,6,7,8, 9,10,12,  ! 3
     &          1,2,5,6,7,8, 9,10,11,  ! 4
     &          1,2,3,4,6,7,10,11,12,  ! 5
     &          1,2,3,4,7,8, 9,11,12,  ! 6
     &          1,2,3,4,5,8, 9,10,12,  ! 7
     &          1,2,3,4,5,6, 9,10,11/  ! 8
C     .................................... corners of the 6 Faces of the cube
      DATA NSQF/1,2,3,4,   ! 1 BLACK=1
     &          5,6,7,8,   ! 2 BLACK=1
     &          1,2,6,5,   ! 3 RED=5
     &          2,3,7,6,   ! 4 BLUE=6
     &          3,4,8,7,   ! 5 RED=5
     &          4,1,5,8/   ! 6 BLUE=6
C     .......................................... face containing corner # 
      DATA NFAC/1,3,6,   ! 1
     &          1,3,4,   ! 2
     &          1,4,5,   ! 3
     &          1,5,6,   ! 4
     &          2,3,6,   ! 5
     &          2,3,4,   ! 6
     &          2,4,5,   ! 7
     &          2,5,6/   ! 8
C     ............................................ colors of lines and faces
      DATA LSCO/1,1,5,6,5,6/
      DATA LICO/9,9,9,9,10,10,10,10,13,14,12,8/
C     ............................................. line width
      DATA DL1/1./,DL2/3./,DLL/3./,DLC/3./
C
      PARAMETER (NLIN=99,MVX=9)
      DIMENSION X1(2,NLIN),Y1(2,NLIN),Z1(2,NLIN),NCOL(NLIN)
      DIMENSION XYZL(3,2),HVDL(6,2)
C     ............................................ face of nside #
      DIMENSION NSID(2),NFASI(3)
      DATA NFASI/3,4,1/
      DIMENSION NSCOL(3)
      DATA NSCOL/9,10,8/,QCR/0.04/
C
      DIMENSION H(4),V(4),HCR(2),VCR(2)
      DATA IDEB/0/
C      DIMENSION RDCO(2),GRCO(2),BLCO(2)
C      DATA RDCO/0.35,0.  /
C      DATA GRCO/0.  ,0.  /
C      DATA BLCO/0.  ,0.4 /
C
      LOGICAL FNO,FBUT(4),DGPNTR,FIN
C
C     K=1,8 cube corners / J=1,3 xyz
C     L=1,NUM # of lines / I=1,2 points of line
C
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO,J_RAL'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PTO,J_RAL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQCL(IAREDO)
      IF(IZOMDO.EQ.0) THEN
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      ELSE
        CALL DRO_CONE_TO_HV(HRB,VRB)
C       CALL DRORCB(HRB,VRB)
      END IF
      CALL DCTYEX('RO TPC',6)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'Y"',2)
      CALL DWRT('Rotation of data in fixed art. cube. Move mouse.')
      CALL DPARSV(86,'RBA',4,-1.)
      CALL DPARGI(86,'RCB',L7)
      CALL DPARGI(86,'RCS',LSCO(1))
      CALL DPARGI(86,'RCT',LSCO(3))
      CALL DPARGI(86,'RCF',LSCO(4))
      LSCO(2)=LSCO(1)
      LSCO(5)=LSCO(3)
      LSCO(6)=LSCO(4)
      CALL DCDST0
      CALL DPARGV(86,'RFS',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(1)
        CALL DPARGV(86,'RFS',2,GRCODD(LS))
        RDCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFT',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(3)
        CALL DPARGV(86,'RFT',2,RDCODD(LS))
        GRCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFF',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(4)
        CALL DPARGV(86,'RFF',2,BLCODD(LS))
        RDCODD(LS)=0.
        GRCODD(LS)=0.
      END IF
      CALL DW_SET_CO
      CALL DPARGI(86,'RLL',LICO( 1))
      CALL DPARGI(86,'RRL',LICO( 5))
      CALL DPARGI(86,'RL1',LICO( 9))
      CALL DPARGI(86,'RL2',LICO(10))
      CALL DPARGI(86,'RL3',LICO(11))
      CALL DPARGI(86,'RL4',LICO(12))
      CALL UFILL(LICO,2,4,LICO(1))
      CALL UFILL(LICO,6,8,LICO(5))
      CALL DPARGV(86,'RSC',2,QCR)
      CALL DPARGV(86,'RQC',4,RQ4)
      IF(RQ4.EQ.1.) THEN
        CALL DPARGV(86,'RQC',2,QDI)
      ELSE
        QDI=0.
      END IF
      CALL DPARGV(86,'RD1',2,DL1)
      CALL DPARGV(86,'RD2',2,DL2)
      CALL DPARGV(86,'RDC',2,DLC)
      CALL DPARGV(86,'RDT',2,DLL)
      CALL DPARGI(86,'RXS',NSCOL(1))
      CALL DPARGI(86,'RXT',NSCOL(2))
      CALL DPARGI(86,'RXF',NSCOL(3))
      CALL DPARGV(86,'RSH',4,RSH4)
      CALL DPARGI(86,'RSH',LSHAD)
      CALL DPARGV(86,'RDS',2,DLSH)
      H1=HLOWDG(IAREDO)
      V1=VLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      QH=DH/DV
      VHS=VM+DS
      VLS=VM-DS
      DLIN=DLINDD
      CALL DGSCUR(HM,VM)
      CALL D_ROT_AC_IN(0,0.,FNO,BETA)
      IF(FNO) GO TO 99
      HMAX=0.01
      VMAX=0.01
      DO K=1,8
        CALL D_ROT_VECTOR(2,XYZSQ(1,K),HUWSQ(1,K))
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)=HUWSQ(2,K)
        HMAX=MAX(HMAX,HUWSQ(1,K))
        VMAX=MAX(VMAX,HUWSQ(2,K))
      END DO
      SC=SFAC*MIN(DH/HMAX,DV/VMAX)
      CALL DQ_CLIP_3D_IN(SC)
      DO K=1,8
        DO J=1,3
          HUWSQ(J,K)=HUWSQ(J,K)*SC
        END DO
      END DO
      DO K=1,8
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)=HUWSQ(2,K)
        DSQ(K)=HUWSQ(3,K)
      END DO
      DMIN=9999.
      DO K=1,8
        IF(DSQ(K).LT.DMIN) THEN
          DMIN=DSQ(K)
          KMIN=K
        END IF
      END DO
C     ............................................. setup tracks
      CALL DROSTR(NLIN,NUM,NFRFT,X1,Y1,Z1,NCOL)
      NFR1=NFRFT+1
C     
C     ............................................. start of loop
    1 CALL DGLEVL(L7)
      CALL DQFAR(H1,V1,H2,V2)
C     ............................................... draw faces
      DO N=1,3
        MN=NFAC(N,KMIN)
        DO I=1,4
          MI=NSQF(I,MN)
          H(I)=QH*HSQ(MI)+HM
          V(I)=   VSQ(MI)+VM
        END DO
        CALL DGLEVL(LSCO(MN))
        CALL DGAREA(4,H,V)
      END DO
C     ................................................ draw thin lines
      DLINDD=DL1
      DO N=1,3
        MN=LSQ1(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=QH*HSQ(II)+HM
          V(I)=   VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw thick lines
      DLINDD=DL2
      DO N=1,9
        MN=LSQ2(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=QH*HSQ(II)+HM
          V(I)=   VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw tracks
      CALL D_ROT_VECTOR_IN(1,
     &  PARADA(2,J_PFI),
     &  PARADA(2,J_PTE),
     &  PARADA(2,J_RAL))
C     ....................... 1 to NFRFT = tracks,  to NUM = sec. tracks
      CALL D_ROT_AC_IN(NSHAD,SC,FNO,BETA)
      DO L=1,NUM
        DO I=1,2
          XYZL(1,I)=X1(I,L)
          XYZL(2,I)=Y1(I,L)
          XYZL(3,I)=Z1(I,L)
        END DO
        CALL D_ROT_LINE_AC(XYZL,HVDL,NSID,FIN)
        IF(FIN) THEN
          IF(L.GE.NFR1) CALL DGDASH(NDASH,IP)
          IF(NSHAD.EQ.1) THEN
C           .......................................... SHADOW
            DO I=1,2
              H(I)= QH*HVDL(4,I)+HM
              V(I)=    HVDL(5,I)+VM
            END DO
            IF(RSH4.GE.0) THEN
              CALL DGLEVL(LSHAD)
            ELSE
              CALL DGLEVL(NCOL(L))
            END IF
            DLINDD=DLSH
            CALL DGDRAW(2,H,V)
          END IF
C         ............................................ tracks
          DLINDD=DLL
          DO I=1,2
            H(I)= QH*HVDL(1,I)+HM
            V(I)=    HVDL(2,I)+VM
          END DO
          CALL DGLEVL(NCOL(L))
          CALL DGDRAW(2,H,V)
C         ............................................ draw crosses
          IF(L.GE.NFR1) CALL DGDASH(0,0)
          DO I=1,2
            DI  = HVDL(3,I)
            IF(NSID(I).NE.0) THEN
              CALL DGLEVL(NSCOL(NSID(I)))
              DLINDD=DLC
              NF=NFASI(NSID(I))
              N0=NSQF(2,NF)
              N1=NSQF(1,NF)
              DO IC=1,2
                DCR=QCR*(1.+QDI*DI/SC)
                DCH=DCR*(HSQ(N1)-HSQ(N0))
                DCV=DCR*(VSQ(N1)-VSQ(N0))
                HCR(1)=H(I)-DCH
                HCR(2)=H(I)+DCH
                VCR(1)=V(I)-DCV
                VCR(2)=V(I)+DCV
                CALL DGDRAW(2,HCR,VCR)
                N1=NSQF(3,NF)
              END DO
            END IF
          END DO
        END IF
      END DO
      CALL DGDASH(0,0)
      CALL DQCHKX
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(1)) THEN
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          CALL DTYANS(
     &      'blank=Continue, else = stop',
     &      ' CS',NANSW)
          IF(     NANSW.LE.0) THEN
            GO TO 99
          ELSE IF(NANSW.EQ.3) THEN
            NSHAD=-NSHAD
            GO TO 1
          END IF
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
        PARADA(2,J_RAL)=PARADA(2,J_RAL)+DA
        IF(PARADA(2,J_RAL).GE.360.) THEN
          PARADA(2,J_RAL)=PARADA(2,J_RAL)-360.
        ELSE IF(PARADA(2,J_RAL).LT.0.) THEN
          PARADA(2,J_RAL)=PARADA(2,J_RAL)+360.
        END IF
        WRITE(TXTADW,1001) PARADA(2,J_RAL)
 1001   FORMAT('Event rotated: AL =',F7.2,'m')
        CALL DWR_OVER_PRINT(27)
        GO TO 1
      END IF
   99 DLINDD=DLIN
      CALL DCDSTC
      CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DRO_SMOOTH_ROT_AC
CH..............+++         AC
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ DRO_SMOOTH_ROT_AC
CH
      SUBROUTINE DRO_SMOOTH_ROT_AC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C
      DIMENSION HRB(4),VRB(4)
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,L7/7/,SFAC/.97/
      DIMENSION IP(2)
      DATA IP/8,4/,NDASH/2/

C
      DIMENSION XYZSQ(3,8)
C     ............................................ cube in system C1
      DATA XYZSQ/
     &   1., 1.,-1.,    ! 1
     &   1.,-1.,-1.,    ! 2
     &  -1.,-1.,-1.,    ! 3
     &  -1., 1.,-1.,    ! 4
     &   1., 1., 1.,    ! 5
     &   1.,-1., 1.,    ! 6
     &  -1.,-1., 1.,    ! 7
     &  -1., 1., 1./    ! 8
C
      DIMENSION HUWSQ(3,8),HSQ(8),VSQ(8),DSQ(8),NSQL(12,2)
      DIMENSION LSQ1(3,8),LSQ2(9,8),NSQF(4,6),NFAC(3,8)
      DIMENSION LICO(12),LSCO(6)
C     .................................................. 12 lines of the cube
C                1  2  3  4  5  6  7  8  9 10 11 12
      DATA NSQL/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &           2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     .................................thin lines containing corner #
      DATA LSQ1/1,4, 9,  ! 1
     &          1,2,10,  ! 2
     &          2,3,11,  ! 3
     &          3,4,12,  ! 4
     &          5,8, 9,  ! 5
     &          5,6,10,  ! 6
     &          6,7,11,  ! 7
     &          7,8,12/  ! 8
C     ................................thick lines not containing corner #
      DATA LSQ2/2,3,5,6,7,8,10,11,12,  ! 1
     &          3,4,5,6,7,8, 9,11,12,  ! 2
     &          1,4,5,6,7,8, 9,10,12,  ! 3
     &          1,2,5,6,7,8, 9,10,11,  ! 4
     &          1,2,3,4,6,7,10,11,12,  ! 5
     &          1,2,3,4,7,8, 9,11,12,  ! 6
     &          1,2,3,4,5,8, 9,10,12,  ! 7
     &          1,2,3,4,5,6, 9,10,11/  ! 8
C     .................................... corners of the 6 Faces of the cube
      DATA NSQF/1,2,3,4,   ! 1 BLACK=1
     &          5,6,7,8,   ! 2 BLACK=1
     &          1,2,6,5,   ! 3 RED=5
     &          2,3,7,6,   ! 4 BLUE=6
     &          3,4,8,7,   ! 5 RED=5
     &          4,1,5,8/   ! 6 BLUE=6
C     .......................................... face containing corner # 
      DATA NFAC/1,3,6,   ! 1
     &          1,3,4,   ! 2
     &          1,4,5,   ! 3
     &          1,5,6,   ! 4
     &          2,3,6,   ! 5
     &          2,3,4,   ! 6
     &          2,4,5,   ! 7
     &          2,5,6/   ! 8
C     ............................................ colors of lines and faces
      DATA LSCO/1,1,5,6,5,6/
      DATA LICO/9,9,9,9,10,10,10,10,13,14,12,8/
C     ............................................. line width
      DATA DL1/1./,DL2/3./,DLL/3./,DLC/3./
C
      PARAMETER (NLIN=99,MVX=9)
      DIMENSION X1(2,NLIN),Y1(2,NLIN),Z1(2,NLIN),NCOL(NLIN)
      DIMENSION XYZL(3,2),HVDL(6,2,NLIN)
C     ............................................ face of nside #
      DIMENSION NSID(2,NLIN),NFASI(3)
      DATA NFASI/3,4,1/
      DIMENSION NSCOL(3)
      DATA NSCOL/9,10,8/,QCR/0.04/
      DATA IDEB/0/
C
      DIMENSION H(4),V(4),HCR(2),VCR(2),DP(2)
C      DIMENSION RDCO(2),GRCO(2),BLCO(2)
C      DATA RDCO/0.35,0.  /
C      DATA GRCO/0.  ,0.  /
C      DATA BLCO/0.  ,0.4 /
C
      LOGICAL FNO,FBUT(4),DGPNTR,FIN(NLIN)
C
      TPARDA=
     &  'J_PTO,J_RAL'
      CALL DPARAM(10
     &  ,J_PTO,J_RAL)
C
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQCL(IAREDO)
      IF(IZOMDO.EQ.0) THEN
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      ELSE
        CALL DRO_CONE_TO_HV(HRB,VRB)
C       CALL DRORCB(HRB,VRB)
      END IF
      CALL DCTYEX('RO TPC',6)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'Y"',2)
      CALL DWRT('Rotation of data and artificila cube. Move mouse.')
      CALL DPARSV(86,'RBA',4,-1.)
      CALL DPARGI(86,'RCB',L7)
      CALL DPARGI(86,'RCS',LSCO(1))
      CALL DPARGI(86,'RCT',LSCO(3))
      CALL DPARGI(86,'RCF',LSCO(4))
      LSCO(2)=LSCO(1)
      LSCO(5)=LSCO(3)
      LSCO(6)=LSCO(4)
      CALL DCDST0
      CALL DPARGV(86,'RFS',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(1)
        CALL DPARGV(86,'RFS',2,GRCODD(LS))
        RDCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFT',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(3)
        CALL DPARGV(86,'RFT',2,RDCODD(LS))
        GRCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFF',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(4)
        CALL DPARGV(86,'RFF',2,BLCODD(LS))
        RDCODD(LS)=0.
        GRCODD(LS)=0.
      END IF
      CALL DW_SET_CO
      CALL DPARGI(86,'RLL',LICO( 1))
      CALL DPARGI(86,'RRL',LICO( 5))
      CALL DPARGI(86,'RL1',LICO( 9))
      CALL DPARGI(86,'RL2',LICO(10))
      CALL DPARGI(86,'RL3',LICO(11))
      CALL DPARGI(86,'RL4',LICO(12))
      CALL UFILL(LICO,2,4,LICO(1))
      CALL UFILL(LICO,6,8,LICO(5))
      CALL DPARGV(86,'RSC',2,QCR)
      CALL DPARGV(86,'RQC',4,RQ4)
      IF(RQ4.EQ.1.) THEN
        CALL DPARGV(86,'RQC',2,QDI)
      ELSE
        QDI=0.
      END IF
      CALL DPARGV(86,'RD1',2,DL1)
      CALL DPARGV(86,'RD2',2,DL2)
      CALL DPARGV(86,'RDC',2,DLC)
      CALL DPARGV(86,'RDT',2,DLL)
      CALL DPARGI(86,'RXS',NSCOL(1))
      CALL DPARGI(86,'RXT',NSCOL(2))
      CALL DPARGI(86,'RXF',NSCOL(3))
      CALL DPARGV(86,'RNP',4,RNP4)
      IF(RNP4.GT.0.) THEN
        CALL DPARGV(86,'RNP',2,RNP2)
        CALL DPARGV(86,'RWP',2,RWP)
        QD0=(HHGHDG(IAREDO)-HLOWDG(IAREDO))/RNP2
        RWP=0.5*RWP
      END IF
      H1=HLOWDG(IAREDO)
      V1=VLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      VHS=VM+DS
      VLS=VM-DS
      DLIN=DLINDD
      AL0=PARADA(2,J_RAL)
      CALL DGSCUR(HM,VM)
      AL=0.
C     ............................................. setup tracks
      CALL DROSTR(NLIN,NUM,NFRFT,X1,Y1,Z1,NCOL)
      NFR1=NFRFT+1
C     ........................................... SETUP ANGLES      
   33 CALL D_ROT_AC_IN(0,0.,FNO,BETA)
      IF(FNO) GO TO 99
C     K=1,8 cube corners / J=1,3 xyz
C     L=1,NUM # of lines / I=1,2 points of line
C     ....................... get rotated cube (rotation 2, system C1 -> CR)
      DO K=1,8
        CALL D_ROT_VECTOR(2,XYZSQ(1,K),HUWSQ(1,K))
      END DO
C     ........................................... calculate cube size
      HMAX=0.01
      RMAX=0.01
      DO K=1,8
        HMAX=MAX(HMAX,HUWSQ(1,K))
        RMAX=MAX(RMAX,HUWSQ(2,K)**2+HUWSQ(3,K)**2)
      END DO
      RMAX=SQRT(RMAX)
      SC=SFAC*MIN(DH/HMAX,DV/RMAX)
      CALL DQ_CLIP_3D_IN(SC)
      DO K=1,8
        DO J=1,3
          HUWSQ(J,K)=HUWSQ(J,K)*SC
        END DO
      END DO
C     ....................... 1 to NFRFT = tracks,  to NUM = sec. tracks
      DO L=1,NUM
        DO I=1,2
          XYZL(1,I)=X1(I,L)
          XYZL(2,I)=Y1(I,L)
          XYZL(3,I)=Z1(I,L)
        END DO
        CALL D_ROT_LINE_AC(XYZL,HVDL(1,1,L),NSID(1,L),FIN(L))
      END DO
C     
C     ............................................. start of loop
    1 CALL DGLEVL(L7)
      CALL DQFAR(H1,V1,H2,V2)
      SA=SIND(AL)
      CA=COSD(AL)
C     ............................................. rotate cube      
      DO K=1,8
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)= CA*HUWSQ(2,K)+SA*HUWSQ(3,K)
        DSQ(K)=-SA*HUWSQ(2,K)+CA*HUWSQ(3,K)
      END DO
C     .............................................. deep corner of cube
      DMIN=9999.
      DO K=1,8
        IF(DSQ(K).LT.DMIN) THEN
          DMIN=DSQ(K)
          KMIN=K
        END IF
      END DO
C     ............................................... draw faces
      DO N=1,3
        MN=NFAC(N,KMIN)
        DO I=1,4
          MI=NSQF(I,MN)
          H(I)=HSQ(MI)+HM
          V(I)=VSQ(MI)+VM
        END DO
        CALL DGLEVL(LSCO(MN))
        CALL DGAREA(4,H,V)
      END DO
C     ................................................ draw thin lines
      DLINDD=DL1
      DO N=1,3
        MN=LSQ1(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw thick lines
      DLINDD=DL2
      DO N=1,9
        MN=LSQ2(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw tracks
      DO L=1,NUM
        IF(FIN(L)) THEN
          DO I=1,2
            H(I)=HM+HVDL(1,I,L)
            V(I)=VM+CA*HVDL(2,I,L)+SA*HVDL(3,I,L)
            DI  =  -SA*HVDL(2,I,L)+CA*HVDL(3,I,L)
            DP(I)=DI
            IF(NSID(I,L).NE.0) THEN
              CALL DGLEVL(NSCOL(NSID(I,L)))
              DLINDD=DLC
              NF=NFASI(NSID(I,L))
              N0=NSQF(2,NF)
              N1=NSQF(1,NF)
              DO IC=1,2
C               ............................................ draw crosses
                DCR=QCR*(1.+QDI*DI/SC)
                DCH=DCR*(HSQ(N1)-HSQ(N0))
                DCV=DCR*(VSQ(N1)-VSQ(N0))
                HCR(1)=H(I)-DCH
                HCR(2)=H(I)+DCH
                VCR(1)=V(I)-DCV
                VCR(2)=V(I)+DCV
                CALL DGDRAW(2,HCR,VCR)
                N1=NSQF(3,NF)
              END DO
            END IF
          END DO
          DLINDD=DLL
          CALL DGLEVL(NCOL(L))
          IF(L.EQ.NFR1) CALL DGDASH(NDASH,IP)
          CALL DGDRAW(2,H,V)
          IF(RNP4.GT.0.) THEN
            DHP= H(2)- H(1)
            DVP= V(2)- V(1)
            DDP=DP(2)-DP(1)
            RDP=SQRT(DHP*DHP+DVP*DVP+DDP*DDP)
            IF(RDP.GT.0.) THEN
              QDP=QD0/RDP
              DHP=DHP*QDP
              DVP=DVP*QDP
              HPP=H(1)
              VPP=V(1)
              KDP=1./QDP
              DO K=1,KDP
                CALL DQFAR(HPP-RWP,VPP-RWP,HPP+RWP,VPP+RWP)
                HPP=HPP+DHP
                VPP=VPP+DVP
              END DO
            END IF
          END IF
        END IF
      END DO
      CALL DGDASH(0,0)
      CALL DQCHKX
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(1)) THEN
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          CALL DTYANS(
     &      'blank=Continue, anything else = stop',
     &      ' C',NANSW)
C            1234567
          IF(NANSW.LE.0) GO TO 99
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
C       ........................................ rotate cube + tracks
        AL=AL+DA
        IF(AL.GE.360.) THEN
          AL=AL-360.
        ELSE IF(AL.LT.0.) THEN
          AL=AL+360.
        END IF
        AG=AL0+AL
        IF(AG.GE.360.) THEN
          AG=AG-360.
        ELSE IF(AG.LT.0.) THEN
          AG=AG+360.
        END IF
        WRITE(TXTADW,1001) AG
 1001   FORMAT('General rotation: AL =',F7.2,'m')
        CALL DWR_OVER_PRINT(30)
        GO TO 1
      END IF
   99 DLINDD=DLIN
      CALL DCDSTC
      CALL DQFR(IAREDO)
      CALL DPCSAR
      PARADA(2,J_RAL)=AG
      END
*DK DRO_SMOOTH_ROT_AC_INCLINED
CH..............+++         AI
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++ DRO_SMOOTH_ROT_AC_INCLINED
CH
      SUBROUTINE DRO_SMOOTH_ROT_AC_INCLINED
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C
      DIMENSION HRB(4),VRB(4)
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,L7/7/,SFAC/.97/
      DIMENSION IP(2)
      DATA IP/8,4/,NDASH/2/

C
      DIMENSION XYZSQ(3,8)
C     ............................................ cube in system C1
      DATA XYZSQ/
     &   1., 1.,-1.,    ! 1
     &   1.,-1.,-1.,    ! 2
     &  -1.,-1.,-1.,    ! 3
     &  -1., 1.,-1.,    ! 4
     &   1., 1., 1.,    ! 5
     &   1.,-1., 1.,    ! 6
     &  -1.,-1., 1.,    ! 7
     &  -1., 1., 1./    ! 8
C
      DIMENSION HUWSQ(3,8),HSQ(8),VSQ(8),DSQ(8),NSQL(12,2)
      DIMENSION LSQ1(3,8),LSQ2(9,8),NSQF(4,6),NFAC(3,8)
      DIMENSION LICO(12),LSCO(6)
C     .................................................. 12 lines of the cube
C                1  2  3  4  5  6  7  8  9 10 11 12
      DATA NSQL/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &           2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     .................................thin lines containing corner #
      DATA LSQ1/1,4, 9,  ! 1
     &          1,2,10,  ! 2
     &          2,3,11,  ! 3
     &          3,4,12,  ! 4
     &          5,8, 9,  ! 5
     &          5,6,10,  ! 6
     &          6,7,11,  ! 7
     &          7,8,12/  ! 8
C     ................................thick lines not containing corner #
      DATA LSQ2/2,3,5,6,7,8,10,11,12,  ! 1
     &          3,4,5,6,7,8, 9,11,12,  ! 2
     &          1,4,5,6,7,8, 9,10,12,  ! 3
     &          1,2,5,6,7,8, 9,10,11,  ! 4
     &          1,2,3,4,6,7,10,11,12,  ! 5
     &          1,2,3,4,7,8, 9,11,12,  ! 6
     &          1,2,3,4,5,8, 9,10,12,  ! 7
     &          1,2,3,4,5,6, 9,10,11/  ! 8
C     .................................... corners of the 6 Faces of the cube
      DATA NSQF/1,2,3,4,   ! 1 BLACK=1
     &          5,6,7,8,   ! 2 BLACK=1
     &          1,2,6,5,   ! 3 RED=5
     &          2,3,7,6,   ! 4 BLUE=6
     &          3,4,8,7,   ! 5 RED=5
     &          4,1,5,8/   ! 6 BLUE=6
C     .......................................... face containing corner # 
      DATA NFAC/1,3,6,   ! 1
     &          1,3,4,   ! 2
     &          1,4,5,   ! 3
     &          1,5,6,   ! 4
     &          2,3,6,   ! 5
     &          2,3,4,   ! 6
     &          2,4,5,   ! 7
     &          2,5,6/   ! 8
C     ............................................ colors of lines and faces
      DATA LSCO/1,1,5,6,5,6/
      DATA LICO/9,9,9,9,10,10,10,10,13,14,12,8/
C     ............................................. line width
      DATA DL1/1./,DL2/3./,DLL/3./,DLC/3./
C
      PARAMETER (NLIN=99,MVX=9)
      DIMENSION X1(2,NLIN),Y1(2,NLIN),Z1(2,NLIN),NCOL(NLIN)
      DIMENSION XYZL(3,2),HVDL(6,2,NLIN)
C     ............................................ face of nside #
      DIMENSION NSID(2,NLIN),NFASI(3)
      DATA NFASI/3,4,1/
      DIMENSION NSCOL(3)
      DATA NSCOL/9,10,8/,QCR/0.04/
      DATA IDEB/0/
C
      DIMENSION H(4),V(4),HCR(2),VCR(2)
C      DIMENSION RDCO(2),GRCO(2),BLCO(2)
C      DATA RDCO/0.35,0.  /
C      DATA GRCO/0.  ,0.  /
C      DATA BLCO/0.  ,0.4 /
C
      LOGICAL FNO,FBUT(4),DGPNTR,FIN(NLIN)
C
      TPARDA=
     &  'J_PTO,J_RAL'
      CALL DPARAM(10
     &  ,J_PTO,J_RAL)
C
      TPARDA=
     &  'J_RBA'
      CALL DPARAM(86
     &  ,J_RBA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQCL(IAREDO)
      IF(IZOMDO.EQ.0) THEN
        RTO=PARADA(2,J_PTO)
        CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      ELSE
        CALL DRO_CONE_TO_HV(HRB,VRB)
C       CALL DRORCB(HRB,VRB)
      END IF
      CB=COSD(PARADA(2,J_RBA))
      IF(CB.EQ.0.) GO TO 99
      HRM=0.5*(HRB(3)+HRB(1))
      DRM=0.5*(HRB(3)-HRB(1))/CB
      HRB(1)=HRM-DRM
      HRB(3)=HRM+DRM
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      CALL DCTYEX('RO TPC',6)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'Y"',2)
      CALL DWRT('Rotation of data and artificila cube. Move mouse.')
      CALL DPARSV(86,'RBA',4,1.)
      CALL DPARGI(86,'RCB',L7)
      CALL DPARGI(86,'RCS',LSCO(1))
      CALL DPARGI(86,'RCT',LSCO(3))
      CALL DPARGI(86,'RCF',LSCO(4))
      LSCO(2)=LSCO(1)
      LSCO(5)=LSCO(3)
      LSCO(6)=LSCO(4)
      CALL DCDST0
      CALL DPARGV(86,'RFS',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(1)
        CALL DPARGV(86,'RFS',2,GRCODD(LS))
        RDCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFT',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(3)
        CALL DPARGV(86,'RFT',2,RDCODD(LS))
        GRCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFF',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(4)
        CALL DPARGV(86,'RFF',2,BLCODD(LS))
        RDCODD(LS)=0.
        GRCODD(LS)=0.
      END IF
      CALL DW_SET_CO
      CALL DPARGI(86,'RLL',LICO( 1))
      CALL DPARGI(86,'RRL',LICO( 5))
      CALL DPARGI(86,'RL1',LICO( 9))
      CALL DPARGI(86,'RL2',LICO(10))
      CALL DPARGI(86,'RL3',LICO(11))
      CALL DPARGI(86,'RL4',LICO(12))
      CALL UFILL(LICO,2,4,LICO(1))
      CALL UFILL(LICO,6,8,LICO(5))
      CALL DPARGV(86,'RSC',2,QCR)
      CALL DPARGV(86,'RQC',4,RQ4)
      IF(RQ4.EQ.1.) THEN
        CALL DPARGV(86,'RQC',2,QDI)
      ELSE
        QDI=0.
      END IF
      CALL DPARGV(86,'RD1',2,DL1)
      CALL DPARGV(86,'RD2',2,DL2)
      CALL DPARGV(86,'RDC',2,DLC)
      CALL DPARGV(86,'RDT',2,DLL)
      CALL DPARGI(86,'RXS',NSCOL(1))
      CALL DPARGI(86,'RXT',NSCOL(2))
      CALL DPARGI(86,'RXF',NSCOL(3))
      H1=HLOWDG(IAREDO)
      V1=VLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      VHS=VM+DS
      VLS=VM-DS
      DLIN=DLINDD
      AL0=PARADA(2,J_RAL)
      CALL DGSCUR(HM,VM)
      AL=0.
C     ............................................. setup tracks
      CALL DROSTR(NLIN,NUM,NFRFT,X1,Y1,Z1,NCOL)
      NFR1=NFRFT+1
C     ........................................... SETUP ANGLES      
   33 CALL D_ROT_AC_IN(0,0.,FNO,BETA)
      IF(FNO) GO TO 99
C     K=1,8 cube corners / J=1,3 xyz
C     L=1,NUM # of lines / I=1,2 points of line
C     ....................... get rotated cube (rotation 2, system C1 -> CR)
      DO K=1,8
        CALL D_ROT_VECTOR(2,XYZSQ(1,K),HUWSQ(1,K))
      END DO
C     ........................................... calculate cube size
      RMAX=0.01
      DO K=1,8
        RMAX=MAX(RMAX,HUWSQ(2,K)**2+HUWSQ(3,K)**2)
      END DO
      HMAX=ABS(SQRT(2.)*COSD(45-BETA))
      RMAX=SQRT(RMAX)
      SC=SFAC*MIN(DH/HMAX,DV/RMAX)
      CALL DQ_CLIP_3D_IN(SC)
      DO K=1,8
        DO J=1,3
          HUWSQ(J,K)=HUWSQ(J,K)*SC
        END DO
      END DO
C     ....................... 1 to NFRFT = tracks,  to NUM = sec. tracks
      DO L=1,NUM
        DO I=1,2
          XYZL(1,I)=X1(I,L)
          XYZL(2,I)=Y1(I,L)
          XYZL(3,I)=Z1(I,L)
        END DO
        CALL D_ROT_LINE_AC(XYZL,HVDL(1,1,L),NSID(1,L),FIN(L))
      END DO
C     
      SB=SIND(BETA)
      CB=COSD(BETA)
C     ............................................. start of loop
    1 CALL DGLEVL(L7)
      CALL DQFAR(H1,V1,H2,V2)
      SA=SIND(AL)
      CA=COSD(AL)
C     ............................................. rotate cube      
      DO K=1,8
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)= CA*HUWSQ(2,K)+SA*HUWSQ(3,K)
        DSQ(K)=-SA*HUWSQ(2,K)+CA*HUWSQ(3,K)
      END DO
      DO K=1,8
        HSQK  = CB*HSQ(K)+SB*DSQ(K)
        DSQ(K)=-SB*HSQ(K)+CB*DSQ(K)
        HSQ(K)=HSQK
      END DO
C     .............................................. deep corner of cube
      DMIN=9999.
      DO K=1,8
        IF(DSQ(K).LT.DMIN) THEN
          DMIN=DSQ(K)
          KMIN=K
        END IF
      END DO
C     ............................................... draw faces
      DO N=1,3
        MN=NFAC(N,KMIN)
        DO I=1,4
          MI=NSQF(I,MN)
          H(I)=HSQ(MI)+HM
          V(I)=VSQ(MI)+VM
        END DO
        CALL DGLEVL(LSCO(MN))
        CALL DGAREA(4,H,V)
      END DO
C     ................................................ draw thin lines
      DLINDD=DL1
      DO N=1,3
        MN=LSQ1(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw thick lines
      DLINDD=DL2
      DO N=1,9
        MN=LSQ2(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw tracks
      DO L=1,NUM
        IF(FIN(L)) THEN
          DO I=1,2
            H(I)= HVDL(1,I,L)
            V(I)= CA*HVDL(2,I,L)+SA*HVDL(3,I,L)
            DI  =-SA*HVDL(2,I,L)+CA*HVDL(3,I,L)
            H(I)= CB*H(I)+SB*DI
            H(I)=H(I)+HM
            V(I)=V(I)+VM
            IF(NSID(I,L).NE.0) THEN
              CALL DGLEVL(NSCOL(NSID(I,L)))
              DLINDD=DLC
              NF=NFASI(NSID(I,L))
              N0=NSQF(2,NF)
              N1=NSQF(1,NF)
              DO IC=1,2
C               ............................................ draw crosses
                DCR=QCR*(1.+QDI*DI/SC)
                DCH=DCR*(HSQ(N1)-HSQ(N0))
                DCV=DCR*(VSQ(N1)-VSQ(N0))
                HCR(1)=H(I)-DCH
                HCR(2)=H(I)+DCH
                VCR(1)=V(I)-DCV
                VCR(2)=V(I)+DCV
                CALL DGDRAW(2,HCR,VCR)
                N1=NSQF(3,NF)
              END DO
            END IF
          END DO
          DLINDD=DLL
          CALL DGLEVL(NCOL(L))
          IF(L.EQ.NFR1) CALL DGDASH(NDASH,IP)
          CALL DGDRAW(2,H,V)
        END IF
      END DO
      CALL DGDASH(0,0)
      CALL DQCHKX
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(1)) THEN
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          CALL DTYANS(
     &      'blank=Continue, anything else = stop',
     &      ' C',NANSW)
C            1234567
          IF(NANSW.LE.0) GO TO 99
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
C       ........................................ rotate cube + tracks
        AL=AL+DA
        IF(AL.GE.360.) THEN
          AL=AL-360.
        ELSE IF(AL.LT.0.) THEN
          AL=AL+360.
        END IF
        AG=AL0+AL
        IF(AG.GE.360.) THEN
          AG=AG-360.
        ELSE IF(AG.LT.0.) THEN
          AG=AG+360.
        END IF
        WRITE(TXTADW,1001) AG
 1001   FORMAT('General rotation: AL =',F7.2,'m')
        CALL DWR_OVER_PRINT(30)
        GO TO 1
      END IF
   99 DLINDD=DLIN
      CALL DCDSTC
      CALL DQFR(IAREDO)
      CALL DPCSAR
      PARADA(2,J_RAL)=AG
      END
*DK D_ROT_LINE_AC
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ D_ROT_LINE_AC 
CH
      SUBROUTINE D_ROT_LINE_AC(XYZ,HVD,NSID,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C    line is rotated into rot system (axis = track or jet etc) and clipped
C    inside cube 
c
C    Inputs    : XYZ(3,2) = coordinates of input line
C    Outputs   : HVD(3,2) = coordinates of output line inside artificail cube 
C                FIN = TRUE if line inside
C
C ---------------------------------------------------------------------
C     1=DROD system 1=cube system
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3,2),HVD(6,2),PU(3),PD(3),U(3,2),W(3,2),S(3,2)
      DIMENSION NSID(2)
      LOGICAL FIN
C     ..............................................................
      DIMENSION VXYZ(3),RXYZ(3)
      LOGICAL FNO
      DATA CSIG2/-1./,A90/90./,A0/0./,M/2/
      DO I=1,2
C       .............................. rotate FI,TE axis to H-axis        
        CALL D_ROT_VECTOR(1,XYZ(1,I),PU)
C       ................. transform line into 3D display system around 0,0,0
        PD(1)=AHSCDQ*(PU(1)+CHW)
        PD(2)=BVSCDQ*(PU(2)+CVW)
        PD(3)=BVSCDQ*(PU(3)+CDW)
C       .............................. rotate into cube system CR -> C1        
        CALL D_ROT_VECTOR_INVERS(2,PD,U(1,I))
      END DO
C     ................................ CLIP line into cube
      CALL DQ_CLIP_3D(U,W,NSID,FIN)

      IF(.NOT.FIN) RETURN
C     ................................ rotate back
      DO I=1,2
C       ..................... rotate into rotated cube system C1 -> CR        
        CALL D_ROT_VECTOR(2,W(1,I),HVD(1,I))
        IF(NSHAD.EQ.1) THEN
          S(1,I)=W(1,I)
          S(2,I)=W(2,I)
          S(3,I)=W(3,I)
          S(M,I)=-SC
          CALL D_ROT_VECTOR(2,S(1,I),HVD(4,I))
        END IF
      END DO
      RETURN
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  D_ROT_AC_IN
CH
      ENTRY D_ROT_AC_IN(ISHAD,SCI,FNO,BETA)
CH
CH --------------------------------------------------------------------
CH
C  initialize artificial cube
C ---------------------------------------------------------------------
C    INPUT:      SC = half side of cube in display coordinates
C
C    Outputs   : FNO = true  rotation cannot be done.
C                Beta = angle between rotation axis and screen
C ---------------------------------------------------------------------
      FNO=.TRUE.
      IF(ISTODS(5,IAREDO,IWUSDO).NE.IPICDO) THEN
        CALL DWRT('No RO picture an the selected window ?"')
        RETURN
      END IF
      SC=SCI
      NSHAD=ISHAD
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PBP'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PBP)
      TPARDA=
     &  'J_RAL,J_RDY,J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(15
     &  ,J_RAL,J_RDY,J_RXX,J_RYY,J_RZZ)
      TPARDA=
     &  'J_RFA,J_RTA,J_RGA,J_RBA'
      CALL DPARAM(86
     &  ,J_RFA,J_RTA,J_RGA,J_RBA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_PBP).NE.0.) THEN
        CALL DWRT('No smooth rotation of "B3" picture."')
        RETURN
      END IF
      IF(PARADA(4,J_RDY).GT.0.) THEN
        CALL DWRT('Define axis: "AX" ... "#')
        RETURN
      END IF
      FNO=.FALSE.
      CALL D_ROT_VECTOR_IN(1,
     &  PARADA(2,J_PFI),
     &  PARADA(2,J_PTE),
     &  PARADA(2,J_RAL))
      IF(PARADA(4,J_RBA).LE.0.) THEN
        FA=PARADA(2,J_RFA)
        TA=PARADA(2,J_RTA)
      ELSE
        FA=A0
        TA=A90
        BETA=PARADA(2,J_RBA)
      END IF
      AA=PARADA(2,J_RGA)
      CALL D_ROT_VECTOR_IN(2,FA,TA,AA)
      CALL DQSET(IAREDO,0.,0.)
      VXYZ(1)=PARADA(2,J_RXX)
      VXYZ(2)=PARADA(2,J_RYY)
      VXYZ(3)=PARADA(2,J_RZZ)
      CALL D_ROT_VECTOR(1,VXYZ,RXYZ)
      CVW=-RXYZ(2)
      CDW=-RXYZ(3)
      IF(IZOMDO.EQ.0) THEN
        CHW=0.
      ELSE
        CHW=CSIG2*0.5*(PARADA(2,J_PTO)+PARADA(2,J_PFR))
      END IF
      END
*DK D_ROT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_ROT
CH
      SUBROUTINE D_ROT(X1,Y1,Z1,X4,Y4,Z4)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C    ROTATE so that X4 = horizntal axis
C    Inputs    : X1, Y1, Z1
C    Outputs   : X4, Y4, Z4
C
C ---------------------------------------------------------------------
C     On VAX 3100 non optimized code needs 11.0 msec's for 1000 calls
C                 for optimized code       10.8 msec's.
      INCLUDE 'DALI_CF.INC'
      X2= CF*X1+SF*Y1
      Y2=-X1*SF+CF*Y1
      Z3=-ST*X2+CT*Z1
      X4= CT*X2+ST*Z1
      Y4= CG*Y2+SG*Z3
      Z4=-SG*Y2+CG*Z3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------  D_ROT_INVERS
CH
      ENTRY D_ROT_INVERS(X4,Y4,Z4,X1,Y1,Z1)
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
      Y3=CG*Y4-SG*Z4
      Z3=SG*Y4+CG*Z4
      X2=CT*X4-ST*Z3
      Z1=ST*X4+CT*Z3
      X1=CF*X2-SF*Y3
      Y1=SF*X2+CF*Y3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  D_ROT_IN
CH
      ENTRY D_ROT_IN(FI,TE,GA)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C!:               initialise rotation of FI TE axis into X' axis
C    Inputs    : if IVEC = 0 matrix for vector rotations is calculated
C    Inputs    : FI, TE, GA = direction of rotation axis
C    Outputs   : no
C
C ---------------------------------------------------------------------
C         .................................................... X2= CF*X1+SF*Y1
C         .................................................... Y2=-X1*SF+CF*Y1
C         .................................................... Z2= Z1
C         .................................................... X3= CT*X2+ST*Z2
C         .................................................... Y3= Y2
C         .................................................... Z3=-ST*X2+CT*Z2
C         ...................................................H=X4= X3
C         ...................................................V=Y4= CG*Y3+SG*Z3
C         .................................................... Z4=-SG*Y3+CG*Z3
C
C     ........................... inverse .......... X3=X4
C     .............................................. Y3=CG*Y4-SG*Z4
C     .............................................. Z3=SG*Y4+CG*Z4
C     .............................................. X2=CT*X3-ST*Z3
C     .............................................. Y2=Y3
C     .............................................. Z2=ST*X3+CT*Z3
C     .............................................. X1=CF*X2-SF*Y2
C     .............................................. Y1=SF*X2+CF*Y2
C     .............................................. Z1=Z2
C ---------------------------------------------------------------------
C
      SF=SIND(FI)
      CF=COSD(FI)
      ST=SIND(90.-TE)
      CT=COSD(90.-TE)
      SG=SIND(GA)
      CG=COSD(GA)
      END
*DK D_ROT_VECTOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_ROT_VECTOR
CH
      SUBROUTINE D_ROT_VECTOR(NS,U,W)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C    Inputs    : U
C    Outputs   : W
C
C ---------------------------------------------------------------------
C     On VAX 3100 non optimized code needs 20.5 msec's for 1000 calls
C                 for optimized code       14.6 msec's for the DO loop.
C      DO N=1,3
C        W(N)=RCT(1,N)*U(1)
C        DO I=2,3
C          W(N)=W(N)+RCT(I,N)*U(I)
C        END DO
C      END DO
C     On VAX 3100 non optimized code needs 10.0 msec's for 1000 calls
C                 for optimized code        9.7 msec's for the code below.
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION U(3),W(3)
      DIMENSION RCT(3,3,4)
      W(1)=RCT(1,1,NS)*U(1)+RCT(2,1,NS)*U(2)+RCT(3,1,NS)*U(3)
      W(2)=RCT(1,2,NS)*U(1)+RCT(2,2,NS)*U(2)+RCT(3,2,NS)*U(3)
      W(3)=RCT(1,3,NS)*U(1)+RCT(2,3,NS)*U(2)+RCT(3,3,NS)*U(3)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ D_ROT_VECTOR_INVERS
CH
      ENTRY D_ROT_VECTOR_INVERS(NS,W,U)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Inputs    : W
C    Outputs   : U
C
C    Called by :
C ---------------------------------------------------------------------
      U(1)=RCT(1,1,NS)*W(1)+RCT(1,2,NS)*W(2)+RCT(1,3,NS)*W(3)
      U(2)=RCT(2,1,NS)*W(1)+RCT(2,2,NS)*W(2)+RCT(2,3,NS)*W(3)
      U(3)=RCT(3,1,NS)*W(1)+RCT(3,2,NS)*W(2)+RCT(3,3,NS)*W(3)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  D_ROT_VECTOR_IN
CH
      ENTRY D_ROT_VECTOR_IN(NS,FI,TE,GA)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C!:               initialise rotation of FI TE axis into X' axis
C    Inputs    : FI, TE, GA = direction of rotation axis
C              : several ( <5 ) rotation matrices (NS) may be stored 
C    Outputs   : no
C
C ---------------------------------------------------------------------
C
C       | X4 |   |  1   0   0 |   | CT   0  ST |   | CF  SF   0 |   | X1 | 
C       | Y4 | = |  0  CG  SG | * |  0   1   0 | * |-SF  CF   0 | * | Y1 | 
C       | Z4 |   |  0 -SG  CG |   |-ST   0  CT |   |  0   0   1 |   | Z1 | 
C
C
C     |    |   |------------------------------------------------|   |    |
C     | X4 |   |  CF*CT           |   SF*CT           |  ST     |   | X1 |      
C     |    |   |-------11---------|-------21----------|---31----|   |    |
C     | Y4 | = | -CF*ST*SG-SF*CG  |  -SF*ST*SG+CF*CG  |  CT*SG  | * | Y1 |
C     |    |   |-------12---------|-------22----------|---32----|   |    |  
C     | Z4 |   | -CF*ST*CG+SF*SG  |  -SF*ST*CG-SF*SG  |  CT*CG  |   | Z1 |
C              |-------13-----------------23--------------33----|   |    |
C
C    Inverse matrix = transposed matrix   R(i,j) -> R(j,i)
C
C    Inverse
C       | X1 |   | CF -SF   0 |   | CT   0 -ST |   |  1   0   0 |   | X4 | 
C       | Y1 | = | SF  CF   0 | * |  0   1   0 | * |  0  CG -SG | * | Y4 | 
C       | Z1 |   |  0   0   1 |   | ST   0  CT |   |  0  SG  CG |   | Z4 | 
C
      SF=SIND(FI)
      CF=COSD(FI)
      ST=SIND(90.-TE)
      CT=COSD(90.-TE)
      SG=SIND(GA)
      CG=COSD(GA)
      RCT(1,1,NS) =  CT*CF
      RCT(2,1,NS) =  SF*CT
      RCT(3,1,NS) =  ST
      RCT(1,2,NS) = -ST*SG*CF - SF*CG
      RCT(2,2,NS) = -ST*SG*SF + CF*CG
      RCT(3,2,NS) =  CT*SG
      RCT(1,3,NS) = -CF*ST*CG + SG*SF
      RCT(2,3,NS) = -ST*CG*SF - SG*CF
      RCT(3,3,NS) =  CT*CG
      END
*DK DQ_CLIP_3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQ_CLIP_3D
CH
      SUBROUTINE DQ_CLIP_3D(U,W,NSID,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann      28-MAR-1988
C    
C    Clip in 3D line in the cube of side SC around 0,0,0.
C
C     Input: line U(2,3) 
C     Output: line W(2,3)    FIN=.TRUE. if inside box
C
C ---------------------------------------------------------------------
C
C
C     U(3,1)-U(3,2)     P-U(3,2) 
C     -------------  =  ---------  
C     U(1,1)-U(1,2)     S-U(1,2)
C
C           DU=U(1,1)-U(1,2)
C           P=U(3,2)+(S-U(1,2)*(U(3,1)-U(3,2))/DU
C
      DIMENSION U(3,2),W(3,2),NSID(2)
      DIMENSION M(2,3)
      DATA M/2,3, 1,3, 1,2/,DFL/1./
C     ....................... DFL correct floating point errors
      LOGICAL FIN
      FIN=.TRUE.
      J=0
C     ...................................... store track point(s) inside cube
      DO I=1,2
        IF(ABS(U(1,I)).LT.SC.AND.
     &     ABS(U(2,I)).LT.SC.AND.
     &     ABS(U(3,I)).LT.SC) THEN
          J=J+1
          CALL UCOPY(U(1,I),W(1,J),3)
          NSID(J)=0
          IF(J.EQ.2) RETURN
        END IF
      END DO
C     ........................................... clip track at cube faces
      DO N=1,3                              
C       ....................................... 3 directions          x, y, z
        D=U(N,1)-U(N,2)
        IF(D.NE.0.) THEN
          DO S=-SC,SC,2.*SC                  
C           .................................. left and right face
            IF((U(N,1).LE.S.AND.S.LE.U(N,2)).OR.
     &         (U(N,2).LE.S.AND.S.LE.U(N,1))) THEN
C             ............ new position at S must ly between 2 track points
              L1=M(1,N)
C             .................................. first variable       y  x  x
              P1=U(L1,2)+(S-U(N,2))*(U(L1,1)-U(L1,2))/D
              IF(ABS(P1).LT.SC) THEN
                L2=M(2,N)
C               ............................... second variable       z  z  y
                P2=U(L2,2)+(S-U(N,2))*(U(L2,1)-U(L2,2))/D
                IF(ABS(P2).LT.SC) THEN
                  J=J+1
                  W( N,J)=S
                  W(L1,J)=P1
                  W(L2,J)=P2
                  NSID(J)=N
                  IF(J.EQ.2) THEN
                    D12=(W(1,1)-W(1,2))**2
     &                 +(W(2,1)-W(2,2))**2
     &                 +(W(3,1)-W(3,2))**2
                    IF(D12.GT.DFL) THEN
                      RETURN
                    ELSE
C                     .... exclude second point on same edge = same position
                      J=1
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END DO
        END IF
      END DO
      FIN=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------- DQCLP_3D
CH
      ENTRY DQ_CLIP_3D_IN(SID)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   27-MAR-1995
C
C!:call before DQ_CLIP_3D
C
C    Inputs    :SC = half side length of artificial cube.
C    Outputs   :no
C
C ---------------------------------------------------------------------
      SC=SID
      CALL DQCL0(-SC,-SC,SC,SC,0.)
      END
*DK DQ_CLIP_3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQ_CLIP_3D
CH
      SUBROUTINE DQ_CLIP_3DD(U,W,NSID,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann      28-MAR-1988
C    
C    Clip in 3D line in the cube of side SC around 0,0,0.
C
C     Input: line U(2,3) 
C     Output: line W(2,3)    FIN=.TRUE. if inside box
C
C ---------------------------------------------------------------------
      LOGICAL FIN
      DIMENSION U(3,2),W(3,2),NSID(2)
      J=0
C
C
C     U(3,1)-U(3,2)     C1-U(3,2) 
C     -------------  =  ---------  
C     U(1,1)-U(1,2)     A1-U(1,2)
C
C     U(3,1)-U(3,2)     C2-U(3,2) 
C     -------------  =  ---------  
C     U(1,1)-U(1,2)     A2-U(1,2)
C
      A1=U(1,1)
      A2=U(1,2)
      B1=U(2,1)
      B2=U(2,2)
      CALL DQCLP(A1,B1,A2,B2,FIN)
      IF(.NOT.FIN) RETURN
      DU=U(1,1)-U(1,2)
 1000 FORMAT(1X,A,6F12.1)
      IF(DU.NE.0.) THEN
        C1=U(3,2)+(A1-U(1,2))*(U(3,1)-U(3,2))/DU
        CALL DQ_CLIP_3D_STORE(J,A1,B1,C1,W,NSID)
        C2=U(3,2)+(A2-U(1,2))*(U(3,1)-U(3,2))/DU
        CALL DQ_CLIP_3D_STORE(J,A2,B2,C2,W,NSID)
      END IF
      IF(J.GT.1) RETURN
C
      B1=U(2,1)
      B2=U(2,2)
      C1=U(3,1)
      C2=U(3,2)
      CALL DQCLP(B1,C1,B2,C2,FIN)
      IF(.NOT.FIN) RETURN
      DU=U(2,1)-U(2,2)
      IF(DU.NE.0.) THEN
        IF(J.LE.1) THEN
          A1=U(1,2)+(B1-U(2,2))*(U(1,1)-U(1,2))/DU
          CALL DQ_CLIP_3D_STORE(J,A1,B1,C1,W,NSID)
          IF(J.LE.1) THEN
            A2=U(1,2)+(B2-U(2,2))*(U(1,1)-U(1,2))/DU
            CALL DQ_CLIP_3D_STORE(J,A2,B2,C2,W,NSID)
          END IF
        END IF
      END IF
      IF(J.GT.1) RETURN
C
      C1=U(3,1)
      C2=U(3,2)
      A1=U(1,1)
      A2=U(1,2)
      CALL DQCLP(C1,A1,C2,A2,FIN)
      IF(.NOT.FIN) RETURN
      DU=U(3,1)-U(3,2)
      IF(DU.NE.0.) THEN
        IF(J.LE.1) THEN
          B1=U(2,2)+(C1-U(3,2))*(U(2,1)-U(2,2))/DU
          CALL DQ_CLIP_3D_STORE(J,A1,B1,C1,W,NSID)
          IF(J.LE.1) THEN
            B2=U(2,2)+(C2-U(3,2))*(U(2,1)-U(2,2))/DU
            CALL DQ_CLIP_3D_STORE(J,A2,B2,C2,W,NSID)
          END IF  
        END IF
      END IF
      END
*DK DQ_CLIP_3D_STORE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++ DQ_CLIP_3D_STORE
CH
      SUBROUTINE DQ_CLIP_3D_STORE(J,A,B,C,W,NSID)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann      28-MAR-1988
C    
C   store point if inside 3D cube and not yet stored into W(J,*)
C   J=0 OR J=1
C ---------------------------------------------------------------------
      DIMENSION W(3,2),NSID(2),ABC(3)
      DATA S/0./
      ABC(1)=ABS(A)
      ABC(2)=ABS(B)
      ABC(3)=ABS(C)
      IF(ABC(1).LE.S.AND.ABC(2).LE.S.AND.ABC(3).LE.S) THEN
        IF(J.EQ.1.AND.
     &    W(1,1).EQ.A.AND.W(2,1).EQ.B.AND.W(3,1).EQ.C) RETURN
        J=J+1
        W(1,J)=A
        W(2,J)=B
        W(3,J)=C
        DO I=1,3
          IF(ABC(I).EQ.S) THEN
            NSID(J)=I
            RETURN
          END IF
        END DO
      END IF
      NSID(J)=0
      END
