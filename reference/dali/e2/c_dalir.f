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
      IF(FGRYDR) THEN
   10   CALL DPARGV(31,'STC',4,STC4)
        IF(STC4.GE.0.) THEN
          PDCODD(2,LCVDDD)=PARADR(2,2)
          PDCODD(2,LCITDD)=PARADR(2,2)
          PDCODD(2,LCNCDD)=PARADR(2,2)
          PDCODD(2,LCTCDD)=PARADR(2,2)
          CALL DSC0
        END IF
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
*DK DRORAS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRORAS
CH
      SUBROUTINE DRORAS
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
C     Rubberband: from Box to Cone
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF,J_PDT,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PDF,J_PDT,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      DD=MAX(PARADA(2,J_PDF),PARADA(2,J_PDT))
      IF(DD.GT.0.)
     &  PARADA(2,J_PAS)=Q/TAND(DD)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DRORBC
CH
      ENTRY DRORBC(HRB,VRB)
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
      DATA TC/' ==> 11 FI=180.1  TE=180.1  P=40.12   E=40.12'/
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
          WRITE(TC(31:35),1001) PP
          WRITE(TC(41:45),1001) E
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
*DK DRZRBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRAS
CH
      SUBROUTINE DRZRAS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)
      NGO=1
      GO TO 1
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRDT
CH
      ENTRY DRZRDT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      NGO=2
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    1 TPARDA=
     &  'J_PTE,J_PDT,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PTE,J_PDT,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TE=PARADA(2,J_PTE)
      IF(TE.GT.90.) TE=180.-TE
      R1=PARADA(2,J_PFR)
      R2=PARADA(2,J_PTO)
      IF(TE.LT.40.) THEN
        QQ=1./COS(TE)
      ELSE
        QQ=1./SIN(TE)
      END IF
      R1=R1*QQ
      R2=R2*QQ
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      IF(NGO.EQ.1) THEN
        IF(PARADA(2,J_PDT).GT.0.)
     &    PARADA(2,J_PAS)=Q/TAND(0.5*PARADA(2,J_PDT))
      ELSE
        IF(PARADA(2,J_PAS).GT.0.)
     &    PARADA(2,J_PDT)=2.*ATAND(Q/PARADA(2,J_PAS))
      END IF
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DRZRBC
CH
      ENTRY DRZRBC(HRB,VRB)
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
C     Rubberband: from Box to Cone
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_PBM).EQ.1.) THEN
         HM=0.5*(HRB(3)+HRB(1))
         VM=0.5*(VRB(3)+VRB(1))
         DH= ABS(HRB(3)-HRB(1))
         DV= ABS(VRB(3)-VRB(1))
         D2=0.5*MAX(DH,DV)
         HRB(1)=HM-D2
         HRB(2)=HM+D2
         HRB(3)=HRB(2)
         HRB(4)=HRB(1)
         VRB(1)=VM-D2
         VRB(2)=VRB(1)
         VRB(3)=VM+D2
         VRB(4)=VRB(3)
      END IF
      TE=DATN2D(VRB(1)+VRB(3),HRB(1)+HRB(3))
      IF(PARADA(2,J_PBM).LT.5.) THEN
         TT=MOD(TE+3600.,360.)
         IF(TT.LT.40..OR.TT.GT.320.) THEN
            FR=HRB(1)
            TO=HRB(2)
         ELSE IF(TT.LE.140.) THEN
            FR=VRB(2)
            TO=VRB(3)
         ELSE IF(TT.LT.220.) THEN
            FR=-HRB(2)
            TO=-HRB(1)
         ELSE
            FR=-VRB(3)
            TO=-VRB(2)
         END IF
         IF(FR.LE.TO) THEN
           PARADA(2,J_PFR)=FR
           PARADA(2,J_PTO)=TO
         ELSE
           PARADA(2,J_PFR)=TO
           PARADA(2,J_PTO)=FR
         END IF
      ELSE
         TT=TE
         IF(TT.GT.180.) TT=360.-TT
         IF(TT.LT.40..OR.TT.GT.140.) THEN
            H12=0.5*(HRB(1)+HRB(2))
            H34=0.5*(HRB(3)+HRB(4))
            H1=ABS(H12)
            H3=ABS(H34)
            IF(H1.LT.H3) THEN
              IF(H34.GT.0.) THEN
                PARADA(2,J_PFR)= H12
              ELSE
                PARADA(2,J_PFR)=-H12
              END IF
              PARADA(2,J_PTO)=H3
            ELSE
              IF(H12.GT.0.) THEN
                PARADA(2,J_PFR)= H34
              ELSE
                PARADA(2,J_PFR)=-H34
              END IF
              PARADA(2,J_PTO)=H1
            END IF
            QQ=1./COSD(TT)
          ELSE
            V12=0.5*(VRB(1)+VRB(2))
            V34=0.5*(VRB(3)+VRB(4))
            V1=ABS(V12)
            V3=ABS(V34)
            IF(V1.LT.V3) THEN
              IF(V34.GT.0.) THEN
                PARADA(2,J_PFR)= V12
              ELSE
                PARADA(2,J_PFR)=-V12
              END IF
              PARADA(2,J_PTO)=V3
            ELSE
              IF(V12.GT.0.) THEN
                PARADA(2,J_PFR)= V34
              ELSE
                PARADA(2,J_PFR)=-V34
              END IF
              PARADA(2,J_PTO)=V1
            END IF
            QQ=1./SIND(TT)
         END IF
         D=(HRB(1)-HRB(2))*SIND(MAX(1.,TT))
         PARADA(2,J_PAS)=ABS((PARADA(2,J_PTO)-PARADA(2,J_PFR))*QQ/D)
      END IF
      IF(TE.LT.180.) THEN
         PARADA(4,J_PFI)= 1.
      ELSE
         PARADA(4,J_PFI)=-1.
         TE=360.-TE
      END IF
      IF(TE.LT.1.) THEN
         TE=1.
      ELSE IF(TE.GT.179.) THEN
         TE=179.
      END IF
      PARADA(2,J_PTE)=TE
      END
