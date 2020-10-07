*DK DYXFTO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXFTO
CH
      SUBROUTINE DYXFTO(RFR,RTO)
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
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      N=PARADA(2,J_PFR)-1000.
      IF(N.GE.0) THEN
         RFR=ROTODK(N)
      ELSE
         RFR=PARADA(2,J_PFR)
      END IF
      N=PARADA(2,J_PTO)-1000.
      IF(N.GE.0) THEN
         RTO=ROTODK(N)
      ELSE
         RTO=PARADA(2,J_PTO)
      END IF
      END
*DK DYXMO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXMO
CH
      SUBROUTINE DYXMO(NAR,FI,H,V,FIN)
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
      X=R*COSD(FI)
      Y=R*SIND(FI)
      CALL DQPOC(X,Y,HH,VV,FIN)
      IF(FIN) THEN
         H=HH
         V=VV
      END IF
      END
*DK DYXPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXPO
CH
      SUBROUTINE DYXPO(NAR,H,V,FI,FMES)
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
      LOGICAL FMES
      CHARACTER *7 DT7,TX,TY
      CALL DQINV(NAR,H,V,X,Y)
      TX=DT7(X)
      TY=DT7(Y)
      WRITE(TXTADW,1000)TX,TY
 1000 FORMAT(9X,'X=',A,'   Y=',A,' [CM]')
      CALL DWRC
      FI=DATN2D(Y,X)
      FMES=.TRUE.
      RETURN
      END
*DK DYXRAS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXRAS
CH
      SUBROUTINE DYXRAS
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
     &  'J_PDF,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PDF,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      IF(PARADA(2,J_PDF).GT.0.)
     &  PARADA(2,J_PAS)=Q/TAND(0.5*PARADA(2,J_PDF))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXRDF
CH
      ENTRY DYXRDF
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
     &  'J_PDF,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PDF,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      IF(PARADA(2,J_PAS).GT.0.)
     &  PARADA(2,J_PDF)=2.*ATAND(Q/PARADA(2,J_PAS))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXRBC
CH
      ENTRY DYXRBC(HRB,VRB)
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
     &  'J_PFI,J_PFR,J_PTO,J_PAS,J_PBM'
      CALL DPARAM(11
     &  ,J_PFI,J_PFR,J_PTO,J_PAS,J_PBM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_PBM).EQ.1.) THEN
         HM=0.5*(HRB(1)+HRB(3))
         VM=0.5*(VRB(1)+VRB(3))
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
      IF(PARADA(2,J_PBM).LT.5.) THEN
         H13=0.5*(HRB(1)+HRB(3))
         V13=0.5*(VRB(1)+VRB(3))
         R13=SQRT(H13*H13+V13*V13)
         D13=0.5*ABS(HRB(1)-HRB(3))
         PARADA(2,J_PFR)=R13-D13
         PARADA(2,J_PTO)=R13+D13
         PARADA(2,J_PFI)=DATN2D(V13,H13)
      ELSE
         H12=0.5*(HRB(1)+HRB(2))
         H34=0.5*(HRB(3)+HRB(4))
         V12=0.5*(VRB(1)+VRB(2))
         V34=0.5*(VRB(3)+VRB(4))
         R12=SQRT(H12*H12+V12*V12)
         R34=SQRT(H34*H34+V34*V34)
         IF(R12.LT.R34) THEN
            PARADA(2,J_PFR)=R12
            PARADA(2,J_PTO)=R34
            PARADA(2,J_PFI)=DATN2D(V34,H34)
         ELSE IF(R12.GT.R34) THEN
            PARADA(2,J_PFR)=R34
            PARADA(2,J_PTO)=R12
            PARADA(2,J_PFI)=DATN2D(V12,H12)
         ELSE
            RETURN
         END IF
         IF(H12*H34.LT.0.) PARADA(2,J_PFR)=-PARADA(2,J_PFR)
         PARADA(2,J_PAS)=SQRT((HRB(3)-HRB(2))**2+(VRB(3)-VRB(2))**2)/
     &     SQRT((HRB(1)-HRB(2))**2+(VRB(1)-VRB(2))**2)
      END IF
      END
*DK DYXSB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXSB
CH
      SUBROUTINE DYXSB
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      EXTERNAL DYXSB1
      LOGICAL FOUT
      DIMENSION HSC(17),VSC(17),IHSC(17),IVSC(17),NDSEG(2,1)
      DATA LEV/8/,NUMPL/1/,NDSEG/1,17/
      CALL DYXSB0(IHC,IVC,FOUT)
      IF(FOUT) RETURN
      CALL DWRT('Transformation of a square.  Move mouse.')
      CALL DWRT_END(0)
      CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DYXSB1,
     &  .FALSE.,NDSEG,NUMPL)
      CALL DWRT('Fix box ("FX").')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXSBF
CH
      ENTRY DYXSBF
CH
CH --------------------------------------------------------------------
CH
      CALL DGLINF(HSC,VSC,IHSC,IVSC,NDSEG,NUMPL,LEV)
      END
*DK DYXSB1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXSB1
CH
      SUBROUTINE DYXSB1(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION HU(5),VU(5),H(17),V(17),NDSEG(2,1)
      CHARACTER *(*) TKBD
      LOGICAL FOUT,FDUM,FBUT(4)
      DATA NP/4/,QP/0.2/
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      HC=IHC
      VC=IVC
      HCUP=AH*HC+BH
      VCUP=AV*VC+BV
      RCUP=SQRT(HCUP*HCUP+VCUP*VCUP)
      IF(RCUP.LT.R2) THEN
        HCU=HCUP/(EE-E*RCUP)
        VCU=VCUP/(EE-E*RCUP)
        RCU=RCUP/(EE-E*RCUP)
      ELSE
        HCU=R2*HCUP/RCUP
        VCU=R2*VCUP/RCUP
        RCU=R2
      END IF
      IF(.NOT.FBUT(1)) THEN
        D=50.
      ELSE IF(.NOT.FBUT(2)) THEN
        D=.5
      ELSE IF(.NOT.FBUT(3)) THEN
        D=.05
      ELSE
        D=5.
      END IF
      DH=D*HCU/RCU
      DV=D*VCU/RCU
      HU(1)=HCU-DH+DV
      HU(2)=HCU+DH+DV
      HU(3)=HCU+DH-DV
      HU(4)=HCU-DH-DV
      HU(5)=HU(1)
      VU(1)=VCU-DV-DH
      VU(2)=VCU+DV-DH
      VU(3)=VCU+DV+DH
      VU(4)=VCU-DV+DH
      VU(5)=VU(1)
      NN=1
      DO K=1,4
        HH=HU(K)
        VV=VU(K)
        DHH=(HU(K+1)-HU(K))*QP
        DVV=(VU(K+1)-VU(K))*QP
        DO N=1,NP
          R=SQRT(HH*HH+VV*VV)
          HN=HH*EE/(1.+E*R)
          VN=VV*EE/(1.+E*R)
          CALL DQPOC(HN,VN,H(NN),V(NN),FDUM)
          HH=HH+DHH
          VV=VV+DVV
          NN=NN+1
        END DO
      END DO
      H(NN)=H(1)
      V(NN)=V(1)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXSB0
CH
      ENTRY DYXSB0(IHC,IVC,FOUT)
CH
CH --------------------------------------------------------------------
CH
      IF(      ISTODS(4,IAREDO,IWUSDO).NE.1.OR.
     &  TPICDP(ISTODS(5,IAREDO,IWUSDO)).NE.'YX') GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO,J_PDS'
      CALL DPARAM(11
     &  ,J_PTO,J_PDS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      R2 = PSTODS(1,J_PTO,IAREDO,IWUSDO)
      IF(PSTODS(2,J_PDS,IAREDO,IWUSDO).NE.1..OR
     &  .PSTODS(1,J_PDS,IAREDO,IWUSDO).LE.0.) THEN
        EE=1.
        E =0.
      ELSE
        R1 = 0.5*R2
        RM = R2 - R1/PSTODS(1,J_PDS,IAREDO,IWUSDO)
        E  = (RM - R1)/(R1*(R2-RM))
        EE = 1.+E*R2
        CALL DQSET(IAREDO,0.,0.)
      END IF
      AH=2.*R2/(HHGHDG(0)-HMINDG(0))
      AV=2.*R2/(VHGHDG(0)-VMINDG(0))
      BH=R2-AH*HHGHDG(0)
      BV=R2-AV*VHGHDG(0)
      IHC=0.6*(HHGHDG(0)+HMINDG(0))
      IVC=0.6*(VHGHDG(0)+VMINDG(0))
      FOUT=.FALSE.
      RETURN
   99 CALL DWRT(' Wrong window selected.')
      FOUT=.TRUE.
      END
*DK DYXSC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXSC
CH
      SUBROUTINE DYXSC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      EXTERNAL DYXSC1
      LOGICAL FOUT
      DIMENSION HSC(50),VSC(50),IHSC(50),IVSC(50),NDSEG(2,25)
      DATA LEV/8/
      CALL DYXSC0(NDSEG,NUMPL,FOUT)
      IF(FOUT) RETURN
      CALL DWRT('Display a scale (1,10,100..)  Move mouse.')
      CALL DWRT_END(0)
      CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DYXSC1,
     &  .FALSE.,NDSEG,NUMPL)
      CALL DWRT('Fix scale ("FS").')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXSCF
CH
      ENTRY DYXSCF
CH
CH --------------------------------------------------------------------
CH
      CALL DGLINF(HSC,VSC,IHSC,IVSC,NDSEG,NUMPL,LEV)
      END
*DK DYXSC1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYXSC1
CH
      SUBROUTINE DYXSC1(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TKBD
      LOGICAL FOUT,FDUM,FBUT(4),FPERS
      DIMENSION RR(24)
      DIMENSION H(50),V(50),NDSEG(2,25)
      DATA DD/8./
      DIMENSION RS(3),RE(3),STEP(3)
      DATA  RS /1.,10.,100./
      DATA  RE /9.,90.,600./
      DATA STEP/1.,10.,100./
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      HC=IHC-HC0
      VC=IVC-VC0
      RC=SQRT(VC*VC+HC*HC)
      QR=1./RC
      QH=HC*QR
      QV=VC*QR
      QR=1./RC
      DH=DD*QV
      DV=DD*QH
      H(1)=H0+QH*RR(1)
      V(1)=V0+QV*RR(1)
      NN=2
      DO N=1,NP
        HH=H0+QH*RR(N)
        VV=V0+QV*RR(N)
        NN=NN+1
        H(NN)=HH
        V(NN)=VV
        NN=NN+1
        H(NN)=HH-DH
        V(NN)=VV+DV
      END DO
      H(2)=HH
      V(2)=VV
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYXSC0
CH
      ENTRY DYXSC0(NDSEG,NUMPL,FOUT)
CH
CH --------------------------------------------------------------------
CH
      IF(      ISTODS(4,IAREDO,IWUSDO).NE.1.OR.
     &  TPICDP(ISTODS(5,IAREDO,IWUSDO)).NE.'YX') GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO,J_PDS,J_PBM'
      CALL DPARAM(11
     &  ,J_PFR,J_PTO,J_PDS,J_PBM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        RFR=0.
      ELSE
        RFR=MAX(0.,PSTODS(1,J_PFR,IAREDO,IWUSDO))
        IF(PSTODS(1,J_PBM,IAREDO,IWUSDO).GE.5.)
     &    CALL DWRT(' Use scale only horizontaly!')
      END IF
      RTO=PSTODS(1,J_PTO,IAREDO,IWUSDO)
      IF(PSTODS(2,J_PDS,IAREDO,IWUSDO).NE.1..OR
     &  .PSTODS(1,J_PDS,IAREDO,IWUSDO).LE.0.) THEN
        FPERS=.FALSE.
      ELSE
        FPERS=.TRUE.
        R2 = RTO
        R1 = 0.5*R2
        RM = R2 - R1/PSTODS(1,J_PDS,IAREDO,IWUSDO)
        E  = (RM - R1)/(R1*(R2-RM))
        EE = 1.+E*R2
      END IF
      CALL DQSET(IAREDO,0.,0.)
      CALL DQPOC(0.,0.,H0,V0,FDUM)
      NP=0
      DO I=1,3
        DO R=RS(I),RE(I),STEP(I)
          IF(R.GT.RTO) GO TO 5
          IF(R.GT.RFR) THEN
            IF(FPERS) THEN
              RU=R*EE/(1.+E*R)
            ELSE
              RU=R
            END IF
            NP=NP+1
            CALL DQPOC(RU,0.,RD,VDUM,FDUM)
            RR(NP)=RD-H0
          END IF
        END DO
      END DO
    5 HC0=0.5*(HMINDG(0)+VHGHDG(0))
      VC0=0.5*(VMINDG(0)+VHGHDG(0))
      IF(NP.EQ.0) THEN
        CALL DWRT(' Too large magnification.')
        FOUT=.TRUE.
      ELSE
        FOUT=.FALSE.
        NUMPL=NP+1
        NN=1
        DO K=1,NUMPL
          NDSEG(1,K)=NN
          NDSEG(2,K)=2
          NN=NN+2
        END DO
      END IF
      RETURN
   99 CALL DWRT(' Wrong window selected.')
      FOUT=.TRUE.
      END
*DK DYZNUD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYZNUD
CH
      SUBROUTINE DYZNUD(TMOD)
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
C
C TO COUNT DIGITAL CLUSTER IN HCAL
C NBA  = NUMBER OF CLUSTER IN BARREL
C N0   = NUMBER OF CLUSTER IN ENDCAPS, TUBES AT PHI=0
C N60  = NUMBER OF CLUSTER IN ENDCAPS, TUBES AT PHI=60
C N300 = NUMBER OF CLUSTER IN ENDCAPS, TUBES AT PHI=300
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TMOD
      CHARACTER *3 T(3),DT3
      DATA T/3*'---'/
      DIMENSION NU(3)
      EQUIVALENCE (NU(1),N0),(NU(2),N60),(NU(3),N300)
      CALL=DVHT0(NUM)
      IF(NUM.LE.0) GO TO 3
      CALL DDRFLG
      CALL VZERO(NU,3)
C
C LOOP ON DIGITAL CLUSTERS
C
      DO 1 K=1,NUM
         ISUBC=DVHT(13,K)
         IF(ISUBC.NE.2)THEN
            IMN=DVHT(12,K)
            IF(IMN.NE.2.AND.IMN.NE.5)THEN
               N0=N0+1
            ELSE
               IF(ISUBC.EQ.1)N60=N60+1
               IF(ISUBC.EQ.3)N300=N300+1
            END IF
         END IF
    1 CONTINUE
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFY'
      CALL DPARAM(11
     &  ,J_PFY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TMOD.EQ.'CALC') THEN
        IF(NU(1).GE.NU(2).AND.NU(1).GE.NU(3)) PARADA(2,J_PFY)=  0.
        IF(NU(2).GE.NU(1).AND.NU(2).GE.NU(3)) PARADA(2,J_PFY)= 60.
        IF(NU(3).GE.NU(1).AND.NU(3).GE.NU(2)) PARADA(2,J_PFY)=120.
      ELSE
        IF(FNOPDR) RETURN
        IF(.NOT.FHCADR) RETURN
        DO 2 I=1,3
          T(I)=DT3(FLOAT(NU(I)))
    2   CONTINUE
    3   WRITE(TXTADW,1000) T
 1000  FORMAT('hcal: # of dig.hits',
     &    ' F1=0:',  A ,
     &    ' F2=60:', A ,
     &    ' F3=120:',A )
       CALL DWRC

C        WRITE(MUN9DU,1000) T
C#ifndef ultrix
C 1000  FORMAT(' hcal: # of dig.hits',
C#else
C 1000  FORMAT(/,' hcal: # of dig.hits',
C#endif /* Ultrix */
C     &    ' F1=0:',  A ,
C     &    ' F2=60:', A ,
C     &    ' F3=120:',A ,'..:',$)

      END IF
      END
*DK DYZRAS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DYZRAS
CH
      SUBROUTINE DYZRAS
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
     &  'J_PDF,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PDF,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      IF(PARADA(2,J_PDF).GT.0.)
     &  PARADA(2,J_PAS)=Q/TAND(0.5*PARADA(2,J_PDF))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYZRDF
CH
      ENTRY DYZRDF
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
     &  'J_PDF,J_PFR,J_PTO,J_PAS'
      CALL DPARAM(11
     &  ,J_PDF,J_PFR,J_PTO,J_PAS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      Q=0.5*(1.-PARADA(2,J_PFR)/PARADA(2,J_PTO))
      IF(PARADA(2,J_PAS).GT.0.)
     &  PARADA(2,J_PDF)=2.*ATAND(Q/PARADA(2,J_PAS))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DYZRBC
CH
      ENTRY DYZRBC(HRB,VRB)
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
      H1=ABS(HRB(1))
      H3=ABS(HRB(3))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PFR,J_PTO,J_PAS,J_RDY'
      CALL DPARAM(10
     &  ,J_PTE,J_PFR,J_PTO,J_PAS,J_RDY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(H1.LT.H3) THEN
        PARADA(2,J_PTO)=H3
        IF(HRB(3).GT.0.) THEN
          PARADA(2,J_PFR)=HRB(1)
          IF(PARADA(2,J_PTE).GT.90.)
     &      PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
        ELSE
          PARADA(2,J_PFR)=-HRB(1)
          IF(PARADA(2,J_PTE).LT.90.)
     &      PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
        END IF
      ELSE
        PARADA(2,J_PTO)=H1
        IF(HRB(1).GT.0.) THEN
          PARADA(2,J_PFR)=HRB(3)
          IF(PARADA(2,J_PTE).GT.90.)
     &      PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
        ELSE
          PARADA(2,J_PFR)=-HRB(3)
          IF(PARADA(2,J_PTE).LT.90.)
     &      PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
        END IF
      END IF
      DV=VRB(3)-VRB(1)
      IF(DV.NE.0.) PARADA(2,J_PAS)=ABS((HRB(3)-HRB(1))/DV)
      PARADA(2,J_RDY)=0.5*(VRB(1)+VRB(3))
      END
