*DK DYX_DF_TO_AS
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DYX_DF_TO_AS
CH
      SUBROUTINE DYX_DF_TO_AS
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4),HR0(4),VR0(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PFR,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PFR,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0)
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
CH -------------------------------------------------------------- DYX_AS_TO_DF
CH
      ENTRY DYX_AS_TO_DF
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
CH -------------------------------------------------------  DYX_HV_TO_CONE
CH
      ENTRY DYX_HV_TO_CONE(HRB,VRB)
CH --------------------------------------------------------------------
CH
      CALL DQZ_OFFSET(-1.,J_PX0,J_PY0,0.,HRB,VRB,HR0,VR0)
      IF(PARADA(2,J_PBX).EQ.2.) THEN
         H13=0.5*(HR0(1)+HR0(3))
         V13=0.5*(VR0(1)+VR0(3))
         R13=SQRT(H13*H13+V13*V13)
         D13=0.5*ABS(HR0(1)-HR0(3))
         PARADA(2,J_PFR)=R13-D13
         PARADA(2,J_PTO)=R13+D13
         PARADA(2,J_PFI)=DATN2D(V13,H13)
      ELSE
         H23=0.5*(HR0(2)+HR0(3))
         H41=0.5*(HR0(4)+HR0(1))
         V23=0.5*(VR0(2)+VR0(3))
         V41=0.5*(VR0(4)+VR0(1))
         R23=SQRT(H23**2+V23**2)
         R41=SQRT(H41**2+V41**2)
         IF(     R23.LT.R41) THEN
            PARADA(2,J_PFR)=R23
            PARADA(2,J_PTO)=R41
            PARADA(2,J_PFI)=DATN2D(V41,H41)
         ELSE IF(R23.GT.R41) THEN
            PARADA(2,J_PFR)=R41
            PARADA(2,J_PTO)=R23
            PARADA(2,J_PFI)=DATN2D(V23,H23)
         ELSE
            RETURN
         END IF
         IF(H23*H41.LT.0.) PARADA(2,J_PFR)=-PARADA(2,J_PFR)
         RR32=(HR0(3)-HR0(2))**2+(VR0(3)-VR0(2))**2
         IF(RR32.GT.0.) PARADA(2,J_PAS)=
     &     SQRT((HR0(2)-HR0(1))**2+(VR0(2)-VR0(1))**2)/SQRT(RR32)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DYX_CONE_TO_HV
CH
      ENTRY DYX_CONE_TO_HV(HRB,VRB)
CH --------------------------------------------------------------------
CH
      IF(PARADA(2,J_PBX).EQ.2.) THEN
        R13=0.5*(PARADA(2,J_PTO)+PARADA(2,J_PFR))
        D13=0.5*(PARADA(2,J_PTO)-PARADA(2,J_PFR))
        H13=R13*COSD(PARADA(2,J_PFI))
        V13=R13*SIND(PARADA(2,J_PFI))
        HR0(1)=H13-D13
        HR0(3)=H13+D13
        VR0(1)=V13-D13
        VR0(3)=V13+D13
        CALL DQ24(HR0,VR0)
      ELSE
        R12=PARADA(2,J_PFR)
        R34=PARADA(2,J_PTO)
        CF=COSD(PARADA(2,J_PFI))
        SF=SIND(PARADA(2,J_PFI))
        H12=R12*CF
        V12=R12*SF
        H34=R34*CF
        V34=R34*SF
        D2=0.5*(R34-R12)/PARADA(2,J_PAS)
        DH=SF*D2
        DV=CF*D2
        HR0(1)=H12+DH
        VR0(1)=V12-DV
        HR0(2)=H34+DH
        VR0(2)=V34-DV
        HR0(3)=H34-DH
        VR0(3)=V34+DV
        HR0(4)=H12-DH
        VR0(4)=V12+DV
      END IF
      CALL DQZ_OFFSET(1.,J_PX0,J_PY0,0.,HR0,VR0,HRB,VRB)
      END
