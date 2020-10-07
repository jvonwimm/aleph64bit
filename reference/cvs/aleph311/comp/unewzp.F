      SUBROUTINE UNEWZP (VV0,IOPT,VV0P)
C ---------------------------------------------------------------
C! COMPUTES THE VALUE OF  THE HELIX PARAMETRES,
C! FOR POINTS LOCATED AFTER THE ITC-TPC WALL
C!    AUTHOR: LL. GARRIDO
C!  INPUT :   VV0 = 1/R*CHARGE   [1/CM]  NEG. IF CLOCKWISE
C!                  TAN(LAMBDA)  -=DZ/DS}TAN(ANGLE TO X,Y PLANE)
C!                  PHI0         -0,2PI} ANGLE TO X-AXIS
C!                  D0*SIGN      [CM]    POS IF AT THIS POINT
C!                               THE ANGULAR MOMEMTUM AROUND
C!                               THE ORIGIN IS POS.
C!                                       POS. IF AXIS ENCIRCLED
C!                  Z0           [CM]    Z POS AT R=D0
C!                  ALFA         [RAD]   SCATTERING ANGLE IN X-Y
C!                  ALFAZ        [RAD]   SCATTERING ANGLE IN R-Z
C!                  (IF ALFA OR ALFAZ NOT KNOW SET IT TO 0.)
C!           IOPT: 0,2,4,5 OPTION USED WHEN THE TRACK WAS FITTED
C!                  (FOUND THIS FALUE IN FRFT BANK)
C!  INPUT :  VV0P  = LIKE VV0
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER (RMS=29.6)
      REAL VV0(*),VV0P(*)
      RR0 = VV0(1)
      DZDS = VV0(2)
      PH0 = VV0(3)
      DD0 = VV0(4)
      ZZ0 = VV0(5)
      ALFA = VV0(6)
      ALFAZ = VV0(7)
      CALL UCOPY (VV0,VV0P,7)
      IF (IOPT.LT.4) RETURN
      SST=1.
      IF (RR0.LT.0.) SST=-1.
      EMS = -.5*RR0
     1    *SQRT(ABS( (RMS*RMS-DD0*DD0)/(1.0-RR0*DD0)))
      IF (EMS.GT.1.) THEN
        EMS = 1.
      ELSEIF (EMS.LT.-1) THEN
        EMS = -1.
      ENDIF
      SMS= -2.0*ASIN(EMS)/RR0
      TMS=-SMS*RR0
      SENMS=SIN(TMS)
      COSMS=COS(TMS)
      X2= SENMS*ALFA/RR0
      X1=-COSMS*ALFA/(1.-DD0*RR0)
      VV0P(4)   = DD0+X2
      VV0P(3)   = PH0+X1
      IF(IOPT.LT.5) RETURN
      SECA2=1.+DZDS**2
      XT=ALFAZ*SECA2
      XZ=-ALFAZ*SECA2*SMS
      VV0P(2) = DZDS + XT
      VV0P(5) = ZZ0  + XZ
      END
#endif
