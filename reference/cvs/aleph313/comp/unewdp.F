      SUBROUTINE UNEWDP (VV0,D0P,PHI0P)
C --------------------------------------------------------------
C! COMPUTES THE VALUE OF D0 AND PHI0 OF THE HELIX PARAMETRES,
C! FOR POINTS LOCATED AFTER THE MULTIPLE SCATTERING OCCURS.
C!    AUTHOR: LL. GARRIDO
C!  INPUT :   VV0 = 1/R*CHARGE   [1/CM]  NEG. IF CLOCKWISE
C!                  TAN(LAMBDA)  -=DZ/DS}TAN(ANGLE TO X,Y PLANE)
C!                  PHI0         -0,2PI} ANGLE TO X-AXIS
C!                  D0*SIGN      [CM]    POS IF AT THIS POINT
C!                               THE ANGULAR MOMEMTUM AROUND
C!                               THE ORIGIN IS POS.
C!                                       POS. IF AXIS ENCIRCLED
C!                  Z0           [CM]    Z POS AT R=D0
C!                  ALFA         [RAD]   SCATTERING ANGLE
C! OUTPUT :   D0P = VALUE OF D0 AFTER THE MUTIPLE SCATTERING
C!        : PHI0P = VALUE OF PHI0 AFTER THE MUTIPLE SCATTERING
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER (RMS=29.6)
      REAL VV0(*)
      RR0 = VV0(1)
      DZDS = VV0(2)
      PH0 = VV0(3)
      DD0 = VV0(4)
      ZZ0 = VV0(5)
      ALFA = VV0(6)
      D0P = DD0
      PHI0P = PH0
      IF (ALFA.EQ.0.) RETURN
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
C      ZMS= ZZ0+DZDS*SMS
C      TMS=-(ZMS-ZZ0)/DZDS*RR0
      TMS=-SMS*RR0
      SENMS=SIN(TMS)
      COSMS=COS(TMS)
      X2= SENMS*ALFA/RR0
      X1=-COSMS*ALFA/(1.-DD0*RR0)
      D0P   = DD0+X2
      PHI0P = PH0+X1
      END
#endif
