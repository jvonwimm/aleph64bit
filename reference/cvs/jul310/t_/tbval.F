      FUNCTION TBVAL(IXV)
C
C-----------------------------------------------------------------------
C! Calib. from V ==> Gauss
C!
C!      Author:    J. Richstein     18-06-84
C!      Modified:  R. Johnson       18-12-86
C!
C!      Input:    IXV         Voltage of Hall probe
C!      Output:   TBVAL       Field in Gauss
C!
C!      Called by TMAGNT
C!---------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION C(14)
C
C     Read in the coefficients according to lehm's data
C     [WV.LEH.COEF(MONAL)] FROM IBM
C
      X1    =-0.2016500E+05
      X2    = 0.2024800E+05
C
      C(1)  = 0.322525E+02
      C(2)  = 0.169872E+05
      C(3)  =-0.180378E+02
      C(4)  = 0.471279E+01
      C(5)  = 0.264518E+01
      C(6)  = 0.128000E+02
      C(7)  =-0.771991E+00
      C(8)  =-0.707983E+01
      C(9)  = 0.0
      C(10) = 0.332935E+01
      C(11) = 0.0
      C(12) =-0.170407E+01
      C(13) = 0.0
      C(14) = 0.931977E+00
C
C     Do the conversion now :
C
      X = IXV
      X=(X+X-X1-X2)/(X2-X1)
      TBVAL = TCHSUM(X,C(1),14)
C
      RETURN
      END
#endif
